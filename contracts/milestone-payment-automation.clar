;; Milestone Payment Automation System
;; Automates payment releases based on milestone completion and approval workflows

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u700))
(define-constant ERR-MILESTONE-NOT-FOUND (err u701))
(define-constant ERR-PAYMENT-ALREADY-RELEASED (err u702))
(define-constant ERR-INSUFFICIENT-BALANCE (err u703))
(define-constant ERR-INVALID-APPROVAL (err u704))
(define-constant ERR-DEADLINE-NOT-REACHED (err u705))
(define-constant ERR-AUTOMATION-DISABLED (err u706))

;; Constants for automation rules
(define-constant AUTO-RELEASE-DELAY-BLOCKS u1008) ;; ~7 days
(define-constant MAX-APPROVAL-PERIOD u2016) ;; ~14 days
(define-constant PERFORMANCE-BONUS-THRESHOLD u90) ;; 90% score

;; Automated milestone payment configurations
(define-map milestone-automation-config
    {project-id: uint, milestone-id: uint}
    {
        auto-release-enabled: bool,
        approval-required: bool,
        multi-party-approval: bool,
        auto-release-deadline: uint,
        payment-amount: uint,
        bonus-enabled: bool,
        bonus-percentage: uint,
        approved-by-client: bool,
        approved-by-freelancer: bool,
        payment-released: bool,
        created-at: uint
    }
)

;; Payment schedules for recurring milestones
(define-map payment-schedules
    {schedule-id: uint}
    {
        project-id: uint,
        client: principal,
        freelancer: principal,
        total-amount: uint,
        payment-frequency: uint,
        payments-remaining: uint,
        next-payment-due: uint,
        auto-pay-enabled: bool,
        schedule-active: bool
    }
)

;; Performance-based bonus tracking
(define-map performance-bonuses
    {project-id: uint, milestone-id: uint}
    {
        quality-score: uint,
        timeliness-score: uint,
        communication-score: uint,
        overall-score: uint,
        bonus-amount: uint,
        bonus-paid: bool
    }
)

;; Approval workflows for complex payments
(define-map approval-workflows
    {workflow-id: uint}
    {
        project-id: uint,
        milestone-id: uint,
        approvers: (list 5 principal),
        approvals-received: uint,
        approvals-required: uint,
        workflow-status: (string-ascii 20),
        created-at: uint,
        completed-at: uint
    }
)

;; Data variables for counters
(define-data-var schedule-counter uint u0)
(define-data-var workflow-counter uint u0)

;; Configure automated milestone payment
(define-public (configure-milestone-automation 
    (project-id uint) 
    (milestone-id uint) 
    (payment-amount uint)
    (auto-release-enabled bool)
    (approval-required bool)
    (bonus-percentage uint))
    (let ((project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND)))
        
        ;; Only project client can configure automation
        (asserts! (is-eq tx-sender (get client (unwrap-panic project))) ERR-NOT-AUTHORIZED)
        (asserts! (<= bonus-percentage u50) ERR-INVALID-APPROVAL) ;; Max 50% bonus
        
        (ok (map-set milestone-automation-config {project-id: project-id, milestone-id: milestone-id}
            {
                auto-release-enabled: auto-release-enabled,
                approval-required: approval-required,
                multi-party-approval: false,
                auto-release-deadline: (+ stacks-block-height AUTO-RELEASE-DELAY-BLOCKS),
                payment-amount: payment-amount,
                bonus-enabled: (> bonus-percentage u0),
                bonus-percentage: bonus-percentage,
                approved-by-client: false,
                approved-by-freelancer: false,
                payment-released: false,
                created-at: stacks-block-height
            }
        ))
    )
)

;; Approve milestone payment (client or freelancer)
(define-public (approve-milestone-payment (project-id uint) (milestone-id uint))
    (let (
        (config (unwrap! (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (get approval-required config) ERR-INVALID-APPROVAL)
        (asserts! (not (get payment-released config)) ERR-PAYMENT-ALREADY-RELEASED)
        
        ;; Check authorization and update approval status
        (asserts! (or (is-eq tx-sender (get client (unwrap-panic project)))
                     (is-eq tx-sender (get freelancer (unwrap-panic project)))) ERR-NOT-AUTHORIZED)
        
        ;; Update approval status based on who is approving
        (if (is-eq tx-sender (get client (unwrap-panic project)))
            (map-set milestone-automation-config {project-id: project-id, milestone-id: milestone-id}
                (merge config {approved-by-client: true}))
            (map-set milestone-automation-config {project-id: project-id, milestone-id: milestone-id}
                (merge config {approved-by-freelancer: true})))
        
        ;; Check if all required approvals are received
        (let ((updated-config (unwrap-panic (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}))))
            (if (and (get approved-by-client updated-config) 
                     (or (not (get multi-party-approval updated-config)) 
                         (get approved-by-freelancer updated-config)))
                (process-milestone-payment project-id milestone-id)
                (ok true)))
    )
)

;; Process automated milestone payment
(define-public (process-milestone-payment (project-id uint) (milestone-id uint))
    (let (
        (config (unwrap! (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (not (get payment-released config)) ERR-PAYMENT-ALREADY-RELEASED)
        
        ;; Check if auto-release conditions are met
        (if (get auto-release-enabled config)
            (asserts! (>= stacks-block-height (get auto-release-deadline config)) ERR-DEADLINE-NOT-REACHED)
            (asserts! (and (get approved-by-client config) 
                          (or (not (get multi-party-approval config)) (get approved-by-freelancer config))) ERR-INVALID-APPROVAL))
        
        ;; Release base payment
        (try! (stx-transfer? (get payment-amount config) (get client (unwrap-panic project)) (get freelancer (unwrap-panic project))))
        
        ;; Process performance bonus if enabled
        (try! (if (get bonus-enabled config)
            (process-performance-bonus project-id milestone-id)
            (ok true)))
        
        ;; Mark payment as released
        (map-set milestone-automation-config {project-id: project-id, milestone-id: milestone-id}
            (merge config {payment-released: true}))
        
        (ok true)
    )
)

;; Create recurring payment schedule
(define-public (create-payment-schedule 
    (project-id uint) 
    (freelancer principal)
    (total-amount uint) 
    (payment-frequency uint) 
    (payment-count uint))
    (let (
        (schedule-id (var-get schedule-counter))
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get client (unwrap-panic project))) ERR-NOT-AUTHORIZED)
        (asserts! (> payment-count u0) ERR-INVALID-APPROVAL)
        
        (var-set schedule-counter (+ schedule-id u1))
        
        (ok (map-set payment-schedules {schedule-id: schedule-id}
            {
                project-id: project-id,
                client: tx-sender,
                freelancer: freelancer,
                total-amount: total-amount,
                payment-frequency: payment-frequency,
                payments-remaining: payment-count,
                next-payment-due: (+ stacks-block-height payment-frequency),
                auto-pay-enabled: true,
                schedule-active: true
            }
        ))
    )
)

;; Process scheduled payment
(define-public (process-scheduled-payment (schedule-id uint))
    (let (
        (schedule (unwrap! (map-get? payment-schedules {schedule-id: schedule-id}) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (get schedule-active schedule) ERR-AUTOMATION-DISABLED)
        (asserts! (>= stacks-block-height (get next-payment-due schedule)) ERR-DEADLINE-NOT-REACHED)
        (asserts! (> (get payments-remaining schedule) u0) ERR-PAYMENT-ALREADY-RELEASED)
        
        (let ((payment-amount (/ (get total-amount schedule) (get payments-remaining schedule))))
            ;; Transfer payment
            (try! (stx-transfer? payment-amount (get client schedule) (get freelancer schedule)))
            
            ;; Update schedule
            (map-set payment-schedules {schedule-id: schedule-id}
                (merge schedule {
                    payments-remaining: (- (get payments-remaining schedule) u1),
                    next-payment-due: (+ stacks-block-height (get payment-frequency schedule)),
                    schedule-active: (> (get payments-remaining schedule) u1)
                }))
        )
        
        (ok true)
    )
)

;; Submit performance scores for bonus calculation
(define-public (submit-performance-scores 
    (project-id uint) 
    (milestone-id uint)
    (quality uint) 
    (timeliness uint) 
    (communication uint))
    (let (
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
        (overall-score (/ (+ quality timeliness communication) u3))
    )
        (asserts! (is-eq tx-sender (get client (unwrap-panic project))) ERR-NOT-AUTHORIZED)
        (asserts! (and (<= quality u100) (<= timeliness u100) (<= communication u100)) ERR-INVALID-APPROVAL)
        
        (ok (map-set performance-bonuses {project-id: project-id, milestone-id: milestone-id}
            {
                quality-score: quality,
                timeliness-score: timeliness,
                communication-score: communication,
                overall-score: overall-score,
                bonus-amount: u0,
                bonus-paid: false
            }
        ))
    )
)

;; Process performance-based bonus
(define-private (process-performance-bonus (project-id uint) (milestone-id uint))
    (let (
        (config (unwrap! (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
        (performance (unwrap! (map-get? performance-bonuses {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
    )
        (if (>= (get overall-score performance) PERFORMANCE-BONUS-THRESHOLD)
            (let ((bonus-amount (/ (* (get payment-amount config) (get bonus-percentage config)) u100)))
                (try! (stx-transfer? bonus-amount (get client (unwrap-panic project)) (get freelancer (unwrap-panic project))))
                (map-set performance-bonuses {project-id: project-id, milestone-id: milestone-id}
                    (merge performance {bonus-amount: bonus-amount, bonus-paid: true}))
                (ok true))
            (ok true))
    )
)

;; Create multi-party approval workflow
(define-public (create-approval-workflow 
    (project-id uint) 
    (milestone-id uint) 
    (approvers (list 5 principal)) 
    (required-approvals uint))
    (let (
        (workflow-id (var-get workflow-counter))
        (project (unwrap! (contract-call? .freelance-rep get-dispute project-id) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get client (unwrap-panic project))) ERR-NOT-AUTHORIZED)
        (asserts! (<= required-approvals (len approvers)) ERR-INVALID-APPROVAL)
        
        (var-set workflow-counter (+ workflow-id u1))
        
        (ok (map-set approval-workflows {workflow-id: workflow-id}
            {
                project-id: project-id,
                milestone-id: milestone-id,
                approvers: approvers,
                approvals-received: u0,
                approvals-required: required-approvals,
                workflow-status: "pending",
                created-at: stacks-block-height,
                completed-at: u0
            }
        ))
    )
)

;; Read-only functions
(define-read-only (get-milestone-automation-config (project-id uint) (milestone-id uint))
    (ok (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}))
)

(define-read-only (get-payment-schedule (schedule-id uint))
    (ok (map-get? payment-schedules {schedule-id: schedule-id}))
)

(define-read-only (get-performance-bonus (project-id uint) (milestone-id uint))
    (ok (map-get? performance-bonuses {project-id: project-id, milestone-id: milestone-id}))
)

(define-read-only (get-approval-workflow (workflow-id uint))
    (ok (map-get? approval-workflows {workflow-id: workflow-id}))
)

;; Check if milestone is ready for auto-release
(define-read-only (is-ready-for-auto-release (project-id uint) (milestone-id uint))
    (let (
        (config (map-get? milestone-automation-config {project-id: project-id, milestone-id: milestone-id}))
    )
        (if (is-some config)
            (let ((cfg (unwrap-panic config)))
                (ok (and (get auto-release-enabled cfg)
                        (>= stacks-block-height (get auto-release-deadline cfg))
                        (not (get payment-released cfg)))))
            (ok false))
    )
)
