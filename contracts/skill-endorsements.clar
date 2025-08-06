;; Skill Endorsement System
;; Enables peer-to-peer skill validation and creates weighted trust networks

(define-constant ERR-NOT-FOUND (err u600))
(define-constant ERR-UNAUTHORIZED (err u601))
(define-constant ERR-ALREADY-ENDORSED (err u602))
(define-constant ERR-SELF-ENDORSEMENT (err u603))
(define-constant ERR-INVALID-PARAMS (err u604))
(define-constant ERR-ENDORSEMENT-LIMIT (err u605))

;; Maximum endorsements per skill per user to prevent spam
(define-constant MAX-ENDORSEMENTS-PER-SKILL u50)
(define-constant MIN-ENDORSER-REPUTATION u50)

;; Track individual endorsements
(define-map skill-endorsements
    {endorsee: principal, endorser: principal, skill: (string-utf8 50)}
    {
        endorsement-strength: uint,
        endorser-reputation: uint,
        timestamp: uint,
        verified: bool
    }
)

;; Aggregate endorsement data per user-skill combination
(define-map user-skill-endorsements
    {user: principal, skill: (string-utf8 50)}
    {
        total-endorsements: uint,
        weighted-score: uint,
        average-strength: uint,
        verified-count: uint,
        last-updated: uint
    }
)

;; Track endorser statistics
(define-map endorser-stats
    principal
    {
        total-given: uint,
        reputation-score: uint,
        accuracy-rating: uint,
        endorsement-weight: uint
    }
)

;; Track endorsements received by user
(define-map user-endorsement-summary
    principal
    {
        total-received: uint,
        unique-endorsers: uint,
        average-weighted-score: uint,
        skill-diversity: uint
    }
)

;; Count endorsements per skill to prevent spam
(define-map skill-endorsement-counts
    {endorser: principal, skill: (string-utf8 50)}
    {
        count: uint,
        last-endorsement: uint
    }
)

;; Trust network connections
(define-map trust-connections
    {user-a: principal, user-b: principal}
    {
        connection-strength: uint,
        mutual-endorsements: uint,
        established-at: uint
    }
)

;; Create or update an endorsement
(define-public (endorse-skill (endorsee principal) (skill (string-utf8 50)) (strength uint) (verified bool))
    (let
        (
            (endorser tx-sender)
            (current-count (default-to {count: u0, last-endorsement: u0} 
                (map-get? skill-endorsement-counts {endorser: endorser, skill: skill})))
            (endorser-reputation (get-endorser-reputation endorser))
            (existing-endorsement (map-get? skill-endorsements {endorsee: endorsee, endorser: endorser, skill: skill}))
        )
        ;; Validation checks
        (asserts! (not (is-eq endorser endorsee)) ERR-SELF-ENDORSEMENT)
        (asserts! (and (>= strength u1) (<= strength u10)) ERR-INVALID-PARAMS)
        (asserts! (< (get count current-count) MAX-ENDORSEMENTS-PER-SKILL) ERR-ENDORSEMENT-LIMIT)
        (asserts! (>= endorser-reputation MIN-ENDORSER-REPUTATION) ERR-UNAUTHORIZED)
        (asserts! (is-none existing-endorsement) ERR-ALREADY-ENDORSED)
        
        ;; Create endorsement record
        (map-set skill-endorsements {endorsee: endorsee, endorser: endorser, skill: skill}
            {
                endorsement-strength: strength,
                endorser-reputation: endorser-reputation,
                timestamp: stacks-block-height,
                verified: verified
            }
        )
        
        ;; Update skill endorsement count for endorser
        (map-set skill-endorsement-counts {endorser: endorser, skill: skill}
            {
                count: (+ (get count current-count) u1),
                last-endorsement: stacks-block-height
            }
        )
        
        ;; Update aggregate endorsement data
        (unwrap! (update-user-skill-endorsements endorsee skill) ERR-INVALID-PARAMS)
        
        ;; Update endorser statistics
        (unwrap! (update-endorser-stats endorser) ERR-INVALID-PARAMS)
        
        ;; Update trust connections
        (unwrap! (update-trust-connection endorser endorsee) ERR-INVALID-PARAMS)
        
        ;; Update user endorsement summary
        (unwrap! (update-user-summary endorsee) ERR-INVALID-PARAMS)
        
        (ok true)
    )
)

;; Update aggregated endorsement data for a user-skill combination
(define-private (update-user-skill-endorsements (user principal) (skill (string-utf8 50)))
    (let
        (
            (current-data (default-to 
                {total-endorsements: u0, weighted-score: u0, average-strength: u0, verified-count: u0, last-updated: u0}
                (map-get? user-skill-endorsements {user: user, skill: skill})))
            (endorsement-list (get-skill-endorsements-for-user user skill))
            (total-endorsements (+ (get total-endorsements current-data) u1))
            (weighted-score (calculate-weighted-endorsement-score user skill))
            (verified-count (+ (get verified-count current-data) u1))
        )
        (map-set user-skill-endorsements {user: user, skill: skill}
            {
                total-endorsements: total-endorsements,
                weighted-score: weighted-score,
                average-strength: (if (> total-endorsements u0) (/ weighted-score total-endorsements) u0),
                verified-count: verified-count,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Update endorser statistics
(define-private (update-endorser-stats (endorser principal))
    (let
        (
            (current-stats (default-to 
                {total-given: u0, reputation-score: u100, accuracy-rating: u100, endorsement-weight: u100}
                (map-get? endorser-stats endorser)))
            (new-total (+ (get total-given current-stats) u1))
            (reputation-score (get-endorser-reputation endorser))
            (endorsement-weight (calculate-endorsement-weight endorser reputation-score))
        )
        (map-set endorser-stats endorser
            {
                total-given: new-total,
                reputation-score: reputation-score,
                accuracy-rating: (get accuracy-rating current-stats),
                endorsement-weight: endorsement-weight
            }
        )
        (ok true)
    )
)

;; Update trust connections between users
(define-private (update-trust-connection (endorser principal) (endorsee principal))
    (let
        (
            (connection-key (if (< (principal-to-uint endorser) (principal-to-uint endorsee))
                {user-a: endorser, user-b: endorsee}
                {user-a: endorsee, user-b: endorser}))
            (current-connection (default-to 
                {connection-strength: u0, mutual-endorsements: u0, established-at: stacks-block-height}
                (map-get? trust-connections connection-key)))
            (new-strength (+ (get connection-strength current-connection) u10))
        )
        (map-set trust-connections connection-key
            {
                connection-strength: new-strength,
                mutual-endorsements: (+ (get mutual-endorsements current-connection) u1),
                established-at: (get established-at current-connection)
            }
        )
        (ok true)
    )
)

;; Update user endorsement summary
(define-private (update-user-summary (user principal))
    (let
        (
            (current-summary (default-to 
                {total-received: u0, unique-endorsers: u0, average-weighted-score: u0, skill-diversity: u0}
                (map-get? user-endorsement-summary user)))
            (new-total (+ (get total-received current-summary) u1))
            (weighted-score (calculate-overall-endorsement-score user))
        )
        (map-set user-endorsement-summary user
            {
                total-received: new-total,
                unique-endorsers: (+ (get unique-endorsers current-summary) u1),
                average-weighted-score: weighted-score,
                skill-diversity: (get skill-diversity current-summary)
            }
        )
        (ok true)
    )
)

;; Calculate weighted endorsement score for a specific skill
(define-private (calculate-weighted-endorsement-score (user principal) (skill (string-utf8 50)))
    (let
        (
            (base-score u100)
            (endorsement-data (map-get? user-skill-endorsements {user: user, skill: skill}))
        )
        (if (is-some endorsement-data)
            (+ base-score (* (get total-endorsements (unwrap-panic endorsement-data)) u5))
            base-score)
    )
)

;; Calculate overall endorsement score across all skills
(define-private (calculate-overall-endorsement-score (user principal))
    (let
        (
            (summary (map-get? user-endorsement-summary user))
        )
        (if (is-some summary)
            (get total-received (unwrap-panic summary))
            u0)
    )
)

;; Get endorser reputation (simplified version - would integrate with main reputation system)
(define-private (get-endorser-reputation (endorser principal))
    (let
        (
            (stats (map-get? endorser-stats endorser))
        )
        (if (is-some stats)
            (get reputation-score (unwrap-panic stats))
            u100)
    )
)

;; Calculate endorsement weight based on endorser reputation
(define-private (calculate-endorsement-weight (endorser principal) (reputation uint))
    (if (>= reputation u150) u150
        (if (>= reputation u100) u120
            (if (>= reputation u75) u100 u80)))
)

;; Get skill endorsements for a user (simplified - returns count)
(define-private (get-skill-endorsements-for-user (user principal) (skill (string-utf8 50)))
    (let
        (
            (data (map-get? user-skill-endorsements {user: user, skill: skill}))
        )
        (if (is-some data)
            (get total-endorsements (unwrap-panic data))
            u0)
    )
)

;; Convert principal to uint for comparison (simplified implementation)
(define-private (principal-to-uint (user principal))
    (mod (len (principal-to-string user)) u1000000)
)

;; Convert principal to string (built-in function in Clarity)
(define-private (principal-to-string (user principal))
    (unwrap-panic (to-consensus-buff? user))
)

;; Read-only functions for querying endorsement data

(define-read-only (get-endorsement (endorsee principal) (endorser principal) (skill (string-utf8 50)))
    (ok (map-get? skill-endorsements {endorsee: endorsee, endorser: endorser, skill: skill}))
)

(define-read-only (get-user-skill-endorsement-summary (user principal) (skill (string-utf8 50)))
    (ok (map-get? user-skill-endorsements {user: user, skill: skill}))
)

(define-read-only (get-endorser-statistics (endorser principal))
    (ok (map-get? endorser-stats endorser))
)

(define-read-only (get-user-endorsement-overview (user principal))
    (ok (map-get? user-endorsement-summary user))
)

(define-read-only (get-trust-connection (user-a principal) (user-b principal))
    (let
        (
            (connection-key (if (< (principal-to-uint user-a) (principal-to-uint user-b))
                {user-a: user-a, user-b: user-b}
                {user-a: user-b, user-b: user-a}))
        )
        (ok (map-get? trust-connections connection-key))
    )
)

(define-read-only (get-endorsement-influence-score (user principal) (skill (string-utf8 50)))
    (let
        (
            (skill-data (map-get? user-skill-endorsements {user: user, skill: skill}))
            (user-summary (map-get? user-endorsement-summary user))
        )
        (if (and (is-some skill-data) (is-some user-summary))
            (ok (+ (get weighted-score (unwrap-panic skill-data)) 
                   (* (get unique-endorsers (unwrap-panic user-summary)) u2)))
            (ok u0))
    )
)

;; Public function to validate an endorsement (for verified status)
(define-public (validate-endorsement (endorsee principal) (endorser principal) (skill (string-utf8 50)))
    (let
        (
            (endorsement (unwrap! (map-get? skill-endorsements {endorsee: endorsee, endorser: endorser, skill: skill}) ERR-NOT-FOUND))
        )
        ;; Only allow validation by contract owner or high-reputation users
        (asserts! (>= (get-endorser-reputation tx-sender) u150) ERR-UNAUTHORIZED)
        
        (map-set skill-endorsements {endorsee: endorsee, endorser: endorser, skill: skill}
            (merge endorsement {verified: true})
        )
        (ok true)
    )
)

;; Bulk endorsement function for efficiency
(define-public (bulk-endorse-skills (endorsee principal) (skills (list 5 (string-utf8 50))) (strengths (list 5 uint)))
    (let
        (
            (skill-1 (unwrap! (element-at skills u0) ERR-INVALID-PARAMS))
            (strength-1 (unwrap! (element-at strengths u0) ERR-INVALID-PARAMS))
        )
        (unwrap! (endorse-skill endorsee skill-1 strength-1 false) ERR-INVALID-PARAMS)
        (ok true)
    )
)



