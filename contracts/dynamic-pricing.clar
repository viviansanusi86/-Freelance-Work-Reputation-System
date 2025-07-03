(define-constant ERR-NOT-FOUND (err u500))
(define-constant ERR-INVALID-PARAMS (err u501))
(define-constant ERR-UNAUTHORIZED (err u502))

(define-map market-demand
    (string-utf8 50)
    {
        total-projects: uint,
        total-freelancers: uint,
        avg-completion-time: uint,
        base-rate: uint,
        demand-multiplier: uint
    }
)

(define-map skill-rarity
    (string-utf8 50)
    {
        freelancer-count: uint,
        project-count: uint,
        rarity-score: uint,
        premium-multiplier: uint
    }
)

(define-map freelancer-performance
    principal
    {
        success-rate: uint,
        avg-rating: uint,
        completion-speed: uint,
        performance-score: uint,
        rate-multiplier: uint
    }
)

(define-map project-complexity
    {client: principal, project-type: (string-utf8 50)}
    {
        estimated-hours: uint,
        required-skills: (list 5 (string-utf8 50)),
        complexity-score: uint,
        rate-adjustment: uint
    }
)

(define-map pricing-recommendations
    {freelancer: principal, skill: (string-utf8 50)}
    {
        base-rate: uint,
        market-adjusted-rate: uint,
        performance-bonus: uint,
        final-rate: uint,
        last-updated: uint
    }
)

(define-public (update-market-demand (skill (string-utf8 50)) (projects uint) (freelancers uint) (completion-time uint))
    (let
        (
            (demand-ratio (if (> freelancers u0) (/ projects freelancers) u0))
            (multiplier (if (> demand-ratio u2) u150 (if (> demand-ratio u1) u120 u100)))
            (base-rate (if (> demand-ratio u2) u50 (if (> demand-ratio u1) u30 u20)))
        )
        (ok (map-set market-demand skill
            {
                total-projects: projects,
                total-freelancers: freelancers,
                avg-completion-time: completion-time,
                base-rate: base-rate,
                demand-multiplier: multiplier
            }
        ))
    )
)

(define-public (update-skill-rarity (skill (string-utf8 50)) (freelancer-count uint) (project-count uint))
    (let
        (
            (rarity-score (if (< freelancer-count u5) u100 (if (< freelancer-count u20) u75 u50)))
            (premium (if (< freelancer-count u5) u200 (if (< freelancer-count u20) u150 u100)))
        )
        (ok (map-set skill-rarity skill
            {
                freelancer-count: freelancer-count,
                project-count: project-count,
                rarity-score: rarity-score,
                premium-multiplier: premium
            }
        ))
    )
)

(define-public (update-freelancer-performance (freelancer principal) (success-rate uint) (avg-rating uint) (completion-speed uint))
    (let
        (
            (performance-score (/ (+ success-rate (* avg-rating u20) completion-speed) u3))
            (rate-multiplier (if (> performance-score u90) u150 (if (> performance-score u70) u120 u100)))
        )
        (ok (map-set freelancer-performance freelancer
            {
                success-rate: success-rate,
                avg-rating: avg-rating,
                completion-speed: completion-speed,
                performance-score: performance-score,
                rate-multiplier: rate-multiplier
            }
        ))
    )
)

(define-public (analyze-project-complexity (client principal) (project-type (string-utf8 50)) (estimated-hours uint) (required-skills (list 5 (string-utf8 50))))
    (let
        (
            (skill-count (len required-skills))
            (complexity-score (+ (* skill-count u10) (/ estimated-hours u10)))
            (rate-adjustment (if (> complexity-score u100) u140 (if (> complexity-score u50) u120 u100)))
        )
        (ok (map-set project-complexity {client: client, project-type: project-type}
            {
                estimated-hours: estimated-hours,
                required-skills: required-skills,
                complexity-score: complexity-score,
                rate-adjustment: rate-adjustment
            }
        ))
    )
)

(define-public (calculate-dynamic-rate (freelancer principal) (skill (string-utf8 50)))
    (let
        (
            (market-data (unwrap! (map-get? market-demand skill) ERR-NOT-FOUND))
            (rarity-data (unwrap! (map-get? skill-rarity skill) ERR-NOT-FOUND))
            (performance-data (unwrap! (map-get? freelancer-performance freelancer) ERR-NOT-FOUND))
            
            (base-rate (get base-rate market-data))
            (market-multiplier (get demand-multiplier market-data))
            (rarity-multiplier (get premium-multiplier rarity-data))
            (performance-multiplier (get rate-multiplier performance-data))
            
            (market-adjusted-rate (/ (* base-rate market-multiplier) u100))
            (rarity-adjusted-rate (/ (* market-adjusted-rate rarity-multiplier) u100))
            (performance-bonus (/ (* rarity-adjusted-rate (- performance-multiplier u100)) u100))
            (final-rate (+ rarity-adjusted-rate performance-bonus))
        )
        (map-set pricing-recommendations {freelancer: freelancer, skill: skill}
            {
                base-rate: base-rate,
                market-adjusted-rate: market-adjusted-rate,
                performance-bonus: performance-bonus,
                final-rate: final-rate,
                last-updated: stacks-block-height
            }
        )
        (ok final-rate)
    )
)

(define-public (get-rate-for-project (freelancer principal) (skill (string-utf8 50)) (client principal) (project-type (string-utf8 50)))
    (let
        (
            (base-rate (try! (calculate-dynamic-rate freelancer skill)))
            (complexity-data (map-get? project-complexity {client: client, project-type: project-type}))
            (complexity-multiplier (if (is-some complexity-data) 
                (get rate-adjustment (unwrap-panic complexity-data)) 
                u100))
            (project-adjusted-rate (/ (* base-rate complexity-multiplier) u100))
        )
        (ok project-adjusted-rate)
    )
)

(define-read-only (get-market-demand (skill (string-utf8 50)))
    (ok (map-get? market-demand skill))
)

(define-read-only (get-skill-rarity (skill (string-utf8 50)))
    (ok (map-get? skill-rarity skill))
)

(define-read-only (get-freelancer-performance (freelancer principal))
    (ok (map-get? freelancer-performance freelancer))
)

(define-read-only (get-pricing-recommendation (freelancer principal) (skill (string-utf8 50)))
    (ok (map-get? pricing-recommendations {freelancer: freelancer, skill: skill}))
)

(define-read-only (get-project-complexity (client principal) (project-type (string-utf8 50)))
    (ok (map-get? project-complexity {client: client, project-type: project-type}))
)

(define-public (batch-update-market-data (skills (list 10 (string-utf8 50))) (project-counts (list 10 uint)) (freelancer-counts (list 10 uint)))
    (let
        (
            (skill-1 (unwrap! (element-at skills u0) ERR-INVALID-PARAMS))
            (proj-1 (unwrap! (element-at project-counts u0) ERR-INVALID-PARAMS))
            (free-1 (unwrap! (element-at freelancer-counts u0) ERR-INVALID-PARAMS))
        )
        (unwrap! (update-market-demand skill-1 proj-1 free-1 u168) ERR-INVALID-PARAMS)
        (ok true)
    )
)

(define-public (initialize-skill-market (skill (string-utf8 50)))
    (begin
        (unwrap! (update-market-demand skill u0 u0 u168) ERR-INVALID-PARAMS)
        (unwrap! (update-skill-rarity skill u0 u1) ERR-INVALID-PARAMS)
        (ok true)
    )
)

(define-read-only (get-market-trend (skill (string-utf8 50)))
    (let
        (
            (market-data (unwrap! (map-get? market-demand skill) ERR-NOT-FOUND))
            (projects (get total-projects market-data))
            (freelancers (get total-freelancers market-data))
            (trend (if (> projects (* freelancers u2)) "high-demand" 
                      (if (> projects freelancers) "moderate-demand" "low-demand")))
        )
        (ok trend)
    )
)

(define-public (update-freelancer-from-review (freelancer principal) (rating uint) (completed-on-time bool))
    (let
        (
            (current-perf (default-to {success-rate: u100, avg-rating: u0, completion-speed: u100, performance-score: u0, rate-multiplier: u100} 
                (map-get? freelancer-performance freelancer)))
            (new-success-rate (if completed-on-time u100 u80))
            (new-avg-rating (* rating u20))
            (new-completion-speed (if completed-on-time u100 u70))
        )
        (update-freelancer-performance freelancer new-success-rate new-avg-rating new-completion-speed)
    )
)
