(define-constant ERR-NOT-FOUND (err u400))
(define-constant ERR-INVALID-PARAMS (err u401))
(define-constant ERR-UNAUTHORIZED (err u402))

(define-map client-requirements
    principal
    {
        required-skills: (list 10 (string-utf8 50)),
        min-rating: uint,
        max-hourly-rate: uint,
        project-duration: uint,
        budget-range: uint,
        preferred-timezone: (string-utf8 50)
    }
)

(define-map freelancer-preferences
    principal
    {
        preferred-project-types: (list 10 (string-utf8 50)),
        min-hourly-rate: uint,
        max-project-duration: uint,
        preferred-clients: (list 20 principal),
        availability-status: bool
    }
)

(define-map match-results
    {client: principal, freelancer: principal}
    {
        compatibility-score: uint,
        skill-match-score: uint,
        rating-score: uint,
        rate-compatibility: uint,
        availability-match: bool,
        last-calculated: uint
    }
)

(define-map client-matches
    {client: principal, match-id: uint}
    {
        freelancer: principal,
        score: uint,
        recommended: bool
    }
)

(define-map client-match-counters
    principal
    {match-count: uint}
)

(define-public (set-client-requirements (skills (list 10 (string-utf8 50))) 
                                       (min-rating uint) 
                                       (max-rate uint) 
                                       (duration uint) 
                                       (budget uint) 
                                       (timezone (string-utf8 50)))
    (begin
        (asserts! (<= min-rating u5) ERR-INVALID-PARAMS)
        (asserts! (> max-rate u0) ERR-INVALID-PARAMS)
        (ok (map-set client-requirements tx-sender
            {
                required-skills: skills,
                min-rating: min-rating,
                max-hourly-rate: max-rate,
                project-duration: duration,
                budget-range: budget,
                preferred-timezone: timezone
            }
        ))
    )
)

(define-public (set-freelancer-preferences (project-types (list 10 (string-utf8 50))) 
                                          (min-rate uint) 
                                          (max-duration uint) 
                                          (preferred-clients (list 20 principal)) 
                                          (available bool))
    (begin
        (asserts! (> min-rate u0) ERR-INVALID-PARAMS)
        (ok (map-set freelancer-preferences tx-sender
            {
                preferred-project-types: project-types,
                min-hourly-rate: min-rate,
                max-project-duration: max-duration,
                preferred-clients: preferred-clients,
                availability-status: available
            }
        ))
    )
)

(define-private (calculate-skill-match-score (required-skills (list 10 (string-utf8 50))) 
                                           (freelancer-skills (list 10 (string-utf8 50))))
    (let
        (
            (total-required (len required-skills))
            (matched-skills (fold count-matching-skills required-skills u0))
        )
        (if (> total-required u0)
            (/ (* matched-skills u100) total-required)
            u0
        )
    )
)

(define-private (count-matching-skills (skill (string-utf8 50)) (acc uint))
    (+ acc u1)
)

(define-private (calculate-rate-compatibility (client-max uint) (freelancer-min uint))
    (if (>= client-max freelancer-min)
        u100
        (if (> freelancer-min u0)
            (/ (* client-max u100) freelancer-min)
            u0
        )
    )
)

(define-private (calculate-rating-score (min-required uint) (actual-rating uint))
    (if (>= actual-rating min-required)
        u100
        (if (> min-required u0)
            (/ (* actual-rating u100) min-required)
            u0
        )
    )
)

(define-public (calculate-match-score (client principal) (freelancer principal))
    (let
        (
            (client-reqs (unwrap! (map-get? client-requirements client) ERR-NOT-FOUND))
            (freelancer-prefs (unwrap! (map-get? freelancer-preferences freelancer) ERR-NOT-FOUND))
         
            (freelancer-rating (unwrap! (contract-call? .freelance-rep get-freelancer-rating freelancer) ERR-NOT-FOUND))
          
            (rating-score (calculate-rating-score 
                (get min-rating client-reqs) 
                (get average-rating freelancer-rating)))
            (rate-compatibility (calculate-rate-compatibility 
                (get max-hourly-rate client-reqs) 
                (get min-hourly-rate freelancer-prefs)))
            (availability-match (get availability-status freelancer-prefs))
        )
        (map-set match-results {client: client, freelancer: freelancer}
            {
                compatibility-score: u1,
                skill-match-score: u2,
                rating-score: rating-score,
                rate-compatibility: rate-compatibility,
                availability-match: availability-match,
                last-calculated: stacks-block-height
            }
        )
        (ok u1)
    )
)

(define-public (find-matches-for-client (client principal))
    (let
        (
            (client-reqs (unwrap! (map-get? client-requirements client) ERR-NOT-FOUND))
            (counter (default-to {match-count: u0} (map-get? client-match-counters client)))
        )
        (asserts! (is-eq tx-sender client) ERR-UNAUTHORIZED)
        (map-set client-match-counters client {match-count: u0})
        (ok true)
    )
)

(define-public (add-match-result (client principal) (freelancer principal) (score uint))
    (let
        (
            (counter (default-to {match-count: u0} (map-get? client-match-counters client)))
            (match-id (get match-count counter))
            (is-recommended (>= score u70))
        )
        (map-set client-match-counters client 
            {match-count: (+ match-id u1)})
        (ok (map-set client-matches {client: client, match-id: match-id}
            {
                freelancer: freelancer,
                score: score,
                recommended: is-recommended
            }
        ))
    )
)

(define-read-only (get-client-requirements (client principal))
    (ok (map-get? client-requirements client))
)

(define-read-only (get-freelancer-preferences (freelancer principal))
    (ok (map-get? freelancer-preferences freelancer))
)

(define-read-only (get-match-result (client principal) (freelancer principal))
    (ok (map-get? match-results {client: client, freelancer: freelancer}))
)

(define-read-only (get-client-match (client principal) (match-id uint))
    (ok (map-get? client-matches {client: client, match-id: match-id}))
)

(define-read-only (get-client-match-count (client principal))
    (ok (get match-count (default-to {match-count: u0} 
        (map-get? client-match-counters client))))
)

(define-public (update-match-recommendation (client principal) (match-id uint) (recommended bool))
    (let
        (
            (match (unwrap! (map-get? client-matches {client: client, match-id: match-id}) ERR-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender client) ERR-UNAUTHORIZED)
        (ok (map-set client-matches {client: client, match-id: match-id}
            (merge match {recommended: recommended})
        ))
    )
)

(define-read-only (get-recommended-matches (client principal))
    (let
        (
            (match-count (get match-count (default-to {match-count: u0} 
                (map-get? client-match-counters client))))
        )
        (ok match-count)
    )
)