;; Freelance Work Reputation System

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-ALREADY-REVIEWED (err u102))

;; Data Maps
(define-map freelancer-ratings
    principal ;; freelancer address
    {
        total-score: uint,
        review-count: uint,
        average-rating: uint
    }
)

(define-map client-reviews
    {client: principal, freelancer: principal}
    {reviewed: bool}
)

;; Public Functions
(define-public (submit-review (freelancer principal) (rating uint))
    (let
        (
            (sender tx-sender)
            (existing-review (default-to {reviewed: false} (map-get? client-reviews {client: sender, freelancer: freelancer})))
            (current-stats (default-to {total-score: u0, review-count: u0, average-rating: u0} 
                (map-get? freelancer-ratings freelancer)))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (not (get reviewed existing-review)) ERR-ALREADY-REVIEWED)
        
        (map-set client-reviews {client: sender, freelancer: freelancer} {reviewed: true})
        (ok (map-set freelancer-ratings freelancer
            {
                total-score: (+ (get total-score current-stats) rating),
                review-count: (+ (get review-count current-stats) u1),
                average-rating: (/ (+ (get total-score current-stats) rating) 
                                 (+ (get review-count current-stats) u1))
            }
        ))
    )
)

;; Read Only Functions
(define-read-only (get-freelancer-rating (freelancer principal))
    (ok (default-to 
        {total-score: u0, review-count: u0, average-rating: u0}
        (map-get? freelancer-ratings freelancer)))
)



;; Add new data map
(define-map freelancer-profiles
    principal
    {
        name: (string-utf8 50),
        skills: (string-utf8 200),
        experience-years: uint
    }
)

(define-public (create-profile (name (string-utf8 50)) (skills (string-utf8 200)) (experience uint))
    (ok (map-set freelancer-profiles tx-sender
        {
            name: name,
            skills: skills,
            experience-years: experience
        }
    ))
)




(define-map skill-endorsements
    {freelancer: principal, skill: (string-utf8 50)}
    {
        endorsement-count: uint,
        endorsers: (list 50 principal)
    }
)

(define-public (endorse-skill (freelancer principal) (skill (string-utf8 50)))
    (let ((current-endorsements (default-to {endorsement-count: u0, endorsers: (list)} 
            (map-get? skill-endorsements {freelancer: freelancer, skill: skill}))))
        (ok (map-set skill-endorsements {freelancer: freelancer, skill: skill}
            {
                endorsement-count: (+ (get endorsement-count current-endorsements) u1),
                endorsers: (unwrap-panic (as-max-len? (append (get endorsers current-endorsements) tx-sender) u50))
            }
        ))
    )
)



(define-map client-ratings
    principal
    {
        reliability-score: uint,
        projects-completed: uint
    }
)

(define-public (rate-client (client principal) (reliability uint))
    (let ((current-rating (default-to {reliability-score: u0, projects-completed: u0} 
            (map-get? client-ratings client))))
        (ok (map-set client-ratings client
            {
                reliability-score: (+ (get reliability-score current-rating) reliability),
                projects-completed: (+ (get projects-completed current-rating) u1)
            }
        ))
    )
)



(define-map detailed-reviews
    {client: principal, freelancer: principal}
    {
        communication: uint,
        quality: uint,
        timeliness: uint,
        overall: uint
    }
)

(define-public (submit-detailed-review (freelancer principal) (comm uint) (qual uint) (time uint))
    (ok (map-set detailed-reviews {client: tx-sender, freelancer: freelancer}
        {
            communication: comm,
            quality: qual,
            timeliness: time,
            overall: (/ (+ comm qual time) u3)
        }
    ))
)



(define-map freelancer-badges
    principal
    {
        badges: (list 10 (string-utf8 50)),
        total-badges: uint
    }
)

(define-public (award-badge (freelancer principal) (badge (string-utf8 50)))
    (let ((current-badges (default-to {badges: (list), total-badges: u0} 
            (map-get? freelancer-badges freelancer))))
        (ok (map-set freelancer-badges freelancer
            {
                badges: (unwrap-panic (as-max-len? (append (get badges current-badges) badge) u10)),
                total-badges: (+ (get total-badges current-badges) u1)
            }
        ))
    )
)




(define-map work-history
    principal
    {
        completed-projects: uint,
        client-list: (list 50 principal),
        total-earnings: uint
    }
)

(define-public (add-project (freelancer principal) (earnings uint))
    (let ((current-history (default-to {completed-projects: u0, client-list: (list), total-earnings: u0} 
            (map-get? work-history freelancer))))
        (ok (map-set work-history freelancer
            {
                completed-projects: (+ (get completed-projects current-history) u1),
                client-list: (unwrap-panic (as-max-len? (append (get client-list current-history) tx-sender) u50)),
                total-earnings: (+ (get total-earnings current-history) earnings)
            }
        ))
    )
)
