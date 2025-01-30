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
