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





;; Add to existing data maps
(define-map disputes
    {dispute-id: uint}
    {
        client: principal,
        freelancer: principal,
        description: (string-utf8 500),
        status: (string-ascii 4),
        created-at: uint,
        resolved-at: uint
    }
)

(define-data-var dispute-counter uint u0)

;; Public function to create a dispute
(define-public (create-dispute (freelancer principal) (description (string-utf8 500)))
    (let
        (
            (dispute-id (var-get dispute-counter))
            (client tx-sender)
            (current-block-height stacks-block-height)
        )
        ;; Increment the dispute counter
        (var-set dispute-counter (+ dispute-id u1))
        
        ;; Create the dispute
        (ok (map-set disputes {dispute-id: dispute-id}
            {
                client: client,
                freelancer: freelancer,
                description: description,
                status: "open",
                created-at: stacks-block-height,
                resolved-at: u0
            }
        ))
    )
)

;; Function to resolve a dispute
(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 4)))
    (let
        (
            (dispute (unwrap! (map-get? disputes {dispute-id: dispute-id}) (err u103)))
            (current-block-height stacks-block-height)
        )
        ;; Only client or freelancer can resolve
        (asserts! (or (is-eq tx-sender (get client dispute)) 
                     (is-eq tx-sender (get freelancer dispute))) 
                 ERR-NOT-AUTHORIZED)
        
        ;; Update the dispute status
        (ok (map-set disputes {dispute-id: dispute-id}
            {
                client: (get client dispute),
                freelancer: (get freelancer dispute),
                description: (get description dispute),
                status: resolution,
                created-at: (get created-at dispute),
                resolved-at: stacks-block-height
            }
        ))
    )
)

;; Read-only function to get dispute details
(define-read-only (get-dispute (dispute-id uint))
    (ok (map-get? disputes {dispute-id: dispute-id}))
)


;; Add to existing data maps
(define-map projects
    {project-id: uint}
    {
        client: principal,
        freelancer: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        status: (string-ascii 6),
        created-at: uint
    }
)

(define-map milestones
    {project-id: uint, milestone-id: uint}
    {
        title: (string-utf8 100),
        description: (string-utf8 300),
        deadline: uint,
        completed: bool,
        completed-at: uint
    }
)

(define-data-var project-counter uint u0)

;; Create a new project
(define-public (create-project (freelancer principal) (title (string-utf8 100)) (description (string-utf8 500)))
    (let
        (
            (project-id (var-get project-counter))
            (client tx-sender)
            (current-block-height stacks-block-height)
        )
        ;; Increment the project counter
        (var-set project-counter (+ project-id u1))
        
        ;; Create the project
        (ok (map-set projects {project-id: project-id}
            {
                client: client,
                freelancer: freelancer,
                title: title,
                description: description,
                status: "active",
                created-at: stacks-block-height
            }
        ))
    )
)

;; Add a milestone to a project
(define-public (add-milestone (project-id uint) (milestone-id uint) (title (string-utf8 100)) 
                             (description (string-utf8 300)) (deadline uint))
    (let
        (
            (project (unwrap! (map-get? projects {project-id: project-id}) (err u104)))
        )
        ;; Only client can add milestones
        (asserts! (is-eq tx-sender (get client project)) ERR-NOT-AUTHORIZED)
        
        ;; Create the milestone
        (ok (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            {
                title: title,
                description: description,
                deadline: deadline,
                completed: false,
                completed-at: u0
            }
        ))
    )
)

;; Mark a milestone as completed
(define-public (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (project (unwrap! (map-get? projects {project-id: project-id}) (err u104)))
            (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) (err u105)))
            (current-block-height stacks-block-height)
        )
        ;; Only client can mark milestone as completed
        (asserts! (is-eq tx-sender (get client project)) ERR-NOT-AUTHORIZED)
        
        ;; Update the milestone
        (ok (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            (merge milestone {
                completed: true,
                completed-at: stacks-block-height
            })
        ))
    )
)



;; Add to existing data maps
(define-map skill-categories
    (string-utf8 50)
    {
        name: (string-utf8 50),
        description: (string-utf8 200)
    }
)

(define-map freelancer-specialized-skills
    {freelancer: principal, category: (string-utf8 50), skill: (string-utf8 50)}
    {
        proficiency-level: uint,
        years-experience: uint,
        endorsement-count: uint
    }
)

;; Add a skill category
(define-public (add-skill-category (name (string-utf8 50)) (description (string-utf8 200)))
    (ok (map-set skill-categories name
        {
            name: name,
            description: description
        }
    ))
)

;; Add a specialized skill for a freelancer
(define-public (add-specialized-skill (category (string-utf8 50)) (skill (string-utf8 50)) 
                                     (proficiency uint) (years uint))
    (let
        (
            (freelancer tx-sender)
        )
        ;; Proficiency should be between 1 and 5
        (asserts! (and (>= proficiency u1) (<= proficiency u5)) ERR-INVALID-RATING)
        
        ;; Add the specialized skill
        (ok (map-set freelancer-specialized-skills 
            {freelancer: freelancer, category: category, skill: skill}
            {
                proficiency-level: proficiency,
                years-experience: years,
                endorsement-count: u0
            }
        ))
    )
)

;; Endorse a specialized skill
(define-public (endorse-specialized-skill (freelancer principal) (category (string-utf8 50)) (skill (string-utf8 50)))
    (let
        (
            (current-skill (unwrap! (map-get? freelancer-specialized-skills 
                {freelancer: freelancer, category: category, skill: skill}) (err u106)))
        )
        ;; Update the endorsement count
        (ok (map-set freelancer-specialized-skills 
            {freelancer: freelancer, category: category, skill: skill}
            (merge current-skill {
                endorsement-count: (+ (get endorsement-count current-skill) u1)
            })
        ))
    )
)

;; Get specialized skills for a freelancer
(define-read-only (get-specialized-skills (freelancer principal) (category (string-utf8 50)) (skill (string-utf8 50)))
    (ok (map-get? freelancer-specialized-skills {freelancer: freelancer, category: category, skill: skill}))
)



;; Add to existing data maps
(define-map portfolio-projects
    {freelancer: principal, project-id: uint}
    {
        title: (string-utf8 100),
        description: (string-utf8 500),
        image-url: (string-utf8 200),
        project-url: (string-utf8 200),
        tags: (list 10 (string-utf8 50)),
        created-at: uint
    }
)

(define-map freelancer-portfolio-counters
    principal
    {
        project-count: uint
    }
)

;; Add a portfolio project
(define-public (add-portfolio-project (title (string-utf8 100)) (description (string-utf8 500))
                                     (image-url (string-utf8 200)) (project-url (string-utf8 200))
                                     (tags (list 10 (string-utf8 50))))
    (let
        (
            (freelancer tx-sender)
            (counter (default-to {project-count: u0} (map-get? freelancer-portfolio-counters freelancer)))
            (project-id (get project-count counter))
            (current-block-height stacks-block-height)
        )
        ;; Update the project counter
        (map-set freelancer-portfolio-counters freelancer
            {project-count: (+ project-id u1)}
        )
        
        ;; Add the portfolio project
        (ok (map-set portfolio-projects {freelancer: freelancer, project-id: project-id}
            {
                title: title,
                description: description,
                image-url: image-url,
                project-url: project-url,
                tags: tags,
                created-at: stacks-block-height
            }
        ))
    )
)

;; Get a portfolio project
(define-read-only (get-portfolio-project (freelancer principal) (project-id uint))
    (ok (map-get? portfolio-projects {freelancer: freelancer, project-id: project-id}))
)

;; Get portfolio project count
(define-read-only (get-portfolio-count (freelancer principal))
    (ok (get project-count (default-to {project-count: u0} 
        (map-get? freelancer-portfolio-counters freelancer))))
)


;; Add to existing data maps
(define-map certifications
    {freelancer: principal, cert-id: uint}
    {
        name: (string-utf8 100),
        issuer: (string-utf8 100),
        issue-date: uint,
        expiry-date: uint,
        verification-url: (string-utf8 200),
        verified: bool
    }
)

(define-map certification-authorities
    principal
    {
        name: (string-utf8 100),
        website: (string-utf8 200),
        verified: bool
    }
)

(define-map freelancer-certification-counters
    principal
    {
        cert-count: uint
    }
)

;; Register as a certification authority
(define-public (register-certification-authority (name (string-utf8 100)) (website (string-utf8 200)))
    (ok (map-set certification-authorities tx-sender
        {
            name: name,
            website: website,
            verified: false
        }
    ))
)

;; Add a certification
(define-public (add-certification (name (string-utf8 100)) (issuer (string-utf8 100))
                                 (issue-date uint) (expiry-date uint) (verification-url (string-utf8 200)))
    (let
        (
            (freelancer tx-sender)
            (counter (default-to {cert-count: u0} (map-get? freelancer-certification-counters freelancer)))
            (cert-id (get cert-count counter))
        )
        ;; Update the certification counter
        (map-set freelancer-certification-counters freelancer
            {cert-count: (+ cert-id u1)}
        )
        
        ;; Add the certification
        (ok (map-set certifications {freelancer: freelancer, cert-id: cert-id}
            {
                name: name,
                issuer: issuer,
                issue-date: issue-date,
                expiry-date: expiry-date,
                verification-url: verification-url,
                verified: false
            }
        ))
    )
)

;; Verify a certification (only by certification authorities)
(define-public (verify-certification (freelancer principal) (cert-id uint))
    (let
        (
            (authority tx-sender)
            (authority-info (unwrap! (map-get? certification-authorities authority) (err u107)))
            (cert (unwrap! (map-get? certifications {freelancer: freelancer, cert-id: cert-id}) (err u108)))
        )
        ;; Only verified authorities can verify certifications
        (asserts! (get verified authority-info) ERR-NOT-AUTHORIZED)
        
        ;; Update the certification
        (ok (map-set certifications {freelancer: freelancer, cert-id: cert-id}
            (merge cert {
                verified: true
            })
        ))
    )
)


;; Add to existing data maps
(define-map freelancer-availability
    principal
    {
        status: (string-utf8 20),
        available-from: uint,
        hours-per-week: uint,
        timezone: (string-utf8 50),
        booking-link: (string-utf8 200)
    }
)

(define-map availability-slots
    {freelancer: principal, slot-id: uint}
    {
        day-of-week: uint,
        start-time: uint,
        end-time: uint,
        available: bool
    }
)

(define-map freelancer-slot-counters
    principal
    {
        slot-count: uint
    }
)

;; Update availability status
(define-public (update-availability (status (string-utf8 20)) (available-from uint) 
                                   (hours uint) (timezone (string-utf8 50)) (booking-link (string-utf8 200)))
    (let
        (
            (freelancer tx-sender)
        )
        ;; Update the availability
        (ok (map-set freelancer-availability freelancer
            {
                status: status,
                available-from: available-from,
                hours-per-week: hours,
                timezone: timezone,
                booking-link: booking-link
            }
        ))
    )
)
