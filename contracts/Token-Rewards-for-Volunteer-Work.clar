;; Token Rewards for Volunteer Work

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-hours (err u102))
(define-constant err-unauthorized (err u103))

(define-data-var token-id-nonce uint u0)
(define-data-var total-volunteer-hours uint u0)

(define-map volunteers
    { volunteer: principal }
    {
        hours: uint,
        total-rewards: uint,
        verified: bool,
        organization: (optional principal),
    }
)

(define-map organizations
    { org: principal }
    {
        name: (string-ascii 50),
        verified: bool,
        total-volunteers: uint,
    }
)

(define-map volunteer-activities
    { id: uint }
    {
        volunteer: principal,
        org: principal,
        hours: uint,
        description: (string-ascii 100),
        timestamp: uint,
        verified: bool,
    }
)

(define-non-fungible-token volunteer-proof uint)

(define-public (register-organization (name (string-ascii 50)))
    (let ((caller tx-sender))
        (asserts! (is-none (get-organization-data caller)) (err u104))
        (map-set organizations { org: caller } {
            name: name,
            verified: false,
            total-volunteers: u0,
        })
        (ok true)
    )
)

(define-public (verify-organization (org principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (match (get-organization-data org)
            org-data (begin
                (map-set organizations { org: org }
                    (merge org-data { verified: true })
                )
                (ok true)
            )
            (err u105)
        )
    )
)

(define-public (register-volunteer)
    (begin
        (asserts! (is-none (get-volunteer-data tx-sender)) (err u106))
        (map-set volunteers { volunteer: tx-sender } {
            hours: u0,
            total-rewards: u0,
            verified: false,
            organization: none,
        })
        (ok true)
    )
)

(define-public (log-volunteer-hours
        (org principal)
        (hours uint)
        (description (string-ascii 100))
    )
    (let (
            (volunteer tx-sender)
            (activity-id (+ (var-get token-id-nonce) u1))
            (current-height (unwrap-panic (get-block-height)))
        )
        (asserts! (> hours u0) err-invalid-hours)
        (asserts! (is-some (get-organization-data org)) (err u107))
        (asserts! (is-some (get-volunteer-data volunteer)) (err u108))
        (var-set token-id-nonce activity-id)
        (var-set total-volunteer-hours (+ (var-get total-volunteer-hours) hours))
        (map-set volunteer-activities { id: activity-id } {
            volunteer: volunteer,
            org: org,
            hours: hours,
            description: description,
            timestamp: current-height,
            verified: false,
        })
        (try! (mint-volunteer-proof activity-id volunteer))
        (ok activity-id)
    )
)

(define-public (verify-volunteer-hours (activity-id uint))
    (let ((activity (unwrap! (get-activity-data activity-id) err-not-found)))
        (asserts! (is-eq tx-sender (get org activity)) err-unauthorized)
        (asserts! (not (get verified activity)) (err u109))
        (map-set volunteer-activities { id: activity-id }
            (merge activity { verified: true })
        )
        (try! (update-volunteer-stats (get volunteer activity) (get hours activity)))
        (ok true)
    )
)

(define-private (update-volunteer-stats
        (volunteer principal)
        (hours uint)
    )
    (match (get-volunteer-data volunteer)
        v-data (begin
            (map-set volunteers { volunteer: volunteer }
                (merge v-data {
                    hours: (+ (get hours v-data) hours),
                    total-rewards: (+ (get total-rewards v-data) (calculate-rewards hours)),
                })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-private (calculate-rewards (hours uint))
    (* hours u10)
)

(define-private (mint-volunteer-proof
        (id uint)
        (recipient principal)
    )
    (nft-mint? volunteer-proof id recipient)
)

(define-read-only (get-volunteer-data (volunteer principal))
    (map-get? volunteers { volunteer: volunteer })
)

(define-read-only (get-organization-data (org principal))
    (map-get? organizations { org: org })
)

(define-read-only (get-activity-data (id uint))
    (map-get? volunteer-activities { id: id })
)

(define-read-only (get-total-volunteer-hours)
    (ok (var-get total-volunteer-hours))
)

(define-read-only (get-block-height)
    (ok stacks-block-height)
)
