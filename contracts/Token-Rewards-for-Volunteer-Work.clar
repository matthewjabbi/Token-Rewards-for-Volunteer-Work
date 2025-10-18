;; Token Rewards for Volunteer Work

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-hours (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-rating (err u110))
(define-constant err-activity-not-verified (err u111))
(define-constant err-already-rated (err u112))

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
            (current-height stacks-block-height)
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
        v-data (let ((new-total-hours (+ (get hours v-data) hours)))
            (map-set volunteers { volunteer: volunteer }
                (merge v-data {
                    hours: new-total-hours,
                    total-rewards: (+ (get total-rewards v-data) (calculate-rewards hours)),
                })
            )
            (unwrap-panic (check-and-award-milestones volunteer new-total-hours))
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

(define-data-var rating-id-nonce uint u0)

(define-map organization-ratings
    { org: principal }
    {
        total-ratings: uint,
        total-score: uint,
        average-rating: uint,
    }
)

(define-map volunteer-ratings
    { id: uint }
    {
        volunteer: principal,
        org: principal,
        rating: uint,
        comment: (string-ascii 200),
        timestamp: uint,
        activity-id: uint,
    }
)

(define-map volunteer-org-ratings
    {
        volunteer: principal,
        org: principal,
    }
    { has-rated: bool }
)

(define-public (rate-organization
        (org principal)
        (rating uint)
        (comment (string-ascii 200))
        (activity-id uint)
    )
    (let (
            (volunteer tx-sender)
            (rating-id (+ (var-get rating-id-nonce) u1))
            (current-height stacks-block-height)
            (activity (unwrap! (get-activity-data activity-id) err-not-found))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) (err u110))
        (asserts! (is-eq (get volunteer activity) volunteer) err-unauthorized)
        (asserts! (is-eq (get org activity) org) err-unauthorized)
        (asserts! (get verified activity) (err u111))
        (asserts!
            (is-none (map-get? volunteer-org-ratings {
                volunteer: volunteer,
                org: org,
            }))
            (err u112)
        )
        (begin
            (var-set rating-id-nonce rating-id)
            (map-set volunteer-ratings { id: rating-id } {
                volunteer: volunteer,
                org: org,
                rating: rating,
                comment: comment,
                timestamp: current-height,
                activity-id: activity-id,
            })
            (map-set volunteer-org-ratings {
                volunteer: volunteer,
                org: org,
            } { has-rated: true }
            )
            (update-organization-rating org rating)
            (ok rating-id)
        )
    )
)

(define-private (update-organization-rating
        (org principal)
        (new-rating uint)
    )
    (let (
            (current-ratings (default-to {
                total-ratings: u0,
                total-score: u0,
                average-rating: u0,
            }
                (map-get? organization-ratings { org: org })
            ))
            (new-total-ratings (+ (get total-ratings current-ratings) u1))
            (new-total-score (+ (get total-score current-ratings) new-rating))
            (new-average (if (> new-total-ratings u0)
                (/ new-total-score new-total-ratings)
                u0
            ))
        )
        (map-set organization-ratings { org: org } {
            total-ratings: new-total-ratings,
            total-score: new-total-score,
            average-rating: new-average,
        })
    )
)

(define-read-only (get-organization-rating (org principal))
    (map-get? organization-ratings { org: org })
)

(define-read-only (get-rating-data (id uint))
    (map-get? volunteer-ratings { id: id })
)

(define-read-only (has-volunteer-rated-org
        (volunteer principal)
        (org principal)
    )
    (is-some (map-get? volunteer-org-ratings {
        volunteer: volunteer,
        org: org,
    }))
)

(define-read-only (get-organization-reputation-score (org principal))
    (match (get-organization-rating org)
        rating-data (ok (get average-rating rating-data))
        (ok u0)
    )
)

(define-read-only (can-rate-organization
        (volunteer principal)
        (org principal)
        (activity-id uint)
    )
    (match (get-activity-data activity-id)
        activity (ok (and
            (is-eq (get volunteer activity) volunteer)
            (is-eq (get org activity) org)
            (get verified activity)
            (not (has-volunteer-rated-org volunteer org))
        ))
        (ok false)
    )
)

(define-constant milestone-tier-1 u10)
(define-constant milestone-tier-2 u50)
(define-constant milestone-tier-3 u100)
(define-constant milestone-tier-4 u250)
(define-constant milestone-tier-5 u500)

(define-map volunteer-milestones
    { volunteer: principal }
    {
        tier-1-achieved: bool,
        tier-2-achieved: bool,
        tier-3-achieved: bool,
        tier-4-achieved: bool,
        tier-5-achieved: bool,
        highest-tier: uint,
        achievement-count: uint,
    }
)

(define-private (check-and-award-milestones
        (volunteer principal)
        (total-hours uint)
    )
    (let (
            (current-milestones (default-to {
                tier-1-achieved: false,
                tier-2-achieved: false,
                tier-3-achieved: false,
                tier-4-achieved: false,
                tier-5-achieved: false,
                highest-tier: u0,
                achievement-count: u0,
            }
                (map-get? volunteer-milestones { volunteer: volunteer })
            ))
            (new-tier-1 (or (get tier-1-achieved current-milestones) (>= total-hours milestone-tier-1)))
            (new-tier-2 (or (get tier-2-achieved current-milestones) (>= total-hours milestone-tier-2)))
            (new-tier-3 (or (get tier-3-achieved current-milestones) (>= total-hours milestone-tier-3)))
            (new-tier-4 (or (get tier-4-achieved current-milestones) (>= total-hours milestone-tier-4)))
            (new-tier-5 (or (get tier-5-achieved current-milestones) (>= total-hours milestone-tier-5)))
            (achievement-count (+ (if new-tier-1
                u1
                u0
            )
                (if new-tier-2
                    u1
                    u0
                )
                (if new-tier-3
                    u1
                    u0
                )
                (if new-tier-4
                    u1
                    u0
                )
                (if new-tier-5
                    u1
                    u0
                )))
            (highest-tier (if new-tier-5
                u5
                (if new-tier-4
                    u4
                    (if new-tier-3
                        u3
                        (if new-tier-2
                            u2
                            (if new-tier-1
                                u1
                                u0
                            )
                        )
                    )
                )
            ))
        )
        (begin
            (map-set volunteer-milestones { volunteer: volunteer } {
                tier-1-achieved: new-tier-1,
                tier-2-achieved: new-tier-2,
                tier-3-achieved: new-tier-3,
                tier-4-achieved: new-tier-4,
                tier-5-achieved: new-tier-5,
                highest-tier: highest-tier,
                achievement-count: achievement-count,
            })
            (ok true)
        )
    )
)

(define-read-only (get-volunteer-milestones (volunteer principal))
    (map-get? volunteer-milestones { volunteer: volunteer })
)

(define-read-only (get-milestone-requirements)
    (ok {
        tier-1: milestone-tier-1,
        tier-2: milestone-tier-2,
        tier-3: milestone-tier-3,
        tier-4: milestone-tier-4,
        tier-5: milestone-tier-5,
    })
)

(define-constant err-skill-not-found (err u113))
(define-constant err-skill-already-exists (err u114))
(define-constant err-invalid-skill-level (err u115))
(define-constant err-cannot-verify-own-skill (err u116))
(define-constant err-already-endorsed (err u117))

(define-data-var skill-id-nonce uint u0)

(define-map volunteer-skills
    {
        volunteer: principal,
        skill-name: (string-ascii 30),
    }
    {
        skill-level: uint,
        verified: bool,
        verifier-count: uint,
        total-endorsements: uint,
    }
)

(define-map skill-verifications
    { id: uint }
    {
        volunteer: principal,
        verifier: principal,
        skill-name: (string-ascii 30),
        activity-id: uint,
        timestamp: uint,
    }
)

(define-map skill-endorsements
    {
        volunteer: principal,
        skill-name: (string-ascii 30),
        endorser: principal,
    }
    { endorsed: bool }
)

(define-public (claim-skill
        (skill-name (string-ascii 30))
        (skill-level uint)
    )
    (let ((volunteer tx-sender))
        (asserts! (is-some (get-volunteer-data volunteer)) (err u108))
        (asserts! (and (>= skill-level u1) (<= skill-level u5))
            err-invalid-skill-level
        )
        (asserts! (is-none (get-volunteer-skill volunteer skill-name))
            err-skill-already-exists
        )
        (map-set volunteer-skills {
            volunteer: volunteer,
            skill-name: skill-name,
        } {
            skill-level: skill-level,
            verified: false,
            verifier-count: u0,
            total-endorsements: u0,
        })
        (ok true)
    )
)

(define-public (verify-volunteer-skill
        (volunteer principal)
        (skill-name (string-ascii 30))
        (activity-id uint)
    )
    (let (
            (verifier tx-sender)
            (verification-id (+ (var-get skill-id-nonce) u1))
            (activity (unwrap! (get-activity-data activity-id) err-not-found))
            (skill-data (unwrap! (get-volunteer-skill volunteer skill-name)
                err-skill-not-found
            ))
        )
        (asserts! (is-eq verifier (get org activity)) err-unauthorized)
        (asserts! (is-eq volunteer (get volunteer activity)) err-unauthorized)
        (asserts! (get verified activity) err-activity-not-verified)
        (asserts! (not (is-eq verifier volunteer)) err-cannot-verify-own-skill)
        (let ((updated-verifiers (+ (get verifier-count skill-data) u1)))
            (var-set skill-id-nonce verification-id)
            (map-set skill-verifications { id: verification-id } {
                volunteer: volunteer,
                verifier: verifier,
                skill-name: skill-name,
                activity-id: activity-id,
                timestamp: stacks-block-height,
            })
            (map-set volunteer-skills {
                volunteer: volunteer,
                skill-name: skill-name,
            }
                (merge skill-data {
                    verified: (>= updated-verifiers u1),
                    verifier-count: updated-verifiers,
                })
            )
            (ok verification-id)
        )
    )
)

(define-public (endorse-volunteer-skill
        (volunteer principal)
        (skill-name (string-ascii 30))
    )
    (let (
            (endorser tx-sender)
            (skill-data (unwrap! (get-volunteer-skill volunteer skill-name)
                err-skill-not-found
            ))
        )
        (asserts! (is-some (get-volunteer-data endorser)) (err u108))
        (asserts! (not (is-eq endorser volunteer)) err-cannot-verify-own-skill)
        (asserts! (is-none (get-skill-endorsement volunteer skill-name endorser))
            err-already-endorsed
        )
        (let ((updated-endorsements (+ (get total-endorsements skill-data) u1)))
            (map-set skill-endorsements {
                volunteer: volunteer,
                skill-name: skill-name,
                endorser: endorser,
            } { endorsed: true }
            )
            (map-set volunteer-skills {
                volunteer: volunteer,
                skill-name: skill-name,
            }
                (merge skill-data { total-endorsements: updated-endorsements })
            )
            (ok true)
        )
    )
)

(define-read-only (get-volunteer-skill
        (volunteer principal)
        (skill-name (string-ascii 30))
    )
    (map-get? volunteer-skills {
        volunteer: volunteer,
        skill-name: skill-name,
    })
)

(define-read-only (get-skill-verification (id uint))
    (map-get? skill-verifications { id: id })
)

(define-read-only (get-skill-endorsement
        (volunteer principal)
        (skill-name (string-ascii 30))
        (endorser principal)
    )
    (map-get? skill-endorsements {
        volunteer: volunteer,
        skill-name: skill-name,
        endorser: endorser,
    })
)

(define-read-only (is-skill-verified
        (volunteer principal)
        (skill-name (string-ascii 30))
    )
    (match (get-volunteer-skill volunteer skill-name)
        skill-data (ok (get verified skill-data))
        (ok false)
    )
)

(define-read-only (get-volunteer-skill-score
        (volunteer principal)
        (skill-name (string-ascii 30))
    )
    (match (get-volunteer-skill volunteer skill-name)
        skill-data (ok (+ (* (get skill-level skill-data) u10)
            (* (get verifier-count skill-data) u5)
            (get total-endorsements skill-data)
        ))
        (ok u0)
    )
)

(define-constant err-insufficient-rewards (err u118))
(define-constant err-withdrawal-cooldown-active (err u119))
(define-constant err-exceeds-max-withdrawal (err u120))

(define-constant withdrawal-cooldown-blocks u144)

(define-data-var withdrawal-id-nonce uint u0)
(define-data-var total-rewards-withdrawn uint u0)
(define-data-var max-withdrawal-amount uint u1000)

(define-map volunteer-withdrawals
    { volunteer: principal }
    {
        total-withdrawn: uint,
        last-withdrawal-block: uint,
        withdrawal-count: uint,
        claimed-rewards: uint,
    }
)

(define-map withdrawal-history
    { id: uint }
    {
        volunteer: principal,
        amount: uint,
        timestamp: uint,
        remaining-balance: uint,
    }
)

(define-public (withdraw-rewards (amount uint))
    (let (
            (volunteer tx-sender)
            (v-data (unwrap! (get-volunteer-data volunteer) err-not-found))
            (withdrawal-data (default-to {
                total-withdrawn: u0,
                last-withdrawal-block: u0,
                withdrawal-count: u0,
                claimed-rewards: u0,
            }
                (map-get? volunteer-withdrawals { volunteer: volunteer })
            ))
            (available-rewards (- (get total-rewards v-data) (get claimed-rewards withdrawal-data)))
            (withdrawal-id (+ (var-get withdrawal-id-nonce) u1))
            (blocks-since-last (- stacks-block-height (get last-withdrawal-block withdrawal-data)))
        )
        (asserts! (> amount u0) err-invalid-hours)
        (asserts! (<= amount (var-get max-withdrawal-amount))
            err-exceeds-max-withdrawal
        )
        (asserts! (<= amount available-rewards) err-insufficient-rewards)
        (asserts!
            (or
                (is-eq (get last-withdrawal-block withdrawal-data) u0)
                (>= blocks-since-last withdrawal-cooldown-blocks)
            )
            err-withdrawal-cooldown-active
        )
        (let ((new-claimed (+ (get claimed-rewards withdrawal-data) amount)))
            (var-set withdrawal-id-nonce withdrawal-id)
            (var-set total-rewards-withdrawn
                (+ (var-get total-rewards-withdrawn) amount)
            )
            (map-set volunteer-withdrawals { volunteer: volunteer } {
                total-withdrawn: (+ (get total-withdrawn withdrawal-data) amount),
                last-withdrawal-block: stacks-block-height,
                withdrawal-count: (+ (get withdrawal-count withdrawal-data) u1),
                claimed-rewards: new-claimed,
            })
            (map-set withdrawal-history { id: withdrawal-id } {
                volunteer: volunteer,
                amount: amount,
                timestamp: stacks-block-height,
                remaining-balance: (- available-rewards amount),
            })
            (ok withdrawal-id)
        )
    )
)

(define-public (update-max-withdrawal (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set max-withdrawal-amount new-max)
        (ok true)
    )
)

(define-read-only (get-withdrawal-data (volunteer principal))
    (map-get? volunteer-withdrawals { volunteer: volunteer })
)

(define-read-only (get-withdrawal-history (id uint))
    (map-get? withdrawal-history { id: id })
)

(define-read-only (get-available-rewards (volunteer principal))
    (match (get-volunteer-data volunteer)
        v-data (let ((withdrawal-data (default-to {
                total-withdrawn: u0,
                last-withdrawal-block: u0,
                withdrawal-count: u0,
                claimed-rewards: u0,
            }
                (map-get? volunteer-withdrawals { volunteer: volunteer })
            )))
            (ok (- (get total-rewards v-data) (get claimed-rewards withdrawal-data)))
        )
        err-not-found
    )
)

(define-read-only (can-withdraw (volunteer principal))
    (match (get-volunteer-data volunteer)
        v-data (let (
                (withdrawal-data (default-to {
                    total-withdrawn: u0,
                    last-withdrawal-block: u0,
                    withdrawal-count: u0,
                    claimed-rewards: u0,
                }
                    (map-get? volunteer-withdrawals { volunteer: volunteer })
                ))
                (blocks-since-last (- stacks-block-height
                    (get last-withdrawal-block withdrawal-data)
                ))
            )
            (ok (or
                (is-eq (get last-withdrawal-block withdrawal-data) u0)
                (>= blocks-since-last withdrawal-cooldown-blocks)
            ))
        )
        (ok false)
    )
)

(define-read-only (get-blocks-until-next-withdrawal (volunteer principal))
    (match (get-withdrawal-data volunteer)
        withdrawal-data (let ((blocks-since-last (- stacks-block-height (get last-withdrawal-block withdrawal-data))))
            (ok (if (>= blocks-since-last withdrawal-cooldown-blocks)
                u0
                (- withdrawal-cooldown-blocks blocks-since-last)
            ))
        )
        (ok u0)
    )
)

(define-read-only (get-total-rewards-withdrawn)
    (ok (var-get total-rewards-withdrawn))
)

(define-read-only (get-withdrawal-limits)
    (ok {
        max-withdrawal: (var-get max-withdrawal-amount),
        cooldown-blocks: withdrawal-cooldown-blocks,
    })
)
