;; Research Grant Management Contract
;; Handles grant proposals, approvals, fund distributions, and reporting

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-APPROVED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-MILESTONE-NOT-FOUND (err u106))
(define-constant ERR-INVALID-INPUT (err u107))
(define-constant ERR-INVALID-LENGTH (err u108))
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-TITLE-LENGTH u100)
(define-constant MAX-MILESTONE-DESC-LENGTH u200)
(define-constant MIN-AMOUNT u1000000) ;; Minimum amount in microSTX
(define-constant MAX-AMOUNT u1000000000000) ;; Maximum amount in microSTX

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-grants uint u0)
(define-data-var treasury-balance uint u0)

;; Define proposal status values
(define-constant STATUS-PENDING u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-COMPLETED u4)

;; Data Maps
(define-map Proposals
    uint 
    {
        researcher: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        amount: uint,
        status: uint,
        approved-by: (optional principal),
        created-at: uint,
        milestones: uint,
        completed-milestones: uint
    }
)

(define-map Milestones
    {proposal-id: uint, milestone-id: uint}
    {
        description: (string-ascii 200),
        amount: uint,
        deadline: uint,
        completed: bool,
        verified: bool
    }
)

(define-map ResearcherStats
    principal
    {
        total-grants: uint,
        total-amount: uint,
        successful-completions: uint
    }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

(define-private (validate-amount (amount uint))
    (and 
        (>= amount MIN-AMOUNT)
        (<= amount MAX-AMOUNT)
    )
)

(define-private (validate-string-length (str (string-ascii 500)) (max-len uint))
    (<= (len str) max-len)
)

(define-private (validate-proposal-id (proposal-id uint))
    (and 
        (> proposal-id u0)
        (<= proposal-id (var-get total-grants))
    )
)

(define-private (validate-milestone-id (milestone-id uint) (total-milestones uint))
    (and 
        (> milestone-id u0)
        (<= milestone-id total-milestones)
    )
)

(define-private (update-researcher-stats (researcher principal) (amount uint))
    (let (
        (current-stats (default-to 
            {total-grants: u0, total-amount: u0, successful-completions: u0}
            (map-get? ResearcherStats researcher)
        ))
    )
    (map-set ResearcherStats
        researcher
        {
            total-grants: (+ (get total-grants current-stats) u1),
            total-amount: (+ (get total-amount current-stats) amount),
            successful-completions: (get successful-completions current-stats)
        }
    )
    )
)

;; Public Functions

;; Initialize treasury with validation
(define-public (initialize-treasury (amount uint))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
        (var-set treasury-balance amount)
        (ok amount)
    )
)

;; Submit new grant proposal with validation
(define-public (submit-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (amount uint)
    (milestones uint)
)
    (let (
        (proposal-id (+ (var-get total-grants) u1))
    )
        (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
        (asserts! (> milestones u0) ERR-INVALID-AMOUNT)
        (asserts! (validate-string-length title MAX-TITLE-LENGTH) ERR-INVALID-LENGTH)
        (asserts! (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-LENGTH)
        
        (map-set Proposals proposal-id
            {
                researcher: tx-sender,
                title: title,
                description: description,
                amount: amount,
                status: STATUS-PENDING,
                approved-by: none,
                created-at: block-height,
                milestones: milestones,
                completed-milestones: u0
            }
        )
        (var-set total-grants proposal-id)
        (ok proposal-id)
    )
)

;; Add milestone to proposal with validation
(define-public (add-milestone
    (proposal-id uint)
    (milestone-id uint)
    (description (string-ascii 200))
    (amount uint)
    (deadline uint)
)
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-INPUT)
        (asserts! (validate-milestone-id milestone-id (get milestones proposal)) ERR-INVALID-INPUT)
        (asserts! (validate-string-length description MAX-MILESTONE-DESC-LENGTH) ERR-INVALID-LENGTH)
        (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
        (asserts! (> deadline block-height) ERR-INVALID-INPUT)
        (asserts! (is-eq tx-sender (get researcher proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status proposal) STATUS-PENDING) ERR-INVALID-STATUS)
        
        (map-set Milestones
            {proposal-id: proposal-id, milestone-id: milestone-id}
            {
                description: description,
                amount: amount,
                deadline: deadline,
                completed: false,
                verified: false
            }
        )
        (ok true)
    )
)

;; Complete milestone with validation
(define-public (complete-milestone (proposal-id uint) (milestone-id uint))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-INPUT)
        (asserts! (validate-milestone-id milestone-id (get milestones proposal)) ERR-INVALID-INPUT)
        (asserts! (is-eq tx-sender (get researcher proposal)) ERR-NOT-AUTHORIZED)
        
        (let (
            (milestone (unwrap! (map-get? Milestones 
                {proposal-id: proposal-id, milestone-id: milestone-id}
            ) ERR-MILESTONE-NOT-FOUND))
        )
            (asserts! (is-eq (get status proposal) STATUS-APPROVED) ERR-INVALID-STATUS)
            (asserts! (not (get completed milestone)) ERR-INVALID-STATUS)
            
            (map-set Milestones
                {proposal-id: proposal-id, milestone-id: milestone-id}
                (merge milestone {completed: true})
            )
            
            (map-set Proposals proposal-id
                (merge proposal {
                    completed-milestones: (+ (get completed-milestones proposal) u1)
                })
            )
            
            (if (is-eq (+ (get completed-milestones proposal) u1) (get milestones proposal))
                (map-set Proposals proposal-id
                    (merge proposal {
                        status: STATUS-COMPLETED,
                        completed-milestones: (+ (get completed-milestones proposal) u1)
                    })
                )
                true
            )
            (ok true)
        )
    )
)

;; Verify milestone with validation
(define-public (verify-milestone (proposal-id uint) (milestone-id uint))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-INPUT)
        (asserts! (validate-milestone-id milestone-id (get milestones proposal)) ERR-INVALID-INPUT)
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        
        (let (
            (milestone (unwrap! (map-get? Milestones 
                {proposal-id: proposal-id, milestone-id: milestone-id}
            ) ERR-MILESTONE-NOT-FOUND))
        )
            (asserts! (get completed milestone) ERR-INVALID-STATUS)
            (asserts! (not (get verified milestone)) ERR-INVALID-STATUS)
            
            (map-set Milestones
                {proposal-id: proposal-id, milestone-id: milestone-id}
                (merge milestone {verified: true})
            )
            (ok true)
        )
    )
)

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? Proposals proposal-id)
)

;; Get milestone details
(define-read-only (get-milestone (proposal-id uint) (milestone-id uint))
    (map-get? Milestones {proposal-id: proposal-id, milestone-id: milestone-id})
)

;; Get researcher stats
(define-read-only (get-researcher-stats (researcher principal))
    (map-get? ResearcherStats researcher)
)

;; Get treasury balance
(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)
