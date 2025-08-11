(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PROPOSAL (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_CLOSED (err u103))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u104))
(define-constant ERR_ALREADY_EXECUTED (err u105))
(define-constant ERR_INSUFFICIENT_FUNDS (err u106))
(define-constant ERR_NOT_MEMBER (err u107))
(define-constant ERR_PROPOSAL_ACTIVE (err u108))
(define-constant ERR_TIME_LOCK_ACTIVE (err u109))

(define-data-var proposal-counter uint u0)
(define-data-var treasury uint u0)
(define-data-var voting-period uint u1440)
(define-data-var min-votes-required uint u3)
(define-data-var time-lock-threshold uint u1000000)
(define-data-var time-lock-period uint u144)

(define-map members principal bool)
(define-map proposals 
  uint 
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    recipient: principal,
    proposer: principal,
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    time-locked-until: (optional uint)
  }
)

(define-map votes {proposal-id: uint, voter: principal} bool)
(define-map member-votes principal uint)
(define-map delegations {delegator: principal} {delegate: principal})
(define-map delegation-power principal uint)

(define-public (join-dao)
  (begin
    (map-set members tx-sender true)
    (map-set member-votes tx-sender u0)
    (map-set delegation-power tx-sender u1)
    (ok true)
  )
)

(define-public (leave-dao)
  (begin
    (map-delete members tx-sender)
    (map-delete member-votes tx-sender)
    (map-delete delegations {delegator: tx-sender})
    (map-delete delegation-power tx-sender)
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set treasury (+ (var-get treasury) amount))
    (ok true)
  )
)

(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (amount uint)
  (recipient principal)
)
  (let 
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (current-block stacks-block-height)
      (end-block (+ current-block (var-get voting-period)))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_PROPOSAL)
    (asserts! (<= amount (var-get treasury)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set proposals proposal-id
      {
        title: title,
        description: description,
        amount: amount,
        recipient: recipient,
        proposer: tx-sender,
        start-block: current-block,
        end-block: end-block,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        time-locked-until: none
      }
    )
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (current-block stacks-block-height)
      (vote-key {proposal-id: proposal-id, voter: tx-sender})
      (voting-power (default-to u1 (map-get? delegation-power tx-sender)))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    (asserts! (is-none (map-get? votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (< current-block (get end-block proposal)) ERR_VOTING_CLOSED)
    
    (map-set votes vote-key true)
    
    (if support
      (map-set proposals proposal-id 
        (merge proposal {votes-for: (+ (get votes-for proposal) voting-power)})
      )
      (map-set proposals proposal-id 
        (merge proposal {votes-against: (+ (get votes-against proposal) voting-power)})
      )
    )
    
    (map-set member-votes tx-sender 
      (+ (default-to u0 (map-get? member-votes tx-sender)) u1)
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (current-block stacks-block-height)
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    )
    (asserts! (>= current-block (get end-block proposal)) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
    (asserts! (>= total-votes (var-get min-votes-required)) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (<= (get amount proposal) (var-get treasury)) ERR_INSUFFICIENT_FUNDS)
    (asserts! 
      (match (get time-locked-until proposal)
        time-lock-block (>= current-block time-lock-block)
        true
      )
      ERR_TIME_LOCK_ACTIVE
    )
    
    (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
    (var-set treasury (- (var-get treasury) (get amount proposal)))
    
    (map-set proposals proposal-id (merge proposal {executed: true}))
    (ok true)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (update-min-votes (new-min uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set min-votes-required new-min)
    (ok true)
  )
)

(define-public (activate-time-lock (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (current-block stacks-block-height)
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (time-lock-until (+ current-block (var-get time-lock-period)))
    )
    (asserts! (>= current-block (get end-block proposal)) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
    (asserts! (>= total-votes (var-get min-votes-required)) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (>= (get amount proposal) (var-get time-lock-threshold)) ERR_INVALID_PROPOSAL)
    (asserts! (is-none (get time-locked-until proposal)) ERR_ALREADY_EXECUTED)
    
    (map-set proposals proposal-id 
      (merge proposal {time-locked-until: (some time-lock-until)})
    )
    (ok time-lock-until)
  )
)

(define-public (update-time-lock-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set time-lock-threshold new-threshold)
    (ok true)
  )
)

(define-public (update-time-lock-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set time-lock-period new-period)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get treasury)) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set treasury (- (var-get treasury) amount))
    (ok true)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (is-member (user principal))
  (default-to false (map-get? members user))
)

(define-read-only (get-treasury-balance)
  (var-get treasury)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-min-votes-required)
  (var-get min-votes-required)
)

(define-read-only (get-time-lock-threshold)
  (var-get time-lock-threshold)
)

(define-read-only (get-time-lock-period)
  (var-get time-lock-period)
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-member-vote-count (member principal))
  (default-to u0 (map-get? member-votes member))
)

(define-public (delegate-vote (delegate principal))
  (let 
    (
      (current-power (default-to u1 (map-get? delegation-power tx-sender)))
      (delegate-power (default-to u1 (map-get? delegation-power delegate)))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    (asserts! (default-to false (map-get? members delegate)) ERR_NOT_MEMBER)
    (asserts! (not (is-eq tx-sender delegate)) ERR_INVALID_PROPOSAL)
    
    (map-set delegations {delegator: tx-sender} {delegate: delegate})
    (map-set delegation-power delegate (+ delegate-power current-power))
    (map-set delegation-power tx-sender u0)
    (ok true)
  )
)

(define-public (revoke-delegation)
  (let 
    (
      (delegation-info (unwrap! (map-get? delegations {delegator: tx-sender}) ERR_INVALID_PROPOSAL))
      (delegate (get delegate delegation-info))
      (current-power (default-to u1 (map-get? delegation-power delegate)))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    
    (map-delete delegations {delegator: tx-sender})
    (map-set delegation-power delegate (- current-power u1))
    (map-set delegation-power tx-sender u1)
    (ok true)
  )
)

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations {delegator: delegator})
)

(define-read-only (get-voting-power (member principal))
  (default-to u0 (map-get? delegation-power member))
)

(define-read-only (is-proposal-passed (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal 
    (let 
      (
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      )
      (and 
        (>= current-block (get end-block proposal))
        (>= total-votes (var-get min-votes-required))
        (> (get votes-for proposal) (get votes-against proposal))
      )
    )
    false
  )
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let 
      (
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (is-active (< current-block (get end-block proposal)))
        (has-min-votes (>= total-votes (var-get min-votes-required)))
        (is-approved (> (get votes-for proposal) (get votes-against proposal)))
      )
      {
        active: is-active,
        passed: (and (not is-active) has-min-votes is-approved),
        executed: (get executed proposal),
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        total-votes: total-votes,
        blocks-remaining: (if is-active (- (get end-block proposal) current-block) u0)
      }
    )
    {
      active: false,
      passed: false,
      executed: false,
      votes-for: u0,
      votes-against: u0,
      total-votes: u0,
      blocks-remaining: u0
    }
  )
)
