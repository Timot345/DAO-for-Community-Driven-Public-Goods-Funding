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
(define-constant ERR_INVALID_MILESTONE (err u110))
(define-constant ERR_MILESTONE_NOT_READY (err u111))
(define-constant ERR_MILESTONE_COMPLETED (err u112))
(define-constant ERR_NOT_RECIPIENT (err u113))

(define-data-var proposal-counter uint u0)
(define-data-var treasury uint u0)
(define-data-var voting-period uint u1440)
(define-data-var min-votes-required uint u3)
(define-data-var time-lock-threshold uint u1000000)
(define-data-var time-lock-period uint u144)
(define-data-var milestone-counter uint u0)
(define-data-var milestone-voting-period uint u720)

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
(define-map reputation-stats principal {total-proposals: uint, successful-proposals: uint})

(define-map proposal-milestones {proposal-id: uint, milestone-index: uint} {
  description: (string-ascii 200),
  amount: uint,
  completed: bool,
  verification-votes-for: uint,
  verification-votes-against: uint,
  verification-end-block: (optional uint),
  released: bool,
  evidence-hash: (optional (string-ascii 64))
})

(define-map proposal-milestone-count uint uint)

(define-map milestone-verification-votes {proposal-id: uint, milestone-index: uint, voter: principal} bool)

(define-map milestone-proposals uint bool)

(define-public (join-dao)
  (begin
    (map-set members tx-sender true)
    (map-set member-votes tx-sender u0)
    (map-set delegation-power tx-sender u1)
    (map-set reputation-stats tx-sender {total-proposals: u0, successful-proposals: u0})
    (ok true)
  )
)

(define-public (leave-dao)
  (begin
    (map-delete members tx-sender)
    (map-delete member-votes tx-sender)
    (map-delete delegations {delegator: tx-sender})
    (map-delete delegation-power tx-sender)
    (map-delete reputation-stats tx-sender)
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
    (let ((current-stats (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats tx-sender))))
      (map-set reputation-stats tx-sender 
        (merge current-stats {total-proposals: (+ (get total-proposals current-stats) u1)})
      )
    )
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
      (proposer-stats (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats (get proposer proposal))))
      (reputation-score (if (> (get total-proposals proposer-stats) u0) 
                         (/ (* (get successful-proposals proposer-stats) u100) (get total-proposals proposer-stats))
                         u50))
      (adjusted-min-votes (if (>= reputation-score u75)
                           (/ (var-get min-votes-required) u2)
                           (if (< reputation-score u25)
                             (* (var-get min-votes-required) u2)
                             (var-get min-votes-required))))
    )
    (asserts! (>= current-block (get end-block proposal)) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
    (asserts! (>= total-votes adjusted-min-votes) ERR_PROPOSAL_NOT_PASSED)
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
    
    (let ((proposer-rep-stats (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats (get proposer proposal)))))
      (map-set reputation-stats (get proposer proposal)
        (merge proposer-rep-stats {successful-proposals: (+ (get successful-proposals proposer-rep-stats) u1)})
      )
    )
    
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

(define-read-only (get-reputation-stats (member principal))
  (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats member))
)

(define-read-only (calculate-reputation-score (member principal))
  (let 
    (
      (stats (get-reputation-stats member))
      (total (get total-proposals stats))
      (successful (get successful-proposals stats))
    )
    (if (> total u0) 
      (/ (* successful u100) total)
      u50
    )
  )
)

(define-read-only (get-adjusted-min-votes (proposer principal))
  (let 
    (
      (reputation-score (calculate-reputation-score proposer))
    )
    (if (>= reputation-score u75)
      (/ (var-get min-votes-required) u2)
      (if (< reputation-score u25)
        (* (var-get min-votes-required) u2)
        (var-get min-votes-required)
      )
    )
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
        (adjusted-min-votes (get-adjusted-min-votes (get proposer proposal)))
        (has-min-votes (>= total-votes adjusted-min-votes))
        (is-approved (> (get votes-for proposal) (get votes-against proposal)))
      )
      {
        active: is-active,
        passed: (and (not is-active) has-min-votes is-approved),
        executed: (get executed proposal),
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        total-votes: total-votes,
        adjusted-min-votes: adjusted-min-votes,
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
      adjusted-min-votes: (var-get min-votes-required),
      blocks-remaining: u0
    }
  )
)

(define-public (create-milestone-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (recipient principal)
  (milestone-descriptions (list 10 (string-ascii 200)))
  (milestone-amounts (list 10 uint))
)
  (let 
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (current-block stacks-block-height)
      (end-block (+ current-block (var-get voting-period)))
      (total-amount (fold + milestone-amounts u0))
      (milestone-count (len milestone-amounts))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    (asserts! (> total-amount u0) ERR_INVALID_PROPOSAL)
    (asserts! (<= total-amount (var-get treasury)) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-eq (len milestone-descriptions) milestone-count) ERR_INVALID_MILESTONE)
    (asserts! (and (> milestone-count u0) (<= milestone-count u10)) ERR_INVALID_MILESTONE)
    
    (map-set proposals proposal-id
      {
        title: title,
        description: description,
        amount: total-amount,
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
    (map-set milestone-proposals proposal-id true)
    (map-set proposal-milestone-count proposal-id milestone-count)
    (fold setup-milestone-item 
      (list 
        {desc: (default-to "" (element-at milestone-descriptions u0)), amt: (default-to u0 (element-at milestone-amounts u0)), idx: u0}
        {desc: (default-to "" (element-at milestone-descriptions u1)), amt: (default-to u0 (element-at milestone-amounts u1)), idx: u1}
        {desc: (default-to "" (element-at milestone-descriptions u2)), amt: (default-to u0 (element-at milestone-amounts u2)), idx: u2}
        {desc: (default-to "" (element-at milestone-descriptions u3)), amt: (default-to u0 (element-at milestone-amounts u3)), idx: u3}
        {desc: (default-to "" (element-at milestone-descriptions u4)), amt: (default-to u0 (element-at milestone-amounts u4)), idx: u4}
        {desc: (default-to "" (element-at milestone-descriptions u5)), amt: (default-to u0 (element-at milestone-amounts u5)), idx: u5}
        {desc: (default-to "" (element-at milestone-descriptions u6)), amt: (default-to u0 (element-at milestone-amounts u6)), idx: u6}
        {desc: (default-to "" (element-at milestone-descriptions u7)), amt: (default-to u0 (element-at milestone-amounts u7)), idx: u7}
        {desc: (default-to "" (element-at milestone-descriptions u8)), amt: (default-to u0 (element-at milestone-amounts u8)), idx: u8}
        {desc: (default-to "" (element-at milestone-descriptions u9)), amt: (default-to u0 (element-at milestone-amounts u9)), idx: u9}
      )
      {proposal-id: proposal-id, milestone-count: milestone-count}
    )
    (var-set proposal-counter proposal-id)
    (let ((current-stats (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats tx-sender))))
      (map-set reputation-stats tx-sender 
        (merge current-stats {total-proposals: (+ (get total-proposals current-stats) u1)})
      )
    )
    (ok proposal-id)
  )
)

(define-private (setup-milestone-item
  (item {desc: (string-ascii 200), amt: uint, idx: uint})
  (context {proposal-id: uint, milestone-count: uint})
)
  (if (< (get idx item) (get milestone-count context))
    (begin
      (map-set proposal-milestones {proposal-id: (get proposal-id context), milestone-index: (get idx item)}
        {
          description: (get desc item),
          amount: (get amt item),
          completed: false,
          verification-votes-for: u0,
          verification-votes-against: u0,
          verification-end-block: none,
          released: false,
          evidence-hash: none
        }
      )
      context
    )
    context
  )
)

(define-public (submit-milestone-completion
  (proposal-id uint)
  (milestone-index uint)
  (evidence-hash (string-ascii 64))
)
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (milestone (unwrap! (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}) ERR_INVALID_MILESTONE))
      (current-block stacks-block-height)
      (verification-end (+ current-block (var-get milestone-voting-period)))
      (is-milestone-proposal (default-to false (map-get? milestone-proposals proposal-id)))
    )
    (asserts! is-milestone-proposal ERR_INVALID_MILESTONE)
    (asserts! (is-eq tx-sender (get recipient proposal)) ERR_NOT_RECIPIENT)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
    (asserts! (> (len evidence-hash) u0) ERR_INVALID_PROPOSAL)
    (asserts! (if (> milestone-index u0)
                (get released (unwrap! (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: (- milestone-index u1)}) ERR_INVALID_MILESTONE))
                true) ERR_MILESTONE_NOT_READY)
    
    (map-set proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}
      (merge milestone {
        evidence-hash: (some evidence-hash),
        verification-end-block: (some verification-end)
      })
    )
    (ok verification-end)
  )
)

(define-public (vote-milestone-verification
  (proposal-id uint)
  (milestone-index uint)
  (approve bool)
)
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (milestone (unwrap! (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}) ERR_INVALID_MILESTONE))
      (vote-key {proposal-id: proposal-id, milestone-index: milestone-index, voter: tx-sender})
      (current-block stacks-block-height)
      (verification-end (unwrap! (get verification-end-block milestone) ERR_MILESTONE_NOT_READY))
      (voting-power (default-to u1 (map-get? delegation-power tx-sender)))
    )
    (asserts! (default-to false (map-get? members tx-sender)) ERR_NOT_MEMBER)
    (asserts! (is-none (map-get? milestone-verification-votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (< current-block verification-end) ERR_VOTING_CLOSED)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
    
    (map-set milestone-verification-votes vote-key true)
    
    (if approve
      (map-set proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}
        (merge milestone {verification-votes-for: (+ (get verification-votes-for milestone) voting-power)})
      )
      (map-set proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}
        (merge milestone {verification-votes-against: (+ (get verification-votes-against milestone) voting-power)})
      )
    )
    (ok true)
  )
)

(define-public (release-milestone-funds
  (proposal-id uint)
  (milestone-index uint)
)
  (let 
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
      (milestone (unwrap! (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}) ERR_INVALID_MILESTONE))
      (current-block stacks-block-height)
      (verification-end (unwrap! (get verification-end-block milestone) ERR_MILESTONE_NOT_READY))
      (total-votes (+ (get verification-votes-for milestone) (get verification-votes-against milestone)))
      (is-approved (and (>= total-votes (var-get min-votes-required))
                       (> (get verification-votes-for milestone) (get verification-votes-against milestone))))
    )
    (asserts! (>= current-block verification-end) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get released milestone)) ERR_ALREADY_EXECUTED)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
    (asserts! is-approved ERR_PROPOSAL_NOT_PASSED)
    (asserts! (<= (get amount milestone) (var-get treasury)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get recipient proposal))))
    (var-set treasury (- (var-get treasury) (get amount milestone)))
    
    (map-set proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index}
      (merge milestone {completed: true, released: true})
    )
    
    (let ((milestone-count (default-to u0 (map-get? proposal-milestone-count proposal-id)))
          (is-last-milestone (is-eq (+ milestone-index u1) milestone-count)))
      (if is-last-milestone
        (begin
          (map-set proposals proposal-id (merge proposal {executed: true}))
          (let ((proposer-rep-stats (default-to {total-proposals: u0, successful-proposals: u0} (map-get? reputation-stats (get proposer proposal)))))
            (map-set reputation-stats (get proposer proposal)
              (merge proposer-rep-stats {successful-proposals: (+ (get successful-proposals proposer-rep-stats) u1)})
            )
          )
          (ok true)
        )
        (ok true)
      )
    )
  )
)

(define-read-only (get-milestone
  (proposal-id uint)
  (milestone-index uint)
)
  (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index})
)

(define-read-only (get-milestone-count (proposal-id uint))
  (default-to u0 (map-get? proposal-milestone-count proposal-id))
)

(define-read-only (is-milestone-proposal (proposal-id uint))
  (default-to false (map-get? milestone-proposals proposal-id))
)

(define-read-only (get-milestone-verification-vote
  (proposal-id uint)
  (milestone-index uint)
  (voter principal)
)
  (map-get? milestone-verification-votes {proposal-id: proposal-id, milestone-index: milestone-index, voter: voter})
)

(define-read-only (get-milestone-status
  (proposal-id uint)
  (milestone-index uint)
)
  (match (map-get? proposal-milestones {proposal-id: proposal-id, milestone-index: milestone-index})
    milestone
    (let 
      (
        (current-block stacks-block-height)
        (total-votes (+ (get verification-votes-for milestone) (get verification-votes-against milestone)))
        (is-approved (and (>= total-votes (var-get min-votes-required))
                         (> (get verification-votes-for milestone) (get verification-votes-against milestone))))
        (is-voting-active (match (get verification-end-block milestone)
                            end-block (< current-block end-block)
                            false))
      )
      (some {
        completed: (get completed milestone),
        released: (get released milestone),
        verification-active: is-voting-active,
        verification-approved: is-approved,
        votes-for: (get verification-votes-for milestone),
        votes-against: (get verification-votes-against milestone),
        has-evidence: (is-some (get evidence-hash milestone))
      })
    )
    none
  )
)

(define-read-only (get-proposal-milestone-progress (proposal-id uint))
  (let 
    (
      (milestone-count (get-milestone-count proposal-id))
      (is-milestone-prop (is-milestone-proposal proposal-id))
    )
    (if is-milestone-prop
      (let 
        (
          (completed-result (fold count-milestone-status
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
            {proposal-id: proposal-id, total: milestone-count, completed: u0, released: u0}
          ))
        )
        (some {
          total-milestones: milestone-count,
          milestones-completed: (get completed completed-result),
          milestones-released: (get released completed-result)
        })
      )
      none
    )
  )
)

(define-private (count-milestone-status
  (index uint)
  (acc {proposal-id: uint, total: uint, completed: uint, released: uint})
)
  (if (< index (get total acc))
    (match (map-get? proposal-milestones {proposal-id: (get proposal-id acc), milestone-index: index})
      milestone
      (merge acc {
        completed: (if (get completed milestone) (+ (get completed acc) u1) (get completed acc)),
        released: (if (get released milestone) (+ (get released acc) u1) (get released acc))
      })
      acc
    )
    acc
  )
)
