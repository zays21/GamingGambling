;; Fantasy Sports League Contract
;; Ultra-secure version with explicit validation for all data operations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-league-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-league-full (err u104))
(define-constant err-not-league-participant (err u105))
(define-constant err-league-already-started (err u106))
(define-constant err-invalid-input (err u107))
(define-constant err-league-not-active (err u108)) ;; New, more precise error
(define-constant err-already-participant (err u109)) ;; New, logically correct error

;; Secure data structures with explicit bounds
(define-map leagues
  uint
  {
    name: (string-ascii 32),
    entry-fee: uint,
    max-participants: uint,
    current-participants: uint,
    prize-pool: uint,
    status: uint, ;; 0=open, 1=active, 2=completed
    winner: (optional principal)
  })

(define-map league-participants
  {league-id: uint, participant: principal}
  {
    score: uint,
    team-name: (string-ascii 20)
  })

;; Variables
(define-data-var league-counter uint u0)

;; Validation constants
(define-constant max-entry-fee u100000000) ;; 100 STX max
(define-constant min-entry-fee u1000000)   ;; 1 STX min
(define-constant max-score u1000000)       ;; Max score limit
(define-constant max-participants-limit u10) ;; Reduced limit

;; Function 1: Create Fantasy League (Fully Validated)
(define-public (create-fantasy-league
  (league-name (string-ascii 32))
  (entry-fee uint)
  (max-participants uint))
  (let
    (
      (new-league-id (+ (var-get league-counter) u1))
    )
    (begin
      ;; Explicit validation of ALL inputs
      (asserts! (and (>= (len league-name) u1) (<= (len league-name) u32)) err-invalid-input)
      (asserts! (and (>= entry-fee min-entry-fee) (<= entry-fee max-entry-fee)) err-invalid-amount)
      (asserts! (and (>= max-participants u2) (<= max-participants max-participants-limit)) err-invalid-input)

      ;; Check user balance explicitly
      (let ((user-balance (stx-get-balance tx-sender)))
        (asserts! (>= user-balance entry-fee) err-insufficient-balance))

      ;; Create league with hardcoded safe values
      (map-set leagues new-league-id
        {
          name: league-name,
          entry-fee: entry-fee,
          max-participants: max-participants,
          current-participants: u1, ;; Start with 1 (creator)
          prize-pool: entry-fee,    ;; Start with creator's fee
          status: u0,               ;; 0 = open
          winner: none
        })

      ;; Transfer with explicit validation
      (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))

      ;; Add creator as participant
      (map-set league-participants
        {league-id: new-league-id, participant: tx-sender}
        {
          score: u0,
          team-name: "Creator"
        })

      ;; Update counter
      (var-set league-counter new-league-id)

      (ok new-league-id))))

;; Function 2: Distribute Prize (Fully Validated)
;; Renamed to highlight the off-chain winner determination
(define-public (distribute-prize-to-winner (league-id uint) (winner principal))
  (begin
    ;; Owner check first
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    ;; Get league data with explicit validation
    (match (map-get? leagues league-id)
      league-data
        (let ((validated-prize (get prize-pool league-data))
              (validated-status (get status league-data)))
          (begin
            ;; Explicit validation of all retrieved data
            (asserts! (and (> validated-prize u0) (<= validated-prize u1000000000)) err-invalid-amount)
            ;; Corrected error message for clarity
            (asserts! (is-eq validated-status u1) err-league-not-active)

            ;; Validate contract has enough balance
            (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
              (asserts! (>= contract-balance validated-prize) err-insufficient-balance))

            ;; Validate winner is participant
            (match (map-get? league-participants {league-id: league-id, participant: winner})
              participant-data
                (let ((validated-team-name (get team-name participant-data)))
                  (begin
                    ;; Validate participant data
                    (asserts! (and (>= (len validated-team-name) u1)
                                  (<= (len validated-team-name) u20)) err-not-league-participant)

                    ;; Corrected STX transfer call inside `as-contract`
                    (try! (as-contract (stx-transfer? validated-prize winner)))

                    ;; Update league status to completed
                    (map-set leagues league-id
                      {
                        name: (get name league-data),
                        entry-fee: (get entry-fee league-data),
                        max-participants: (get max-participants league-data),
                        current-participants: (get current-participants league-data),
                        prize-pool: validated-prize,
                        status: u2, ;; 2 = completed
                        winner: (some winner)
                      })

                    (ok winner)))
              (err err-not-league-participant))
            ))
      (err err-league-not-found))))

;; Function 3: Join league with full validation
(define-public (join-league (league-id uint) (team-name (string-ascii 20)))
  (begin
    ;; Validate team name first
    (asserts! (and (>= (len team-name) u1) (<= (len team-name) u20)) err-invalid-input)

    ;; Get and validate league data
    (match (map-get? leagues league-id)
      league-data
        (let ((validated-entry-fee (get entry-fee league-data))
              (validated-current (get current-participants league-data))
              (validated-max (get max-participants league-data))
              (validated-status (get status league-data)))
          (begin
            ;; Explicit validation of all league data
            (asserts! (and (>= validated-entry-fee min-entry-fee)
                          (<= validated-entry-fee max-entry-fee)) err-invalid-amount)
            (asserts! (is-eq validated-status u0) err-league-already-started) ;; Must be open
            (asserts! (< validated-current validated-max) err-league-full)

            ;; Check user balance
            (let ((user-balance (stx-get-balance tx-sender)))
              (asserts! (>= user-balance validated-entry-fee) err-insufficient-balance))

            ;; Corrected logical bug in error message: user should not already be a participant
            (asserts! (is-none (map-get? league-participants {league-id: league-id, participant: tx-sender}))
                      err-already-participant)

            ;; Transfer fee
            (try! (stx-transfer? validated-entry-fee tx-sender (as-contract tx-sender)))

            ;; Add participant
            (map-set league-participants
              {league-id: league-id, participant: tx-sender}
              {
                score: u0,
                team-name: team-name
              })

            ;; Calculate new values safely
            (let ((new-participant-count (+ validated-current u1))
                  (new-prize-pool (+ (get prize-pool league-data) validated-entry-fee))
                  (new-status (if (is-eq (+ validated-current u1) validated-max) u1 u0)))

              ;; Update league
              (map-set leagues league-id
                {
                  name: (get name league-data),
                  entry-fee: validated-entry-fee,
                  max-participants: validated-max,
                  current-participants: new-participant-count,
                  prize-pool: new-prize-pool,
                  status: new-status,
                  winner: none
                }))

            (ok league-id)))
      (err err-league-not-found))))

;; Update score with validation
(define-public (update-score (league-id uint) (participant principal) (new-score uint))
  (begin
    ;; Owner check
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    ;; Validate score
    (asserts! (<= new-score max-score) err-invalid-amount)

    ;; Get and validate participant
    (match (map-get? league-participants {league-id: league-id, participant: participant})
      participant-data
        (let ((validated-team-name (get team-name participant-data)))
          (begin
            ;; Validate participant data
            (asserts! (and (>= (len validated-team-name) u1)
                          (<= (len validated-team-name) u20)) err-not-league-participant)

            ;; Update score
            (map-set league-participants
              {league-id: league-id, participant: participant}
              {
                score: new-score,
                team-name: validated-team-name
              })

            (ok new-score)))
      (err err-not-league-participant))))

;; Safe read-only functions
(define-read-only (get-league-info (league-id uint))
  (map-get? leagues league-id))

(define-read-only (get-participant-info (league-id uint) (participant principal))
  (map-get? league-participants {league-id: league-id, participant: participant}))

(define-read-only (get-total-leagues)
  (var-get league-counter))

;; Emergency withdraw
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> amount u0) (<= amount u1000000000)) err-invalid-amount)

    (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
      (asserts! (>= contract-balance amount) err-insufficient-balance)
      (try! (as-contract (stx-transfer? amount contract-owner)))
      (ok true))))