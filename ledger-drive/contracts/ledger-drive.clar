;; LedgerDrive Protocol - Clarity Smart Contract v2.0
;; Cross-chain DeFi yield optimization protocol

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-withdrawal-failed (err u105))
(define-constant err-transfer-failed (err u106))

;; Data Variables
(define-data-var protocol-fee-rate uint u25) ;; 0.25% represented as 25 basis points
(define-data-var total-value-locked uint u0)
(define-data-var vault-counter uint u0)
(define-data-var ledg-token-supply uint u1000000000000) ;; 1 billion LEDG tokens
(define-data-var driv-token-supply uint u500000000000) ;; 500 million DRIV tokens

;; Data Maps
(define-map vaults
  { vault-id: uint }
  {
    owner: principal,
    deposited-amount: uint,
    ytoken-balance: uint,
    yield-earned: uint,
    strategy: (string-ascii 50),
    is-active: bool,
    created-at: uint
  }
)

(define-map user-vault-ids
  { user: principal }
  { vault-ids: (list 100 uint) }
)

(define-map ledg-balances
  { holder: principal }
  { balance: uint }
)

(define-map driv-balances
  { holder: principal }
  { balance: uint }
)

(define-map ytoken-balances
  { holder: principal }
  { balance: uint }
)

(define-map ledg-staking
  { staker: principal }
  { 
    staked-amount: uint,
    rewards-earned: uint,
    staked-at: uint
  }
)

;; Private Functions
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get protocol-fee-rate)) u10000)
)

(define-private (calculate-ytoken-amount (deposit-amount uint))
  ;; Simplified 1:1 minting for initial deposits
  ;; In production, would factor in vault performance
  deposit-amount
)

(define-private (add-vault-to-user (user principal) (vault-id uint))
  (let
    (
      (current-vaults (default-to 
        { vault-ids: (list) }
        (map-get? user-vault-ids { user: user })
      ))
    )
    (map-set user-vault-ids
      { user: user }
      { vault-ids: (unwrap-panic (as-max-len? 
        (append (get vault-ids current-vaults) vault-id) 
        u100)) }
    )
  )
)

;; Public Functions - Vault Management

(define-public (create-vault (initial-deposit uint) (strategy (string-ascii 50)))
  (let
    (
      (vault-id (+ (var-get vault-counter) u1))
      (fee (calculate-fee initial-deposit))
      (net-deposit (- initial-deposit fee))
      (ytoken-amount (calculate-ytoken-amount net-deposit))
    )
    (asserts! (> initial-deposit u0) err-invalid-amount)
    
    ;; Create vault
    (map-set vaults
      { vault-id: vault-id }
      {
        owner: tx-sender,
        deposited-amount: net-deposit,
        ytoken-balance: ytoken-amount,
        yield-earned: u0,
        strategy: strategy,
        is-active: true,
        created-at: block-height
      }
    )
    
    ;; Mint yTokens to user
    (map-set ytoken-balances
      { holder: tx-sender }
      { balance: (+ (get-ytoken-balance tx-sender) ytoken-amount) }
    )
    
    ;; Update protocol state
    (var-set vault-counter vault-id)
    (var-set total-value-locked (+ (var-get total-value-locked) net-deposit))
    (add-vault-to-user tx-sender vault-id)
    
    (ok vault-id)
  )
)

(define-public (deposit-to-vault (vault-id uint) (amount uint))
  (let
    (
      (vault (unwrap! (map-get? vaults { vault-id: vault-id }) err-vault-not-found))
      (fee (calculate-fee amount))
      (net-deposit (- amount fee))
      (ytoken-amount (calculate-ytoken-amount net-deposit))
    )
    (asserts! (is-eq tx-sender (get owner vault)) err-not-authorized)
    (asserts! (get is-active vault) err-vault-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update vault
    (map-set vaults
      { vault-id: vault-id }
      (merge vault {
        deposited-amount: (+ (get deposited-amount vault) net-deposit),
        ytoken-balance: (+ (get ytoken-balance vault) ytoken-amount)
      })
    )
    
    ;; Mint additional yTokens
    (map-set ytoken-balances
      { holder: tx-sender }
      { balance: (+ (get-ytoken-balance tx-sender) ytoken-amount) }
    )
    
    ;; Update TVL
    (var-set total-value-locked (+ (var-get total-value-locked) net-deposit))
    
    (ok true)
  )
)

(define-public (withdraw-from-vault (vault-id uint) (amount uint))
  (let
    (
      (vault (unwrap! (map-get? vaults { vault-id: vault-id }) err-vault-not-found))
      (user-ytokens (get-ytoken-balance tx-sender))
    )
    (asserts! (is-eq tx-sender (get owner vault)) err-not-authorized)
    (asserts! (get is-active vault) err-vault-not-found)
    (asserts! (<= amount (get deposited-amount vault)) err-insufficient-balance)
    (asserts! (>= user-ytokens amount) err-insufficient-balance)
    
    ;; Update vault
    (map-set vaults
      { vault-id: vault-id }
      (merge vault {
        deposited-amount: (- (get deposited-amount vault) amount),
        ytoken-balance: (- (get ytoken-balance vault) amount)
      })
    )
    
    ;; Burn yTokens
    (map-set ytoken-balances
      { holder: tx-sender }
      { balance: (- user-ytokens amount) }
    )
    
    ;; Update TVL
    (var-set total-value-locked (- (var-get total-value-locked) amount))
    
    (ok amount)
  )
)

;; Public Functions - LEDG Token Management

(define-public (transfer-ledg (recipient principal) (amount uint))
  (let
    (
      (sender-balance (get-ledg-balance tx-sender))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    
    (map-set ledg-balances
      { holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    (map-set ledg-balances
      { holder: recipient }
      { balance: (+ (get-ledg-balance recipient) amount) }
    )
    
    (ok true)
  )
)

(define-public (stake-ledg (amount uint))
  (let
    (
      (sender-balance (get-ledg-balance tx-sender))
      (current-stake (default-to
        { staked-amount: u0, rewards-earned: u0, staked-at: u0 }
        (map-get? ledg-staking { staker: tx-sender })
      ))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    
    ;; Deduct from balance
    (map-set ledg-balances
      { holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    ;; Add to staking
    (map-set ledg-staking
      { staker: tx-sender }
      {
        staked-amount: (+ (get staked-amount current-stake) amount),
        rewards-earned: (get rewards-earned current-stake),
        staked-at: block-height
      }
    )
    
    (ok true)
  )
)

(define-public (unstake-ledg (amount uint))
  (let
    (
      (stake (unwrap! (map-get? ledg-staking { staker: tx-sender }) err-insufficient-balance))
    )
    (asserts! (>= (get staked-amount stake) amount) err-insufficient-balance)
    
    ;; Update staking
    (map-set ledg-staking
      { staker: tx-sender }
      (merge stake {
        staked-amount: (- (get staked-amount stake) amount)
      })
    )
    
    ;; Return to balance
    (map-set ledg-balances
      { holder: tx-sender }
      { balance: (+ (get-ledg-balance tx-sender) amount) }
    )
    
    (ok true)
  )
)

;; Public Functions - DRIV Token Management

(define-public (transfer-driv (recipient principal) (amount uint))
  (let
    (
      (sender-balance (get-driv-balance tx-sender))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    
    (map-set driv-balances
      { holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    (map-set driv-balances
      { holder: recipient }
      { balance: (+ (get-driv-balance recipient) amount) }
    )
    
    (ok true)
  )
)

;; Read-only Functions

(define-read-only (get-vault-info (vault-id uint))
  (map-get? vaults { vault-id: vault-id })
)

(define-read-only (get-user-vaults (user principal))
  (default-to 
    { vault-ids: (list) }
    (map-get? user-vault-ids { user: user })
  )
)

(define-read-only (get-ledg-balance (holder principal))
  (default-to u0 
    (get balance (map-get? ledg-balances { holder: holder }))
  )
)

(define-read-only (get-driv-balance (holder principal))
  (default-to u0 
    (get balance (map-get? driv-balances { holder: holder }))
  )
)

(define-read-only (get-ytoken-balance (holder principal))
  (default-to u0 
    (get balance (map-get? ytoken-balances { holder: holder }))
  )
)

(define-read-only (get-staking-info (staker principal))
  (map-get? ledg-staking { staker: staker })
)

(define-read-only (get-total-value-locked)
  (var-get total-value-locked)
)

(define-read-only (get-protocol-fee-rate)
  (var-get protocol-fee-rate)
)

(define-read-only (get-vault-count)
  (var-get vault-counter)
)

;; Admin Functions

(define-public (set-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set protocol-fee-rate new-fee)
    (ok true)
  )
)

(define-public (distribute-ledg (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set ledg-balances
      { holder: recipient }
      { balance: (+ (get-ledg-balance recipient) amount) }
    )
    (ok true)
  )
)

(define-public (distribute-driv (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set driv-balances
      { holder: recipient }
      { balance: (+ (get-driv-balance recipient) amount) }
    )
    (ok true)
  )
)
