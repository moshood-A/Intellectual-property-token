;; Intellectual Property Token Contract
;; Author: Claude
;; Version: 1.0
;; Description: A smart contract for tokenizing intellectual property rights

;; Constants and Token Configuration
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-id-not-found (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-registered (err u104))
(define-constant err-license-expired (err u105))
(define-constant err-invalid-royalty (err u106))
(define-constant err-zero-payment (err u107))

;; Data storage
(define-map ip-tokens
  { token-id: uint }
  {
    creator: principal,
    title: (string-ascii 256),
    description: (string-utf8 1024),
    creation-date: uint,
    metadata-url: (optional (string-ascii 256)),
    royalty-percentage: uint,
    token-uri: (string-ascii 256)
  }
)

;; Ownership mapping
(define-map token-owners
  { token-id: uint }
  { owner: principal }
)

;; Token counter
(define-data-var token-counter uint u0)

;; License details mapping
(define-map licenses
  { token-id: uint, licensee: principal }
  {
    start-time: uint,
    expiration: uint,
    license-type: (string-ascii 64),
    license-terms: (string-utf8 1024),
    active: bool
  }
)

;; Revenue tracking for royalties
(define-map creator-revenue
  { creator: principal }
  { amount: uint }
)

;; Royalty transactions history
(define-map royalty-history
  { token-id: uint, tx-id: uint }
  {
    from: principal,
    to: principal,
    amount: uint,
    timestamp: uint
  }
)

(define-data-var royalty-tx-counter uint u0)

;; Read-only functions

(define-read-only (get-token-counter)
  (var-get token-counter)
)

(define-read-only (get-token-info (token-id uint))
  (match (map-get? ip-tokens { token-id: token-id })
    token-info token-info
    (err err-token-id-not-found)
  )
)

(define-read-only (get-token-owner (token-id uint))
  (match (map-get? token-owners { token-id: token-id })
    ownership (ok (get owner ownership))
    (err err-token-id-not-found)
  )
)

(define-read-only (get-license-info (token-id uint) (licensee principal))
  (match (map-get? licenses { token-id: token-id, licensee: licensee })
    license-info (ok license-info)
    (err u108) ;; License not found
  )
)

(define-read-only (get-creator-revenue (creator principal))
  (default-to { amount: u0 } (map-get? creator-revenue { creator: creator }))
)

(define-read-only (is-license-valid (token-id uint) (licensee principal))
  (match (map-get? licenses { token-id: token-id, licensee: licensee })
    license-info 
      (and 
        (get active license-info)
        (> (get expiration license-info) block-height))
    false
  )
)

;; Public functions

;; Register new intellectual property
(define-public (register-ip 
                (title (string-ascii 256))
                (description (string-utf8 1024))
                (metadata-url (optional (string-ascii 256)))
                (royalty-percentage uint)
                (token-uri (string-ascii 256)))
  (let 
    (
      (token-id (var-get token-counter))
      (creation-date block-height)
    )
    
    ;; Validate royalty percentage (0-100%)
    (asserts! (<= royalty-percentage u100) (err err-invalid-royalty))
    
    ;; Store token info
    (map-set ip-tokens
      { token-id: token-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        creation-date: creation-date,
        metadata-url: metadata-url,
        royalty-percentage: royalty-percentage,
        token-uri: token-uri
      }
    )
    
    ;; Set ownership
    (map-set token-owners
      { token-id: token-id }
      { owner: tx-sender }
    )
    
    ;; Increment token counter
    (var-set token-counter (+ token-id u1))
    
    ;; Return token ID
    (ok token-id)
  )
)

;; Transfer ownership of IP token
(define-public (transfer-ip (token-id uint) (recipient principal))
  (let
    (
      (current-owner (unwrap! (get-token-owner token-id) (err err-token-id-not-found)))
    )
    
    ;; Check if sender is the token owner
    (asserts! (is-eq tx-sender current-owner) (err err-not-token-owner))
    
    ;; Update ownership
    (map-set token-owners
      { token-id: token-id }
      { owner: recipient }
    )
    
    (ok true)
  )
)

;; Update IP metadata
(define-public (update-metadata 
                (token-id uint)
                (title (string-ascii 256))
                (description (string-utf8 1024))
                (metadata-url (optional (string-ascii 256)))
                (token-uri (string-ascii 256)))
  (let
    (
      (token-info (unwrap! (map-get? ip-tokens { token-id: token-id }) (err err-token-id-not-found)))
      (current-owner (unwrap! (get-token-owner token-id) (err err-token-id-not-found)))
    )
    
    ;; Ensure only owner can update
    (asserts! (is-eq tx-sender current-owner) (err err-not-token-owner))
    
    ;; Update token info while preserving creation date, creator and royalty percentage
    (map-set ip-tokens
      { token-id: token-id }
      {
        creator: (get creator token-info),
        title: title,
        description: description,
        creation-date: (get creation-date token-info),
        metadata-url: metadata-url,
        royalty-percentage: (get royalty-percentage token-info),
        token-uri: token-uri
      }
    )
    
    (ok true)
  )
)

;; Update royalty percentage
(define-public (update-royalty (token-id uint) (royalty-percentage uint))
  (let
    (
      (token-info (unwrap! (map-get? ip-tokens { token-id: token-id }) (err err-token-id-not-found)))
    )
    
    ;; Ensure only creator can update royalty
    (asserts! (is-eq tx-sender (get creator token-info)) (err err-unauthorized))
    
    ;; Validate royalty percentage (0-100%)
    (asserts! (<= royalty-percentage u100) (err err-invalid-royalty))
    
    ;; Update royalty percentage while preserving other token info
    (map-set ip-tokens
      { token-id: token-id }
      {
        creator: (get creator token-info),
        title: (get title token-info),
        description: (get description token-info),
        creation-date: (get creation-date token-info),
        metadata-url: (get metadata-url token-info),
        royalty-percentage: royalty-percentage,
        token-uri: (get token-uri token-info)
      }
    )
    
    (ok true)
  )
)

;; Issue a license for the IP
(define-public (issue-license 
                (token-id uint)
                (licensee principal)
                (duration uint)
                (license-type (string-ascii 64))
                (license-terms (string-utf8 1024))
                (payment uint))
  (let
    (
      (token-info (unwrap! (map-get? ip-tokens { token-id: token-id }) (err err-token-id-not-found)))
      (current-owner (unwrap! (get-token-owner token-id) (err err-token-id-not-found)))
      (royalty-amount (* payment (/ (get royalty-percentage token-info) u100)))
      (owner-amount (- payment royalty-amount))
      (creator (get creator token-info))
      (start-time block-height)
      (expiration (+ block-height duration))
    )
    
    ;; Ensure only owner can issue license
    (asserts! (is-eq tx-sender current-owner) (err err-not-token-owner))
    
    ;; Check payment is not zero
    (asserts! (> payment u0) (err err-zero-payment))
    
    ;; Transfer payment from licensee
    (try! (stx-transfer? payment licensee tx-sender))
    
    ;; If creator is not the owner, send royalties
    (if (not (is-eq creator current-owner))
        (begin
          ;; Transfer royalty to creator
          (try! (stx-transfer? royalty-amount tx-sender creator))
          
          ;; Update creator revenue
          (let ((current-revenue (get amount (get-creator-revenue creator))))
            (map-set creator-revenue
              { creator: creator }
              { amount: (+ current-revenue royalty-amount) }
            )
          )
          
          ;; Record royalty transaction
          (let ((tx-id (var-get royalty-tx-counter)))
            (map-set royalty-history
              { token-id: token-id, tx-id: tx-id }
              {
                from: licensee,
                to: creator,
                amount: royalty-amount,
                timestamp: block-height
              }
            )
            (var-set royalty-tx-counter (+ tx-id u1))
          )
        )
        true
    )
    
    ;; Store license details
    (map-set licenses
      { token-id: token-id, licensee: licensee }
      {
        start-time: start-time,
        expiration: expiration,
        license-type: license-type,
        license-terms: license-terms,
        active: true
      }
    )
    
    (ok true)
  )
)

;; Revoke a license
(define-public (revoke-license (token-id uint) (licensee principal))
  (let
    (
      (current-owner (unwrap! (get-token-owner token-id) (err err-token-id-not-found)))
      (license-info (unwrap! (map-get? licenses { token-id: token-id, licensee: licensee }) (err u108)))
    )
    
    ;; Ensure only owner can revoke license
    (asserts! (is-eq tx-sender current-owner) (err err-not-token-owner))
    
    ;; Update license to inactive
    (map-set licenses
      { token-id: token-id, licensee: licensee }
      {
        start-time: (get start-time license-info),
        expiration: (get expiration license-info),
        license-type: (get license-type license-info),
        license-terms: (get license-terms license-info),
        active: false
      }
    )
    
    (ok true)
  )
)

;; Extend license duration
(define-public (extend-license (token-id uint) (additional-duration uint) (payment uint))
  (let
    (
      (token-info (unwrap! (map-get? ip-tokens { token-id: token-id }) (err err-token-id-not-found)))
      (current-owner (unwrap! (get-token-owner token-id) (err err-token-id-not-found)))
      (license-info (unwrap! (map-get? licenses { token-id: token-id, licensee: tx-sender }) (err u108)))
      (royalty-amount (* payment (/ (get royalty-percentage token-info) u100)))
      (owner-amount (- payment royalty-amount))
      (creator (get creator token-info))
      (new-expiration (+ (get expiration license-info) additional-duration))
    )
    
    ;; Check if license is active
    (asserts! (get active license-info) (err err-license-expired))
    
    ;; Check payment is not zero
    (asserts! (> payment u0) (err err-zero-payment))
    
    ;; Transfer payment from licensee to owner
    (try! (stx-transfer? payment tx-sender current-owner))
    
    ;; If creator is not the owner, send royalties
    (if (not (is-eq creator current-owner))
        (begin
          ;; Transfer royalty to creator
          (try! (stx-transfer? royalty-amount current-owner creator))
          
          ;; Update creator revenue
          (let ((current-revenue (get amount (get-creator-revenue creator))))
            (map-set creator-revenue
              { creator: creator }
              { amount: (+ current-revenue royalty-amount) }
            )
          )
          
          ;; Record royalty transaction
          (let ((tx-id (var-get royalty-tx-counter)))
            (map-set royalty-history
              { token-id: token-id, tx-id: tx-id }
              {
                from: tx-sender,
                to: creator,
                amount: royalty-amount,
                timestamp: block-height
              }
            )
            (var-set royalty-tx-counter (+ tx-id u1))
          )
        )
        true
    )
    
    ;; Update license with new expiration
    (map-set licenses
      { token-id: token-id, licensee: tx-sender }
      {
        start-time: (get start-time license-info),
        expiration: new-expiration,
        license-type: (get license-type license-info),
        license-terms: (get license-terms license-info),
        active: true
      }
    )
    
    (ok true)
  )
)

;; Allow creators to withdraw their accumulated royalties
(define-public (withdraw-royalties)
  (let
    (
      (revenue-info (get-creator-revenue tx-sender))
      (amount (get amount revenue-info))
    )
    
    ;; Check if there are royalties to withdraw
    (asserts! (> amount u0) (err u109))
    
    ;; Reset creator revenue
    (map-set creator-revenue
      { creator: tx-sender }
      { amount: u0 }
    )
    
    ;; Transfer accumulated royalties from contract to creator
    (try! (as-contract (stx-transfer? amount contract-owner tx-sender)))
    
    (ok amount)
  )
)

;; Contract initialization
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (ok true)
  )
)