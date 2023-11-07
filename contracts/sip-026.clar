(use-trait token .sip-010-trait.sip-010-trait)
(impl-trait .sip-026-trait.sip-026-trait)

(define-constant err-asset-not-found (err u1))
(define-constant err-asset-by-principal-not-found (err u2))

(define-constant this-contract (as-contract tx-sender))

(define-map vaults uint { asset-contract: principal, vault-holdings: uint })
(define-map vault-assets-by-principal { vault-id: uint, owner: principal } { holdings: uint, contract: principal })

(define-map vault-id-principal uint principal)

(define-data-var last-id uint u0)

;; trait functions
(define-public (asset-contract (vault-id uint))
  (ok (get asset-contract (unwrap! (map-get? vaults vault-id) err-asset-not-found)))
)

(define-public (holdings (vault-id uint))
  (match (map-get? vaults vault-id)
    amount (ok (get vault-holdings amount))
    (ok u0)
  )
)

(define-public (holdings-of (vault-id uint) (owner principal))
  (match (map-get? vault-assets-by-principal { owner: owner, vault-id: vault-id})
    amount (ok (get holdings amount))
    (ok u0)
  )
)

(define-public (deposit (vault-id uint) (asset <token>) (amount uint) (sender principal))
  (let (
      (asset-by-principal-data (map-get? vault-assets-by-principal { vault-id: vault-id, owner: sender }))
      (asset-data (map-get? vaults vault-id))
      (asset-principal (contract-of asset))
    )
    (try! (contract-call? asset transfer amount sender this-contract none))

    (match asset-data
      ;; asset has already been deposited before
      asset-data-ret
        (begin
          (match asset-by-principal-data
            asset-by-principal-data-ret
              (let (
                (prev-amount (get holdings asset-by-principal-data-ret))
                (vault-holdings (get vault-holdings asset-data-ret))
              )
                (map-set vaults vault-id { asset-contract: asset-principal, vault-holdings: (+ vault-holdings amount) })
                (map-set vault-assets-by-principal { vault-id: vault-id, owner: tx-sender } { contract: asset-principal, holdings: (+ prev-amount amount) })
                true
              )
            ;; first time depositing this asset
            (let (
              (vault-holdings (get vault-holdings asset-data-ret))
            )
              (map-set vaults vault-id { asset-contract: asset-principal, vault-holdings: (+ vault-holdings amount) })
              (map-set vault-assets-by-principal { vault-id: vault-id, owner: tx-sender } { contract: asset-principal, holdings: amount })
              true
            )
          ) 
        )
        ;; asset has never been deposited
        ;; first time depositing this asset
        (begin
          (map-set vaults vault-id { asset-contract: asset-principal, vault-holdings: amount })
          (map-set vault-assets-by-principal { vault-id: vault-id, owner: tx-sender } { contract: asset-principal, holdings: amount })
          false
        )
    )
    (ok amount)
  )
)

(define-public (withdraw (vault-id uint) (asset <token>) (amount uint) (recipient principal))
  (let (
      (asset-by-principal-data (map-get? vault-assets-by-principal { vault-id: vault-id, owner: recipient }))
      (asset-data (map-get? vaults vault-id))
      (prev-amount (match asset-by-principal-data result (get holdings result) u0))
      (asset-amount (match asset-data result (get vault-holdings result) u0))
      (asset-principal (contract-of asset))
    )
    (try! (as-contract (contract-call? asset transfer amount this-contract recipient none)))

    ;; does not check for underflow or if user already deposited
    (map-set vaults
      vault-id
      { asset-contract: asset-principal, vault-holdings: (if (> amount asset-amount) u0 (- asset-amount amount)) })
    (map-set vault-assets-by-principal
      { vault-id: vault-id, owner: tx-sender }
      { contract: asset-principal, holdings: (if (> amount prev-amount) u0 (- prev-amount amount)) })

    (ok amount)
  )
)
