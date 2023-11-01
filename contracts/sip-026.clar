(use-trait token .sip-010-trait.sip-010-trait)
(impl-trait .sip-026-trait.sip-026-trait)

(define-constant err-asset-not-found (err u1))
(define-constant err-asset-by-principal-not-found (err u2))

(define-constant this-contract (as-contract tx-sender))

(define-map assets uint { asset-contract:  principal, asset-holdings: uint })

(define-map asset-by-principal { vault-id: uint, owner: principal } { holdings: uint, contract: principal })

;; trait functions
(define-public (asset-contract (vault-id uint))
  (ok (get asset-contract (unwrap! (map-get? assets vault-id) err-asset-not-found)))
)

(define-public (holdings (vault-id uint))
  (match (map-get? assets vault-id)
    amount (ok (get asset-holdings amount))
    (ok u0)
  )
)

(define-public (holdings-of (vault-id uint) (owner principal))
  (match (map-get? asset-by-principal { owner: owner, vault-id: vault-id})
    amount (ok (get holdings amount))
    (ok u0)
  )
)

(define-public (deposit (vault-id uint) (asset <token>) (amount uint) (sender principal))
  (let (
      (asset-by-principal-data (map-get? asset-by-principal { vault-id: vault-id, owner: sender }))
      (asset-data (map-get? assets vault-id))
      (prev-amount (match asset-by-principal-data result (get holdings result) u0))
      (asset-amount (match asset-data result (get asset-holdings result) u0))
      (asset-principal (contract-of asset))
    )
    (try! (contract-call? asset transfer amount sender this-contract none))

    (map-set assets vault-id { asset-contract: asset-principal, asset-holdings: (+ asset-amount amount) })
    (map-set asset-by-principal { vault-id: vault-id, owner: tx-sender } { contract: asset-principal, holdings: (+ prev-amount amount) })

    (ok amount)
  )
)


(define-public (withdraw (vault-id uint) (asset <token>) (amount uint) (recipient principal))
  (let (
      (asset-by-principal-data (map-get? asset-by-principal { vault-id: vault-id, owner: recipient }))
      (asset-data (map-get? assets vault-id))
      (prev-amount (match asset-by-principal-data result (get holdings result) u0))
      (asset-amount (match asset-data result (get asset-holdings result) u0))
      (asset-principal (contract-of asset))
    )
    (try! (as-contract (contract-call? asset transfer amount this-contract recipient none)))

    (map-set assets
      vault-id
      { asset-contract: asset-principal, asset-holdings: (if (> amount asset-amount) u0 (- asset-amount amount)) })
    (map-set asset-by-principal
      { vault-id: vault-id, owner: tx-sender }
      { contract: asset-principal, holdings: (if (> amount prev-amount) u0 (- prev-amount amount)) })

    (ok amount)
  )
)
