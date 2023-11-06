(use-trait sip-010-trait .sip-010-trait.sip-010-trait)

(define-trait sip-026-trait
  (
    (holdings (uint) (response uint uint))

    (holdings-of (uint principal) (response uint uint))

    (asset-contract (uint) (response principal uint))
  )
)