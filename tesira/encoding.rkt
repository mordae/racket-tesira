#lang typed/racket/base
;
; Tesira Data Encoding
;

(require racket/match)

(provide TExpr)

(require/typed/provide tesira/private/encoding
  (texpr->string (-> TExpr String))
  (string->texpr (-> String TExpr))
  (write-texpr (->* (TExpr) (Output-Port) Void))
  (read-texpr (->* () (Input-Port) TExpr)))


(define-type TExpr
  (Rec TExpr
    (U String Real Boolean
       (Listof TExpr)
       (HashTable Symbol TExpr))))


; vim:set ts=2 sw=2 et:
