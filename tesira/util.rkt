#lang typed/racket/base
;
; Tesira Client Utilities
;

(require racket/match
         racket/function)

(require mordae/match)

(require tesira)

(provide tesira-get
         tesira-set)


(: tesira-get (-> Tesira Symbol Symbol TExpr * TExpr))
(define (tesira-get a-tesira alias attribute . args)
  (match-let (((hash-lookup ('value value))
               (apply tesira-send a-tesira alias 'get attribute args)))
    (values value)))


(: tesira-set (-> Tesira Symbol Symbol TExpr * Void))
(define (tesira-set a-tesira alias attribute . args)
  (void
    (apply tesira-send a-tesira alias 'set attribute args)))


; vim:set ts=2 sw=2 et:
