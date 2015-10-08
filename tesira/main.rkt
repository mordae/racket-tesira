#lang typed/racket/base
;
; Tesira Client
;

(require racket/match
         racket/string)

(require mordae/syntax
         mordae/match)

(require tesira/telnet
         tesira/encoding)

(provide Tesira
         Tesira-Response
         Tesira-Number
         TExpr
         tesira?
         tesira-number?
         tesira-connect
         tesira-listen
         tesira-send)


(define-type Tesira tesira)

(define-type Tesira-Response
  (HashTable Symbol TExpr))


(define-predicate tesira-number? Tesira-Number)


(define-logger tesira)


(struct tesira
  ((in : Input-Port)
   (out : Output-Port)
   (lock : Semaphore)
   (notify : (-> Tesira-Response Void))))

(struct ok
  ((result : Tesira-Response)))

(struct notify
  ((result : Tesira-Response)))

(struct err
  ((message : String)))


(: tesira-connect
  (->* (String) (Positive-Integer (-> Tesira-Response Void)) Tesira))
(define (tesira-connect hostname (port-no 23) (notify void))
  (log-tesira-info "connecting to ~a, port ~a" hostname port-no)

  (define-values (in out)
    (tcp-connect/telnet hostname port-no))

  (log-tesira-debug "awaiting welcome")
  (unless (regexp-match #rx"Welcome.*?\n" in)
    (log-tesira-error "we are not welcome")
    (error 'tesira-connect "we are not welcome"))

  (log-tesira-debug "connected")
  (tesira in out (make-semaphore 1) notify))


(: tesira-send (-> Tesira Symbol Symbol Symbol TExpr * Tesira-Response))
(define (tesira-send a-tesira alias verb attr . args)
  (match-let (((tesira in out lock notifier) a-tesira))
    (with-semaphore lock
      (parameterize ((current-output-port out)
                     (current-input-port in))
        (let ((alias (symbol->string alias))
              (verb  (symbol->string verb))
              (attr  (symbol->string attr)))
          (unless (id? alias)
            (raise-arguments-error 'tesira-send
                                   "invalid alias" "alias" alias))

          (unless (id? verb)
            (raise-arguments-error 'tesira-send
                                   "invalid verb" "verb" verb))

          (unless (id? verb)
            (raise-arguments-error 'tesira-send
                                   "invalid attribute" "attr" attr))

          (define arguments
            (map texpr->string args))

          (let ((args (string-join arguments)))
            (log-tesira-info "-> ~a ~a ~a ~a" alias verb attr args)
            (printf "~a ~a ~a ~a\r\n" alias verb attr args)
            (flush-output)))

        (let drain : Tesira-Response ()
          (match (receive)
            ((ok result)
             (values result))

            ((notify result)
             (notifier result)
             (drain))

            ((err message)
             (error 'tesira-send "~a" message))))))))


(: tesira-listen (-> Tesira Tesira-Response))
(define (tesira-listen a-tesira)
  (match-let (((tesira in out lock _) a-tesira))
    (with-semaphore lock
      (parameterize ((current-output-port out)
                     (current-input-port in))
        (match (receive)
          ((ok _)
           (error 'tesira-listen "received a reply on a listener socket"))

          ((notify result)
           (values result))

          ((err message)
           (error 'tesira-listen "~a" message)))))))


(: id? (-> Any Boolean : #:+ String))
(define (id? v)
  (and (string? v)
       (regexp-match #rx"^[a-zA-Z0-9]+$" v)
       #t))


(: receive (-> (U ok notify err)))
(define (receive)
  (match (read-line (current-input-port) 'any)
    ((regexp-parts #rx"^! *(.*)" (_ str))
     (log-tesira-info "<- ! ~a" str)
     (notify (cast (string->texpr (format "{~a}" str)) Tesira-Response)))

    ((regexp-parts #rx"^\\+OK *(.*)" (_ str))
     (log-tesira-info "<- +OK ~a" str)
     (ok (cast (string->texpr (format "{~a}" str)) Tesira-Response)))

    ((regexp-parts #rx"^-ERR *(.*)" (_ str))
     (log-tesira-info "<- -ERR ~a" str)
     (err str))

    ((? string? line)
     (log-tesira-error "-> unknown: ~a" line)
     (error 'tesira-send "unknown message received"))

    ((? eof-object?)
     (error 'tesira-send "connection terminated"))))


; vim:set ts=2 sw=2 et:
