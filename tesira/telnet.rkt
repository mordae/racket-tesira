#lang typed/racket/base
;
; De-Telnetization of TCP Sockets
;

(require racket/tcp
         racket/match)

(provide tcp-connect/telnet)


(: tcp-connect/telnet
  (-> String Positive-Integer (values Input-Port Output-Port)))
(define (tcp-connect/telnet hostname port-no)
  (define-values (in out)
    (tcp-connect hostname port-no))

  (let negotiate ()
    (when (regexp-match-peek #rx#"^\xFF.." in)
      (match (list (read-byte in)
                   (read-byte in)
                   (read-byte in))
        ((list _ #xFD #x0A)
         (void))

        ((list _ #xFD (? byte? option))
         (write-bytes #"\xFF\xFC" out)
         (write-byte option out))

        ((list _ #xFB (? byte? option))
         (write-bytes #"\xFF\xFE" out)
         (write-byte option out))

        (else (void)))

      (flush-output out)
      (negotiate)))

  (values in out))


; vim:set ts=2 sw=2 et:
