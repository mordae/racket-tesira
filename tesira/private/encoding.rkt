#lang racket/base
;
; Tesira Data Encoding - Untyped Code
;

(require racket/match
         parser-tools/yacc
         parser-tools/lex)

(require
  (prefix-in : parser-tools/lex-sre))

(provide texpr->string
         string->texpr
         write-texpr
         read-texpr)


(define (read-texpr (in (current-input-port)))
  (tesira-parse
    (λ ()
      (tesira-lex in))))


(define (write-texpr v (out (current-output-port)))
  (parameterize ((current-output-port out))
    (match v
      ((? real? v)
       (display v))

      ((? string? v)
       (printf "~s" v))

      ((? symbol? v)
       (write-texpr
         (symbol->string v)))

      ((? boolean? v)
       (display (if v "true" "false")))

      ((? list? v)
       (display "[")
       (write-texpr-list v)
       (display "]"))

      ((? hash? v)
       (display "{")
       (write-texpr-hash v)
       (display "}")))))


(define (write-texpr-list v (out (current-output-port)))
  (parameterize ((current-output-port out))
    (let ((first #t))
      (for ((item v))
        (if first
            (set! first #f)
            (display " "))
        (write-texpr item)))))


(define (write-texpr-hash v (out (current-output-port)))
  (parameterize ((current-output-port out))
    (let ((first #t))
      (for (((key value) v))
        (if first
            (set! first #f)
            (display " "))
        (write-texpr key)
        (display ":")
        (write-texpr value)))))


(define (texpr->string v)
  (let ((out (open-output-string)))
    (write-texpr v out)
    (get-output-string out)))


(define (string->texpr str)
  (let ((in (open-input-string str)))
    (read-texpr in)))


(define-tokens datum
  (string number boolean))

(define-empty-tokens syntax
  (colon left-brace right-brace left-bracket right-bracket space eof))


(define tesira-lex/base
  (lexer
    ((:: (:? #\-)
         (:+ (char-range #\0 #\9))
         (:? (:: #\. (:+ (char-range #\0 #\9)))))
     (token-number
       (string->number lexeme)))

    ((:: #\"
         (:* (:or (:~ #\")
                  (:: #\\ #\")))
         #\")
     (let* ((end (string-length lexeme))
            (str (substring lexeme 1 (sub1 end))))
       (token-string
         (regexp-replace* #rx"\\\"" str "\""))))

    ((:or "true" "false")
     (token-boolean (equal? lexeme "true")))

    ((:+ whitespace)
     (token-space))

    (":" (token-colon))
    ("{" (token-left-brace))
    ("}" (token-right-brace))
    ("[" (token-left-bracket))
    ("]" (token-right-bracket))))


(define (tesira-lex in)
  (let ((a-token (tesira-lex/base in)))
    (if (eq? 'space a-token)
        (tesira-lex in)
        (values a-token))))


(define tesira-parse
  (parser
    (grammar
      (value ((string)  $1)
             ((number)  $1)
             ((boolean) $1)
             ((list)    $1)
             ((dict)    $1))

      (dict ((left-brace dict-pairs right-brace)
             (values $2)))

      (dict-pairs ((string colon value dict-pairs)
                   (let* ((key (string->symbol $1)))
                     (hash-set $4 key $3)))

                  (()
                   (hasheq)))

      (list ((left-bracket list-items right-bracket)
             (values $2)))

      (list-items ((value list-items)
                   (cons $1 $2))

                  (() null)))

    (tokens datum syntax)
    (start value)
    (end eof)
    (error (λ (tok-ok? tok-name tok-value)
             (error 'tesira-parser "unexpected ~a token: ~s" tok-name tok-value)))))


; vim:set ts=2 sw=2 et:
