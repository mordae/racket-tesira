#lang typed/racket/base
;
; Tesira Mixer Model
;

(require racket/match
         typed/racket/class
         typed/json)

(require mordae/match)

(require tesira
         tesira/util)

(provide Tesira-Mixer%
         Tesira-Mixer-Line%
         tesira-mixer%
         tesira-mixer-input%
         tesira-mixer-output%)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-type Tesira-Mixer%
  (Class
    (init-field (device Tesira)
                (alias Symbol))

    (get (-> Symbol TExpr * TExpr))
    (set (-> Symbol TExpr * Void))

    (primitive-get (-> Symbol (Listof TExpr) TExpr))
    (primitive-set (-> Symbol (Listof TExpr) Void))

    (get-num-inputs  (-> Natural))
    (get-num-outputs (-> Natural))

    (get-input  (-> Natural (Instance Tesira-Mixer-Line%)))
    (get-output (-> Natural (Instance Tesira-Mixer-Line%)))

    (status (-> JSExpr))))


(define-type Tesira-Mixer-Line%
  (Class
    (init-field (mixer (Instance Tesira-Mixer%))
                (index Natural))

    (get (-> Symbol TExpr * TExpr))
    (set (-> Symbol TExpr * Void))

    (get-label  (-> String))
    (set-label! (-> String Void))

    (get-level  (-> Tesira-Number))
    (set-level! (-> Tesira-Number Void))

    (get-mute? (-> Boolean))
    (set-mute! (-> Boolean Void))

    (status (-> JSExpr))))


(: make-tesira-mixer-line% (-> Symbol Symbol Symbol Tesira-Mixer-Line%))
(define (make-tesira-mixer-line% level-name label-name mute-name)
  (class object%
    (init-field mixer index)

    (define last-level-time : Real 0.0)
    (define last-level : Tesira-Number 0.0)

    (define last-mute?-time : Real 0.0)
    (define last-mute? : Boolean #f)

    (define last-label-time : Real 0.0)
    (define last-label : String "")

    (define/public (get attribute . args)
      (send mixer primitive-get attribute (list* index args)))

    (define/public (set attribute . args)
      (send mixer primitive-set attribute (list* index args)))

    (define/public (get-label)
      (let ((now (current-inexact-milliseconds)))
        (when (> (- now last-level-time) 60000)
          (set! last-label-time now)
          (set! last-label
            (cast (get label-name) String))))

      (values last-label))

    (define/public (set-label! value)
      (set label-name value)
      (set! last-label-time 0))

    (define/public (get-level)
      (let ((now (current-inexact-milliseconds)))
        (when (> (- now last-level-time) 15000)
          (set! last-level-time now)
          (set! last-level
            (cast (get level-name) Tesira-Number))))

      (values last-level))

    (define/public (set-level! value)
      (set level-name value)
      (set! last-level-time 0))

    (define/public (get-mute?)
      (let ((now (current-inexact-milliseconds)))
        (when (> (- now last-mute?-time) 15000)
          (set! last-mute?-time now)
          (set! last-mute?
            (cast (get mute-name) Boolean))))

      (values last-mute?))

    (define/public (set-mute! value)
      (set mute-name value)
      (set! last-mute?-time 0))

    (define/public (status)
      (hashjs 'label (get-label)
              'level (get-level)
              'mute? (get-mute?)))

    (super-new)))

(define tesira-mixer-input%
  (make-tesira-mixer-line% 'inputLevel 'inputLabel 'inputMute))

(define tesira-mixer-output%
  (make-tesira-mixer-line% 'outputLevel 'outputLabel 'outputMute))


(: tesira-mixer% Tesira-Mixer%)
(define tesira-mixer%
  (class object%
    (init-field device alias)

    (define num-inputs : Natural
      (cast (get 'numInputs) Natural))

    (define num-outputs : Natural
      (cast (get 'numOutputs) Natural))

    (define inputs : (Vectorof (Instance Tesira-Mixer-Line%))
      (for/vector ((i num-inputs)) : (Instance Tesira-Mixer-Line%)
        (new tesira-mixer-input% (mixer this) (index (add1 i)))))

    (define outputs : (Vectorof (Instance Tesira-Mixer-Line%))
      (for/vector ((i num-outputs)) : (Instance Tesira-Mixer-Line%)
        (new tesira-mixer-output% (mixer this) (index (add1 i)))))

    (define/public (primitive-get attribute args)
      (apply tesira-get device alias attribute args))

    (define/public (primitive-set attribute args)
      (apply tesira-set device alias attribute args))

    (define/public (get attribute . args)
      (primitive-get attribute args))

    (define/public (set attribute . args)
      (primitive-set attribute args))

    (define/public (get-num-inputs)
      (values num-inputs))

    (define/public (get-num-outputs)
      (values num-outputs))

    (define/public (get-input index)
      (vector-ref inputs index))

    (define/public (get-output index)
      (vector-ref outputs index))

    (define/public (status)
      (hashjs 'inputs
              (for/list ((input inputs)) : (Listof JSExpr)
                (send input status))

              'outputs
              (for/list ((output outputs)) : (Listof JSExpr)
                (send output status))))

    (super-new)))


; vim:set ts=2 sw=2 et:
