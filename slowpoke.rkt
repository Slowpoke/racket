#lang racket

;;
;; cleanup parentheses
(define-syntax bind-macro
    (syntax-rules (to)
        { [_ id to a]                                                        [define-syntax id (syntax-rules () ([_ x (... ...)] [a x (... ...)]))] }
        { [_ id (args ...) to body    ]                                      [define-syntax id (syntax-rules () ([_ args ...   ] [body         ]))] }
        { [_ id (args ...) to body ...]                                      [define-syntax id (syntax-rules () ([_ args ...   ] [body ...     ]))] }
        { [_ id rules a1 to b1]                                              [define-syntax id (syntax-rules rules (a1 b1))] }
        { [_ id rules a1 to b1 a2 to b2]                                     [define-syntax id (syntax-rules rules (a1 b1) (a2 b2))] }
        { [_ id rules a1 to b1 a2 to b2 a3 to b3]                            [define-syntax id (syntax-rules rules (a1 b1) (a2 b2) (a3 b3))] }
        { [_ id rules a1 to b1 a2 to b2 a3 to b3 a4 to b4]                   [define-syntax id (syntax-rules rules (a1 b1) (a2 b2) (a3 b3) (a4 b4))] }
        { [_ id rules a1 to b1 a2 to b2 a3 to b3 a4 to b4 a5 to b5]          [define-syntax id (syntax-rules rules (a1 b1) (a2 b2) (a3 b3) (a4 b4) (a5 b5))] }
        { [_ id rules a1 to b1 a2 to b2 a3 to b3 a4 to b4 a5 to b5 a6 to b6] [define-syntax id (syntax-rules rules (a1 b1) (a2 b2) (a3 b3) (a4 b4) (a5 b5) (a6 b6))] }        ))

{ bind-macro bind (to)
    [_ id (args ...) with helper to body    ] to [define (id args ...) helper  body     ]
    [_ id (args ...) with helper to body ...] to [define (id args ...) helper (body ...)]
    [_ id                        to body    ] to [define (id         )         body     ]
    [_ id                        to body ...] to [define (id         )        (body ...)]
    [_ id (args ...)             to body    ] to [define (id args ...)         body     ]
    [_ id (args ...)             to body ...] to [define (id args ...)        (body ...)] }

{ bind-macro fn (=>)
    [_    => body]     to [lambda ()  body     ]
    [_    => body ...] to [lambda () (body ...)]
    [_ xs => body]     to [lambda xs  body     ]
    [_ xs => body ...] to [lambda xs (body ...)] }

{ bind-macro when (else)
    [_ e1 a1 else d]                         to [if e1 a1 d]
    [_ e1 a1 _ e2 a2 else d]                 to [cond (e1 a1) (e2 a2) (#t d)]
    [_ e1 a1 _ e2 a2 _ e3 a3 else d]         to [cond (e1 a1) (e2 a2) (e3 a3) (#t d)]
    [_ e1 a1 _ e2 a2 _ e3 a3 _ e4 a4 else d] to [cond (e1 a1) (e2 a2) (e3 a3) (e4 a4) (#t d)] }

{ bind-macro with (do and =)
    [_ a1 = b1 do body ...]                                     to [letrec ([a1 b1]) (body ...)]
    [_ a1 = b1 and a2 = b2 do body ...]                         to [letrec ([a1 b1] [a2 b2]) (body ...)]
    [_ a1 = b1 and a2 = b2 and a3 = b3 do body ...]             to [letrec ([a1 b1] [a2 b2] [a3 b3]) (body ...)]
    [_ a1 = b1 and a2 = b2 and a3 = b3 and a4 = b4 do body ...] to [letrec ([a1 b1] [a2 b2] [a3 b3] [a4 b4]) (body ...)] }

{ bind-macro try (catch)
    [_ body ... catch e1 a1]                   to [with-handlers ([e1 a1]) (body ...)]
    [_ body ... catch e1 a1 e2 a2]             to [with-handlers ([e1 a1] [e2 a2]) (body ...)]
    [_ body ... catch e1 a1 e2 a2 e3 a3]       to [with-handlers ([e1 a1] [e2 a2] [e3 a3]) (body ...)]
    [_ body ... catch e1 a1 e2 a2 e3 a3 e4 a4] to [with-handlers ([e1 a1] [e2 a2] [e3 a3] [e4 a4]) (body ...)] }

{ bind-macro ~ ()
    [_ x op y    ] to [op x  y     ]
    [_ x op y ...] to [op x (y ...)] }

{ bind-macro call (with)
    [_ op with x    ] to [op  x     ]
    [_ op with x ...] to [op (x ...)] }

{ bind-macro :: to cons }
{ bind-macro @ to append }

{ bind-macro head to car }
{ bind-macro tail to cdr }

{ bind-macro any? to ormap }
{ bind-macro all? to andmap }

{ bind-macro mod to remainder }

;;
;; tests
{ bind-macro write-line (format-string args ...) to
        call displayln with format format-string args ... }

{ bind-macro test-val (id action answ) to
        with pr     = (try ~ #f :: action
                       catch [fn (ex) => #t]
                             [fn (ex) => ~ #t :: ex])
         and raised = [head pr]
         and result = [tail pr]
          do
        when raised [write-line "~a FAIL: raised exception ~v instead of returning a result" id result]
        when [equal? result answ] [write-line "~a SUCCESS" id]
        else (write-line "~a FAIL: returned wrong result ~v" id result) }

{ bind-macro test-exn (id action pred) to
        with pr     = (try ~ #f :: action
                       catch [fn (ex) => #t]
                             [fn (ex) => ~ #t :: ex] )
         and raised = [head pr]
         and result = [tail pr]
          do
        when [~ raised and pred result] [write-line "~a SUCCESS" id]
        when raised [write-line "~a FAIL: raised wrong exception ~v" id result]
        else (write-line "~a FAIL: returned ~v instead of raising an exception" id result) }

{ bind-macro error-with-msg? (msg) to fn (ex) => ~ (exn:fail? ex) and equal? msg (exn-message ex) }
