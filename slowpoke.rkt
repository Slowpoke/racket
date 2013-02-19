#lang racket
(provide (all-defined-out))

;;
;; cleanup parentheses
(define-syntax bind-macro 
    (syntax-rules (to)
        { [_ id to a]                                               [define-syntax id (syntax-rules () ([_ x (... ...)] [a x (... ...)]))] }
        { [_ id rules a1 to a2]                                     [define-syntax id (syntax-rules rules (a1 a2))] }
        { [_ id rules a1 to a2 b1 to b2]                            [define-syntax id (syntax-rules rules (a1 a2) (b1 b2))] }
        { [_ id rules a1 to a2 b1 to b2 c1 to c2]                   [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2))] }
        { [_ id rules a1 to a2 b1 to b2 c1 to c2 d1 to d2]          [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2) (d1 d2))] }
        { [_ id rules a1 to a2 b1 to b2 c1 to c2 d1 to d2 e1 to e2] [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2) (d1 d2) (e1 e2))] } ))
 
{ bind-macro bind (to)
    [_ id (args ...) to body    ] to [define (id args ...) body]
    [_ id (args ...)    body    ] to [define (id args ...) body]
    [_ id (args ...) to body ...] to [define (id args ...) (body ...)]
    [_ id (args ...)    body ...] to [define (id args ...) (body ...)] }

{ bind-macro fn (=>)
    [_ x => body]     to [lambda x body]
    [_ x => body ...] to [lambda x (body ...)] }

{ bind-macro when (else)
    [_ e1 a1 else d]                 to [if e1 a1 d]
    [_ e1 a1 _ e2 a2 else d]         to [cond (e1 a1) (e2 a2) (#t d)]
    [_ e1 a1 _ e2 a2 _ e3 a3 else d] to [cond (e1 a1) (e2 a2) (e3 a3) (#t d)] }

{ bind-macro with (do and =)
    [_ a1 = b1 do body ...]                         to [let* ([a1 b1]) (body ...)]
    [_ a1 = b1 and a2 = b2 do body ...]             to [let* ([a1 b1] [a2 b2]) (body ...)]
    [_ a1 = b1 and a2 = b2 and a3 = b3 do body ...] to [let* ([a1 b1] [a2 b2] [a3 b3]) (body ...)] }

{ bind-macro try (catch)
    [_ body ... catch e1 a1]             to [with-handlers ([e1 a1]) (body ...)]
    [_ body ... catch e1 a1 e2 a2]       to [with-handlers ([e1 a1] [e2 a2]) (body ...)]
    [_ body ... catch e1 a1 e2 a2 e3 a3] to [with-handlers ([e1 a1] [e2 a2] [e3 a3]) (body ...)]}

{ bind-macro ~ ()
    [_ x op y]     to [op x y]
    [_ x op y ...] to [~ x op (y ...)] }

{ bind-macro :: to cons }
{ bind-macro @ to append }

{ bind-macro head to car }
{ bind-macro tail to cdr }

{ bind-macro any? to ormap }
{ bind-macro all? to andmap }

{ bind-macro mod to remainder }

;;
;; tests
{ bind-macro write-line ()
    [_ format-string args ...] to [displayln (format format-string args ...)] }

{ bind-macro test-val ()
    [_ id action answ] to [with result = (try ~ #f :: action
                                          catch [fn (ex) => #t]
                                                [fn (ex) => ~ #t :: ex] )
                            and raised = [head result]
                            and result = [tail result]
                             do
                           when raised [write-line "~a FAIL: raised exception ~v instead of returning a result" id result]
                           when [equal? result answ] [write-line "~a SUCCESS" id]
                           else (write-line "~a FAIL: returned wrong result ~v" id result)] }

{ bind-macro test-exn ()
    [_ id action pred] to [with result = (try ~ #f :: action
                                          catch [fn (ex) => #t]
                                                [fn (ex) => ~ #t :: ex] )
                            and raised = [head result]
                            and result = [tail result]
                             do
                           when [~ raised and pred result] [write-line "~a SUCCESS" id]
                           when raised [write-line "~a FAIL: raised wrong exception ~v" id result]
                           else (write-line "~a FAIL: returned ~v instead of raising an exception" id result)] }

{ bind-macro error-with-msg? () [_ msg] to [fn (exn) => ~ (exn:fail? exn) and equal? msg (exn-message exn)] }
;;
;; Examples
;[test-val 10 (sequence 3 11 2) '(3 5 7 9 11)]
;[test-val 11 (sequence 3 8 3) '(3 6)]
;[test-val 12 (sequence 3 2 1) null]
;[test-val 20 (string-append-map null "-qwe") null]
;[test-val 21 (string-append-map '("ab" "cd") "-qwe") '("ab-qwe" "cd-qwe")]
;[test-val 30 (list-nth-mod '(0 1 2 3 4 5 6 7 8 9 10) 12) 1]
;[test-exn 31 (list-nth-mod '(0 1 2 3 4 5 6 7 8 9 10) -1) (error-with-msg? "list-nth-mod: negative number")]
;[test-exn 32 (list-nth-mod null 12) (error-with-msg? "list-nth-mod: empty list")]
