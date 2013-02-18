#lang racket
(provide (all-defined-out))

(define-syntax bind-macro
    (syntax-rules (to)
        { [bind-macro id rules a1 to a2]                                     [define-syntax id (syntax-rules rules (a1 a2))] }
        { [bind-macro id rules a1 to a2 b1 to b2]                            [define-syntax id (syntax-rules rules (a1 a2) (b1 b2))] }
        { [bind-macro id rules a1 to a2 b1 to b2 c1 to c2]                   [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2))] }
        { [bind-macro id rules a1 to a2 b1 to b2 c1 to c2 d1 to d2]          [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2) (d1 d2))] }
        { [bind-macro id rules a1 to a2 b1 to b2 c1 to c2 d1 to d2 e1 to e2] [define-syntax id (syntax-rules rules (a1 a2) (b1 b2) (c1 c2) (d1 d2) (e1 e2))] } ))

{ bind-macro bind ()
    [bind id (a) body ...]       to [define (id a) (body ...)]
    [bind id (a b) body ...]     to [define (id a b) (body ...)]
    [bind id (a b c) body ...]   to [define (id a b c) (body ...)]
    [bind id (a b c d) body ...] to [define (id a b c d) (body ...)] }

{ bind-macro rebind ()
    [rebind a b] to { bind-macro a () a to b } }

{ bind-macro fn (=>)
    [fn x => body ...] to [lambda x (body ...)] }

{ bind-macro ~ ()
    [~ x op y]     to [op x y]
    [~ x op y ...] to [~ x op (y ...)] }

{ bind-macro :: ()
    [:: x ...] to [cons x ...] }

{ bind-macro @ () 
    [@ x ...] to [append x ...] }

{ bind-macro == () 
    [== x ...] to [equal? x ...] }

{ bind-macro mod ()
    [mod a b] to [remainder a b] }

{ bind-macro when (else)
    [when e1 a1 else d]             to [if e1 a1 d]
    [when e1 a1 e2 a2 else d]       to [cond (e1 a1) (e2 a2) (#t d)]
    [when e1 a1 e2 a2 e3 a3 else d] to [cond (e1 a1) (e2 a2) (e3 a3) (#t d)] }
