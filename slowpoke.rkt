#lang racket

(provide (all-defined-out))

; ------- cleanup parentheses -------

(require (for-syntax syntax/parse))
(require (for-syntax syntax/parse/experimental/template))

(begin-for-syntax

    (define-syntax bind-class
        (syntax-rules (to)
            { [_ id p1]                [define-syntax-class          id                     (pattern p1)] }
            { [_ id p1 to t1]          [define-syntax-class          id #:attributes (tmpl) (pattern p1 #:with tmpl #'t1)] }
            { [_ id p1 to t1 p2 to t2] [define-syntax-class          id #:attributes (tmpl) (pattern p1 #:with tmpl #'t1) (pattern p2 #:with tmpl #'t2)] } ))
    (define-syntax bind-splicing-class
        (syntax-rules (to)
            { [_ id p1]                [define-splicing-syntax-class id                     (pattern p1)] }
            { [_ id p1 to t1]          [define-splicing-syntax-class id #:attributes (tmpl) (pattern p1 #:with tmpl #'t1)] } 
            { [_ id p1 to t1 p2 to t2] [define-splicing-syntax-class id #:attributes (tmpl) (pattern p1 #:with tmpl #'t1) (pattern p2 #:with tmpl #'t2)] } ))

    { bind-class do   [~literal do  ] }
    { bind-class to   [~literal to  ] }
    { bind-class else [~literal else] }
    { bind-class when [~literal when] }
    { bind-class with [~literal with] }

    { bind-class expr-to/with   [~and expr:expr (~not (~or _:to   _:with))] to expr }
    { bind-class expr-do/with   [~and expr:expr (~not (~or _:do   _:with))] to expr }
    { bind-class expr-when/else [~and expr:expr (~not (~or _:when _:else))] to expr }

    { bind-splicing-class bind-id
        [~seq id:id                    ] to  id
        [~seq id:id ((~seq arg:id ...))] to [id arg ...] }

    { bind-splicing-class helper-body
        [~seq _:with       body:expr-to/with      ] to  body.tmpl
        [~seq _:with (~seq body:expr-to/with ...+)] to [body.tmpl ...] }

    { bind-splicing-class when-body
        [~seq _:when e:expr       body:expr-when/else      ] to [e  body.tmpl     ]
        [~seq _:when e:expr (~seq body:expr-when/else ...+)] to [e (body.tmpl ...)] }
    { bind-splicing-class when-body-last
        [~seq _:when e:expr       body:expr      ] to [e  body]
        [~seq _:when e:expr (~seq body:expr ...+)] to [e (body ...)] }

    { bind-splicing-class with-body
        [~seq _:with id:id (~literal =)       body:expr-do/with      ] to [id  body.tmpl     ]
        [~seq _:with id:id (~literal =) (~seq body:expr-do/with ...+)] to [id (body.tmpl ...)] }

    { bind-splicing-class body
              body:expr      to  body
        [~seq body:expr ...] to [body ...] }

    { bind-splicing-class pat-to-tmpl
        [~seq a:expr _:to b:expr] to [a (template b)] }

)

(define-syntax (bind-macro stx)
    (syntax-parse stx
        { [_ id:id _:to a:id]                                      [template (define-syntax id (syntax-rules () ([_ x (... ...)] [a x (... ...)])))] }
        { [_ id:id (~seq macro:pat-to-tmpl ...+)]                  [template (define-syntax (id stx) (syntax-parse stx macro.tmpl ...)  )] }
        { [_ id:id ((~seq arg:id ...)) _:to (~seq body:expr ...+)] [template (define-syntax id (syntax-rules () ([_ arg ...] [body ...])))] } ))

{ bind-macro bind
    [_ id:bind-id (~seq helper:helper-body ...) _:to body:body] to [define id.tmpl helper.tmpl ... body.tmpl] }

{ bind-macro fn
    [_ (~optional args:expr) (~literal =>) body:body] to [lambda (?? args ()) body.tmpl] }

{ bind-macro when
    [_ e:expr       then:expr-when/else       _:else else:body] to [if e  then.tmpl      else.tmpl]
    [_ e:expr (~seq then:expr-when/else ...+) _:else else:body] to [if e (then.tmpl ...) else.tmpl]
    [(~seq when:when-body ...+) _:else else:body]   to [cond when.tmpl ... (#t else.tmpl)]
    [(~seq when:when-body ...) last:when-body-last] to [cond when.tmpl ... last.tmpl] }

{ bind-macro with
    [(~seq with:with-body ...+) _:do body:body] to [letrec (with.tmpl ...) body.tmpl] }

{ bind-macro ~ 
    [_ x:expr op:expr y:body] to [op x y.tmpl] }

{ bind-macro !
    [_ x:expr ...] to [not (x ...)] }

{ bind-macro  ::  to cons }
{ bind-macro head to car }
{ bind-macro tail to cdr }
{ bind-macro mod  to remainder }

; ------- tests -------

{ bind-macro write-line
    [_ format-string args ...] to [displayln (format format-string args ...)] }

{ bind-macro try
    [_ body:expr ... (~literal catch) handler:body] to [with-handlers ([(fn (exn) => #t) handler.tmpl]) (body ...)] }

{ bind-macro test-val (id action answ) to
        with pair = try ~ #f :: action
                    catch fn (exn) => ~ #t :: exn
        with exn? = head pair
        with rslt = tail pair
          do when exn?               write-line "~a FAIL: raised exception ~v instead of returning a result" id rslt
             when [equal? rslt answ] write-line "~a SUCCESS" id
             else                    write-line "~a FAIL: returned wrong result ~v" id rslt }

{ bind-macro test-exn (id action pred) to
        with pair = try ~ #f :: action
                    catch fn (exn) => ~ #t :: exn
        with exn? = head pair
        with rslt = tail pair
          do when [~ exn? and pred rslt] write-line "~a SUCCESS" id
             when exn?                   write-line "~a FAIL: raised wrong exception ~v" id rslt
             else                        write-line "~a FAIL: returned ~v instead of raising an exception" id rslt }

{ bind-macro error-with-msg? (msg) to fn (exn) => ~ (exn:fail? exn) and equal? msg (exn-message exn) }
