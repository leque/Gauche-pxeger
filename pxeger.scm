(define-module pxeger
  (use srfi-1)
  (use srfi-14)
  (use data.random)
  (use text.tree)
  (use util.match)
  (export regexp->string-generator))

(select-module pxeger)

(define (regexp-naive-ast re)
  (regexp-parse (regexp->string re)))

(define (return/env res env)
  (list res env))

(define (context-result ctx)
  (car ctx))

(define (context-env ctx)
  (cadr ctx))

;; Generates strings that match a given regular expression.
;; Unsupported syntaxes are:
;; - non-greedy patterns -- *?, +?, ??, {n,m}?
;; - beginning / end of string -- ^, $
;; - word boundary -- \b, \B
;; - look ahead -- (?=pat), (?!pat)
;; - look behind -- (?<=pat), (?<!pat)
;; - atomic patterns -- (?>pattern), *+, ++, ?+
;; - conditional matching -- (?test-pattern then-pattern|else-pattern)
(define (regexp->string-generator re
                                  :key
                                  (char-set-universe char-set:full)
                                  (*-max-repeat 8))
  (define (ast->generator uncase? env ast)
    (define (wrap-gen gen)
      (^[] (return/env (gen) env)))
    (define (gconst x)
      (^[] x))
    (define (sequence env asts)
      (^[]
        (let loop ((asts asts)
                   (env env)
                   (rs '()))
          (if (null? asts)
              (return/env (reverse! rs) env)
              (let* ((g (ast->generator uncase? env (car asts)))
                     (ctx (g)))
                (loop (cdr asts)
                      (context-env ctx)
                      (cons (context-result ctx) rs)))))))
    (match ast
      ((? char? c)
       (wrap-gen (if uncase?
                     (^[] (if (booleans) c (char-upcase c)))
                     (gconst c))))
      ((? char-set? cs)
       (wrap-gen (chars$ cs)))
      (('comp . (? char-set? cs))
       (wrap-gen (chars$ (char-set-difference char-set-universe cs))))
      ('any
       (wrap-gen (chars$ char-set-universe)))
      (('seq-uncase . asts)
       (ast->generator #t env `(seq ,@asts)))
      (('seq-case . asts)
       (ast->generator #f env `(seq ,@asts)))
      (('seq . asts)
       (sequence env asts))
      (('alt . asts)
       (samples-from
        (list->vector (map (cut ast->generator uncase? env <>) asts))))
      (('rep m n . asts)
       (let ((ast `(seq ,@asts))
             (sizer (integers-between$ m
                                       (if (number? n)
                                           n
                                           (+ m *-max-repeat)))))
         (case (sizer)
           ((0)
            (wrap-gen (gconst '())))
           ((1)
            (ast->generator uncase? env ast))
           (else
            => (lambda (n)
                 (let ((g (ast->generator uncase? env ast)))
                   (^[]
                     (let ((ctx (g)))
                       (return/env
                        (list
                         (map context-result (list-tabulate (- n 1) (^_ (g))))
                         (context-result ctx))
                        (context-env ctx))))))))))
      (((? integer? n) sym . asts)
       (let ((gen (ast->generator uncase? env `(seq ,@asts))))
         (^[]
           (let* ((ctx (gen))
                  (res (context-result ctx))
                  (env~ (context-env ctx)))
             (return/env res (alist-cons n res env~))))))
      ((backref . (? integer? n))
       ;; #/(){0}\1/ fails here as ((chars$ #[])) does
       (wrap-gen (gconst (cdr (assv n env)))))
      ((cpat (? integer? n) ypat npat)
       (let ((ast (if (assv n env) ypat npat)))
         (ast->generator uncase? env `(seq ,@ast))))
      ((or 'bol 'eol 'wb 'nwb
           ('rep-min . _)
           ('rep-while . _)
           ('cpat . _)
           ('once . _)
           ('assert . _)
           ('nassert . _))
       (error "unsupported syntax: " ast))
      (_
       (error "unrecognized syntax: " ast))))
  (unless (char-set? char-set-universe)
    (error "char-set required, but got: " char-set-universe))
  (unless (and (integer? *-max-repeat)
               (>= *-max-repeat 0))
    (error "non-negative integer required, but got: " *-max-repeat))
  (let ((gen (ast->generator #f '() (regexp-naive-ast re))))
    (^[]
      (tree->string (context-result (gen))))))
