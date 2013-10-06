(define-module pxeger
  (use srfi-1)
  (use srfi-14)
  (use data.random)
  (use text.tree)
  (use util.match)
  (export regexp->string-generator))

(select-module pxeger)

(define char-set:any
  (char-set-complement #[]))

(define (regexp-naive-ast re)
  (regexp-parse (regexp->string re)))

;; Generates strings that match a given regular expression.
;; Unsupported syntaxes are:
;; - non-greedy patterns -- *?, +?, ??, {n,m}?
;; - back reference -- \n
;; - named back reference -- \k<name>
;; - beginning / end of string -- ^, $
;; - word boundary -- \b, \B
;; - look ahead -- (?=pat), (?!pat)
;; - look behind -- (?<=pat), (?<!pat)
;; - atomic patterns -- (?>pattern), *+, ++, ?+
;; - conditional matching -- (?test-pattern then-pattern|else-pattern)
(define (regexp->string-generator re
                                  :key
                                  (char-set-universe char-set:any)
                                  (*-max-repeat 8))
  (define (ast->generator uncase? ast)
    (match ast
      ((? char? c)
       (if uncase?
           (^[] (if (booleans) c (char-upcase c)))
           (^[] c)))
      ((? char-set? cs)
       (chars$ cs))
      (('comp . (? char-set? cs))
       (chars$ (char-set-difference char-set-universe cs)))
      ('any
       (chars$ char-set-universe))
      (('seq-uncase . asts)
       (ast->generator #t `(seq ,@asts)))
      (('seq-case . asts)
       (ast->generator #f `(seq ,@asts)))
      (('seq . asts)
       (let ((gens (map (cut ast->generator uncase? <>) asts)))
         (apply tuples-of gens)))
      (('alt . asts)
       (samples-from (list->vector (map (cut ast->generator uncase? <>) asts))))
      (('rep m n . asts)
       (let ((g (ast->generator uncase? `(seq ,@asts)))
             (sizer (integers-between$ m
                                       (if (number? n)
                                           n
                                           (+ m *-max-repeat)))))
         (lists-of sizer g)))
      (((? integer? n) sym . asts)
       (ast->generator uncase? `(seq ,@asts)))
      ((or 'bol 'eol 'wb 'nwb
           ('rep-min . _)
           ('rep-while . _)
           ('cpat . _)
           ('backref . _)
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
  (let ((gen (ast->generator #f (regexp-naive-ast re))))
    (^[] (tree->string (gen)))))
