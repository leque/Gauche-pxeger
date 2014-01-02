(use gauche.test)

(use srfi-13)
(use srfi-14)
(use gauche.lazy)
(use data.random)

(use pxeger)

(test-start "pxeger")
(test-module 'pxeger)

(define (test-pxeger re)
  (dolist (s (ltake (generator->lseq (regexp->string-generator re)) 10))
    (test* (format "~S from ~S" s re)
           #t
           (and (re s) #t))))

(test-pxeger #/a/)
(test-pxeger #/ab/)
(test-pxeger #/(a|b)/)
(test-pxeger #/[a-z]/)
(test-pxeger #/[^a-z]/)
(test-pxeger #/./)
(test-pxeger #/(a|b)*/)
(test-pxeger #/(a|b)?/)
(test-pxeger #/(a|b)+/)
(test-pxeger #/(a|b){2,3}/)
(test-pxeger #/(a|b){2}/)
(test-pxeger #/(a|b){2,}/)
(test-pxeger #/(a|b){,3}/)
(test-pxeger #/[a-z]*[0-9]{2,3}/i)
(test-pxeger #/(?-i:[a-z]*)[0-9]{2,3}/i)
(test-pxeger #/(?-i:[a-z]*)[a-z]{2,3}/i)
(test-pxeger #/\s\d\w\S\D\W/i)

(define (f re n)
  (apply max
         (lmap string-length
               (ltake (regexp->string-generator re :*-max-repeat n)
                      32))))

(dolist (n (ltake (generator->lseq (integers$ 16)) 8))
  (test* (format "max-repeat (*) <= ~A" n)
         #t
         (<= (f #/.*/ n) n))
  (test* (format "max-repeat (+) <= ~A" n)
         #t
         (<= (f #/.+/ n) (+ n 1)))
  (test* (format "max-repeat ({3}) <= ~A" n)
         #t
         (<= (f #/.{3}/ n) (+ n 3)))
  )

(let ((u #[a-zA-Z0-9]))
  (test* "char-set-universe: #[a-zA-Z0-9]"
         #t
         (string-every u
                       ((regexp->string-generator #/.{128}/
                                                  :char-set-universe u))))
  (test* "char-set-universe: #[a-zA-Z0-9&&[^A-Z]]"
         #t
         (string-every (char-set-difference u #[A-Z])
                       ((regexp->string-generator #/[^A-Z]{128}/
                                                  :char-set-universe u)))))

(test-end :exit-on-failure #t)
