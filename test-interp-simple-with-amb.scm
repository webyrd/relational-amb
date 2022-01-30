(load "mk-vicare.scm")
(load "mk.scm")
(load "interp-simple-with-amb.scm")
(load "test-check.scm")

(test "interp-simple-with-amb-1"
  (run 1 (q) (evalo '(amb) q))
  '())

(test "interp-simple-with-amb-2"
  (run 1 (q) (evalo '(amb 5) q))
  '(5))

(test "interp-simple-with-amb-3"
  (run 1 (q) (evalo '(amb 5 6) q))
  '(5))

(test "interp-simple-with-amb-4"
  (run* (q) (evalo '(amb 5 6) q))
  '(5 6))

(test "interp-simple-with-amb-5"
  (run* (q) (evalo '(amb 5 6 7 8) q))
  '(5 6 7 8))

(test "interp-simple-with-amb-6"
  (run* (q) (evalo '(amb (list 1 2) 6 7 8) q))
  '(6 7 8 (1 2)))

(test "interp-simple-with-amb-7"
  (run* (q) (evalo '(amb (list 1 2) 6 7 (list 'cat 'dog)) q))
  '(6 7 (1 2) (cat dog)))

(test "interp-simple-with-amb-8"
  (run* (q) (evalo '(amb (list 1 2) 6 7 (list 'cat 'dog)) q))
  '(6 7 (1 2) (cat dog)))

(test "interp-simple-with-amb-9"
  (run* (q) (evalo '(amb (list 1 2) 6 7 (list 'cat 'dog)) q))
  '(6 7 (1 2) (cat dog)))


(test "interp-simple-with-amb-10"
  (run 1 (q) (evalo '(amb ((lambda (x) (x x)) (lambda (x) (x x)))
                         5)
                   q))
  '(5))

(test "interp-simple-with-amb-11"
  (run 1 (q) (evalo '(amb ((lambda (x) (x x)) (lambda (x) (x x)))
                          ((lambda (x) (x x)) (lambda (x) (x x)))
                          ((lambda (x) (x x)) (lambda (x) (x x)))
                          5
                          ((lambda (x) (x x)) (lambda (x) (x x)))
                          ((lambda (x) (x x)) (lambda (x) (x x)))
                          ((lambda (x) (x x)) (lambda (x) (x x))))
                    q))
  '(5))

(test "interp-simple-with-amb-12"
  (run* (q) (evalo '(amb (cons 3)
                          5
                          (car 4)
                          (cdr 6))
                    q))
  '(5))


;; Tests/examples from SICP:

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node88.html
;; Variations on a Scheme--Nondeterministic Computing 


;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node89.html
;; Amb and Search 

(test "interp-simple-with-amb--sicp-1"
  (run* (q) (evalo '(list (amb 1 2 3) (amb 'a 'b)) q))
  '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b)))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node90.html
;; Examples of Nondeterministic Programs


#|
(define require
  (lambda (p)
    (if (not p)
        (amb)
        'ignore)))
|#

(test "dummy-require-1"
  (run* (v)
    (evalo
     `(letrec ((require
                (lambda (_)
                  _))
               (member
                (lambda (x ls)
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) x) ls)
                    (else (member x (cdr ls))))))
               (nouns
                (lambda ()
                  '(noun student professor cat class)))
               (verbs
                (lambda ()
                  '(verb studies lectures eats sleeps)))
               (articles
                (lambda ()
                  '(article the a)))
               (parse-sentence
                (lambda (unparsed)
                  (match (parse-noun-phrase unparsed)
                    [`(,np . ,unparsed)
                     (match (parse-word (verbs) unparsed)
                       [`(,v . ,unparsed)
                        (cons
                         (list 'sentence
                               np
                               v)
                         unparsed)])])))
               (parse-noun-phrase
                (lambda (unparsed)
                  (match (parse-word (articles) unparsed)
                    [`(,a . ,unparsed)
                     (match (parse-word (nouns) unparsed)
                       [`(,n . ,unparsed)
                        (cons
                         (list 'noun-phrase
                               a
                               n)
                         unparsed)])])))
               (parse-word
                (lambda (word-list unparsed)
                  (let ((_ (require (not (null? unparsed)))))
                    (let ((_ (require (member (car unparsed) (cdr word-list)))))
                      (match unparsed
                        [`(,found-word . ,unparsed)
                         (cons
                          (list (car word-list) found-word)
                          unparsed)])))))
               (parse
                (lambda (input)
                  (let ((unparsed input))
                    (match (parse-sentence unparsed)
                      [`(,sent . ,unparsed)
                       (let ((_ (require (null? unparsed))))
                         sent)])))))
        (parse '(the cat eats)))
     v))
  '((sentence
     (noun-phrase (article the) (noun cat))
     (verb eats))))

(test "grammar-1"
  (run* (v)
    (evalo
     `(letrec ((require
                (lambda (p)
                  (if (not p)
                      (amb)
                      'ignore)))
               (member
                (lambda (x ls)
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) x) ls)
                    (else (member x (cdr ls))))))
               (nouns
                (lambda ()
                  '(noun student professor cat class)))
               (verbs
                (lambda ()
                  '(verb studies lectures eats sleeps)))
               (articles
                (lambda ()
                  '(article the a)))
               (parse-sentence
                (lambda (unparsed)
                  (match (parse-noun-phrase unparsed)
                    [`(,np . ,unparsed)
                     (match (parse-word (verbs) unparsed)
                       [`(,v . ,unparsed)
                        (cons
                         (list 'sentence
                               np
                               v)
                         unparsed)])])))
               (parse-noun-phrase
                (lambda (unparsed)
                  (match (parse-word (articles) unparsed)
                    [`(,a . ,unparsed)
                     (match (parse-word (nouns) unparsed)
                       [`(,n . ,unparsed)
                        (cons
                         (list 'noun-phrase
                               a
                               n)
                         unparsed)])])))
               (parse-word
                (lambda (word-list unparsed)
                  (let ((_ (require (not (null? unparsed)))))
                    (let ((_ (require (member (car unparsed) (cdr word-list)))))
                      (match unparsed
                        [`(,found-word . ,unparsed)
                         (cons
                          (list (car word-list) found-word)
                          unparsed)])))))
               (parse
                (lambda (input)
                  (let ((unparsed input))
                    (match (parse-sentence unparsed)
                      [`(,sent . ,unparsed)
                       (let ((_ (require (null? unparsed))))
                         sent)])))))
        (parse '(the cat eats)))
     v))
  '((sentence
     (noun-phrase (article the) (noun cat))
     (verb eats))))



(test "grammar-2"
  (run* (v)
    (evalo
     `(letrec ((require
                (lambda (p)
                  (if (not p)
                      (amb)
                      'ignore)))
               (member
                (lambda (x ls)
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) x) ls)
                    (else (member x (cdr ls))))))
               (nouns
                (lambda ()
                  '(noun student professor cat class)))
               (verbs
                (lambda ()
                  '(verb studies lectures eats sleeps)))
               (articles
                (lambda ()
                  '(article the a)))
               (prepositions
                (lambda ()
                  '(prep for to in by with)))
               (parse-sentence
                (lambda (unparsed)
                  (match (parse-noun-phrase unparsed)
                    [`(,np . ,unparsed)
                     (match (parse-verb-phrase unparsed)
                       [`(,vp . ,unparsed)
                        (cons
                         (list 'sentence
                               np
                               vp)
                         unparsed)])])))
               (parse-simple-noun-phrase
                (lambda (unparsed)
                  (match (parse-word (articles) unparsed)
                    [`(,a . ,unparsed)
                     (match (parse-word (nouns) unparsed)
                       [`(,n . ,unparsed)
                        (cons
                         (list 'simple-noun-phrase
                               a
                               n)
                         unparsed)])])))
               (parse-noun-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (noun-phrase unparsed)
                              (amb (cons
                                    noun-phrase
                                    unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'noun-phrase
                                                          noun-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-simple-noun-phrase unparsed)
                      [`(,np . ,unparsed)
                       (maybe-extend np unparsed)]))))
               (parse-prepositional-phrase
                (lambda (unparsed)
                  (match (parse-word (prepositions) unparsed)
                    [`(,prep . ,unparsed)
                     (match (parse-noun-phrase unparsed)
                       [`(,np . ,unparsed)
                        (cons
                         (list 'prep-phrase
                               prep
                               np)
                         unparsed)])])))
               (parse-verb-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (verb-phrase unparsed)
                              (amb (cons verb-phrase
                                         unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'verb-phrase
                                                          verb-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-word (verbs) unparsed)
                      [`(,v . ,unparsed)
                       (maybe-extend v unparsed)]))))
               (parse-word
                (lambda (word-list unparsed)
                  (let ((_ (require (not (null? unparsed)))))
                    (let ((_ (require (member (car unparsed) (cdr word-list)))))
                      (match unparsed
                        [`(,found-word . ,unparsed)
                         (cons
                          (list (car word-list) found-word)
                          unparsed)])))))
               (parse
                (lambda (input)
                  (let ((unparsed input))
                    (match (parse-sentence unparsed)
                      [`(,sent . ,unparsed)
                       (let ((_ (require (null? unparsed))))
                         sent)])))))
        (parse '(the student with the cat sleeps in the class)))
     v))
  '((sentence
     (noun-phrase
      (simple-noun-phrase (article the) (noun student))
      (prep-phrase (prep with)
                   (simple-noun-phrase
                    (article the) (noun cat))))
     (verb-phrase
      (verb sleeps)
      (prep-phrase (prep in)
                   (simple-noun-phrase
                    (article the) (noun class)))))))


(test "grammar-3"
  (run* (v)
    (evalo
     `(letrec ((require
                (lambda (p)
                  (if (not p)
                      (amb)
                      'ignore)))
               (member
                (lambda (x ls)
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) x) ls)
                    (else (member x (cdr ls))))))
               (nouns
                (lambda ()
                  '(noun student professor cat class)))
               (verbs
                (lambda ()
                  '(verb studies lectures eats sleeps)))
               (articles
                (lambda ()
                  '(article the a)))
               (prepositions
                (lambda ()
                  '(prep for to in by with)))
               (parse-sentence
                (lambda (unparsed)
                  (match (parse-noun-phrase unparsed)
                    [`(,np . ,unparsed)
                     (match (parse-verb-phrase unparsed)
                       [`(,vp . ,unparsed)
                        (cons
                         (list 'sentence
                               np
                               vp)
                         unparsed)])])))
               (parse-simple-noun-phrase
                (lambda (unparsed)
                  (match (parse-word (articles) unparsed)
                    [`(,a . ,unparsed)
                     (match (parse-word (nouns) unparsed)
                       [`(,n . ,unparsed)
                        (cons
                         (list 'simple-noun-phrase
                               a
                               n)
                         unparsed)])])))
               (parse-noun-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (noun-phrase unparsed)
                              (amb (cons
                                    noun-phrase
                                    unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'noun-phrase
                                                          noun-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-simple-noun-phrase unparsed)
                      [`(,np . ,unparsed)
                       (maybe-extend np unparsed)]))))
               (parse-prepositional-phrase
                (lambda (unparsed)
                  (match (parse-word (prepositions) unparsed)
                    [`(,prep . ,unparsed)
                     (match (parse-noun-phrase unparsed)
                       [`(,np . ,unparsed)
                        (cons
                         (list 'prep-phrase
                               prep
                               np)
                         unparsed)])])))
               (parse-verb-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (verb-phrase unparsed)
                              (amb (cons verb-phrase
                                         unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'verb-phrase
                                                          verb-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-word (verbs) unparsed)
                      [`(,v . ,unparsed)
                       (maybe-extend v unparsed)]))))
               (parse-word
                (lambda (word-list unparsed)
                  (let ((_ (require (not (null? unparsed)))))
                    (let ((_ (require (member (car unparsed) (cdr word-list)))))
                      (match unparsed
                        [`(,found-word . ,unparsed)
                         (cons
                          (list (car word-list) found-word)
                          unparsed)])))))
               (parse
                (lambda (input)
                  (let ((unparsed input))
                    (match (parse-sentence unparsed)
                      [`(,sent . ,unparsed)
                       (let ((_ (require (null? unparsed))))
                         sent)])))))
        (parse '(the professor lectures to the student with the cat)))
     v))
  '((sentence
     (simple-noun-phrase (article the) (noun professor))
     (verb-phrase
      (verb-phrase
       (verb lectures)
       (prep-phrase (prep to)
                    (simple-noun-phrase
                     (article the) (noun student))))
      (prep-phrase (prep with)
                   (simple-noun-phrase
                    (article the) (noun cat)))))
    (sentence
     (simple-noun-phrase (article the) (noun professor))
     (verb-phrase
      (verb lectures)
      (prep-phrase (prep to)
                   (noun-phrase
                    (simple-noun-phrase
                     (article the) (noun student))
                    (prep-phrase (prep with)
                                 (simple-noun-phrase
                                  (article the) (noun cat)))))))))

;; From an exercise at the end of 'Examples of Nondeterministic
;; Programs':
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node90.html
(test "grammar-3"
  (length
   (run* (v)
     (evalo
      `(letrec ((require
                 (lambda (p)
                   (if (not p)
                       (amb)
                       'ignore)))
                (member
                 (lambda (x ls)
                   (cond
                     ((null? ls) #f)
                     ((equal? (car ls) x) ls)
                     (else (member x (cdr ls))))))
                (nouns
                 (lambda ()
                   '(noun student professor cat class)))
                (verbs
                 (lambda ()
                   '(verb studies lectures eats sleeps)))
                (articles
                 (lambda ()
                   '(article the a)))
                (prepositions
                 (lambda ()
                   '(prep for to in by with)))
                (parse-sentence
                 (lambda (unparsed)
                   (match (parse-noun-phrase unparsed)
                     [`(,np . ,unparsed)
                      (match (parse-verb-phrase unparsed)
                        [`(,vp . ,unparsed)
                         (cons
                          (list 'sentence
                                np
                                vp)
                          unparsed)])])))
                (parse-simple-noun-phrase
                 (lambda (unparsed)
                   (match (parse-word (articles) unparsed)
                     [`(,a . ,unparsed)
                      (match (parse-word (nouns) unparsed)
                        [`(,n . ,unparsed)
                         (cons
                          (list 'simple-noun-phrase
                                a
                                n)
                          unparsed)])])))
                (parse-noun-phrase
                 (lambda (unparsed)
                   (letrec ((maybe-extend
                             (lambda (noun-phrase unparsed)
                               (amb (cons
                                     noun-phrase
                                     unparsed)
                                    (match (parse-prepositional-phrase unparsed)
                                      [`(,prepp . ,unparsed)
                                       (maybe-extend (list 'noun-phrase
                                                           noun-phrase
                                                           prepp)
                                                     unparsed)])))))
                     (match (parse-simple-noun-phrase unparsed)
                       [`(,np . ,unparsed)
                        (maybe-extend np unparsed)]))))
                (parse-prepositional-phrase
                 (lambda (unparsed)
                   (match (parse-word (prepositions) unparsed)
                     [`(,prep . ,unparsed)
                      (match (parse-noun-phrase unparsed)
                        [`(,np . ,unparsed)
                         (cons
                          (list 'prep-phrase
                                prep
                                np)
                          unparsed)])])))
                (parse-verb-phrase
                 (lambda (unparsed)
                   (letrec ((maybe-extend
                             (lambda (verb-phrase unparsed)
                               (amb (cons verb-phrase
                                          unparsed)
                                    (match (parse-prepositional-phrase unparsed)
                                      [`(,prepp . ,unparsed)
                                       (maybe-extend (list 'verb-phrase
                                                           verb-phrase
                                                           prepp)
                                                     unparsed)])))))
                     (match (parse-word (verbs) unparsed)
                       [`(,v . ,unparsed)
                        (maybe-extend v unparsed)]))))
                (parse-word
                 (lambda (word-list unparsed)
                   (let ((_ (require (not (null? unparsed)))))
                     (let ((_ (require (member (car unparsed) (cdr word-list)))))
                       (match unparsed
                         [`(,found-word . ,unparsed)
                          (cons
                           (list (car word-list) found-word)
                           unparsed)])))))
                (parse
                 (lambda (input)
                   (let ((unparsed input))
                     (match (parse-sentence unparsed)
                       [`(,sent . ,unparsed)
                        (let ((_ (require (null? unparsed))))
                          sent)])))))
         (parse '(the professor lectures to the student in the class with the cat)))
      v)))
  5)

;; Relational interpreter version of the Alyssa P. Hacker exercise at
;; the end of 'Examples of Nondeterministic Programs':
;;
;; We generate sentence/parse pairs.
(test "grammar-generate-1"
  (run 6 (s v)
    (evalo
     `(letrec ((require
                (lambda (p)
                  (if (not p)
                      (amb)
                      'ignore)))
               (member
                (lambda (x ls)
                  (cond
                    ((null? ls) #f)
                    ((equal? (car ls) x) ls)
                    (else (member x (cdr ls))))))
               (nouns
                (lambda ()
                  '(noun student professor cat class)))
               (verbs
                (lambda ()
                  '(verb studies lectures eats sleeps)))
               (articles
                (lambda ()
                  '(article the a)))
               (prepositions
                (lambda ()
                  '(prep for to in by with)))
               (parse-sentence
                (lambda (unparsed)
                  (match (parse-noun-phrase unparsed)
                    [`(,np . ,unparsed)
                     (match (parse-verb-phrase unparsed)
                       [`(,vp . ,unparsed)
                        (cons
                         (list 'sentence
                               np
                               vp)
                         unparsed)])])))
               (parse-simple-noun-phrase
                (lambda (unparsed)
                  (match (parse-word (articles) unparsed)
                    [`(,a . ,unparsed)
                     (match (parse-word (nouns) unparsed)
                       [`(,n . ,unparsed)
                        (cons
                         (list 'simple-noun-phrase
                               a
                               n)
                         unparsed)])])))
               (parse-noun-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (noun-phrase unparsed)
                              (amb (cons
                                    noun-phrase
                                    unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'noun-phrase
                                                          noun-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-simple-noun-phrase unparsed)
                      [`(,np . ,unparsed)
                       (maybe-extend np unparsed)]))))
               (parse-prepositional-phrase
                (lambda (unparsed)
                  (match (parse-word (prepositions) unparsed)
                    [`(,prep . ,unparsed)
                     (match (parse-noun-phrase unparsed)
                       [`(,np . ,unparsed)
                        (cons
                         (list 'prep-phrase
                               prep
                               np)
                         unparsed)])])))
               (parse-verb-phrase
                (lambda (unparsed)
                  (letrec ((maybe-extend
                            (lambda (verb-phrase unparsed)
                              (amb (cons verb-phrase
                                         unparsed)
                                   (match (parse-prepositional-phrase unparsed)
                                     [`(,prepp . ,unparsed)
                                      (maybe-extend (list 'verb-phrase
                                                          verb-phrase
                                                          prepp)
                                                    unparsed)])))))
                    (match (parse-word (verbs) unparsed)
                      [`(,v . ,unparsed)
                       (maybe-extend v unparsed)]))))
               (parse-word
                (lambda (word-list unparsed)
                  (let ((_ (require (not (null? unparsed)))))
                    (let ((_ (require (member (car unparsed) (cdr word-list)))))
                      (match unparsed
                        [`(,found-word . ,unparsed)
                         (cons
                          (list (car word-list) found-word)
                          unparsed)])))))
               (parse
                (lambda (input)
                  (let ((unparsed input))
                    (match (parse-sentence unparsed)
                      [`(,sent . ,unparsed)
                       (let ((_ (require (null? unparsed))))
                         sent)])))))
        (parse ',s))
     v))
  '(((the student studies)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)))
    ((the student lectures)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb lectures)))
    ((the student eats)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb eats)))
    ((a student studies)
     (sentence (simple-noun-phrase (article a) (noun student)) (verb studies)))
    ((a student lectures)
     (sentence (simple-noun-phrase (article a) (noun student)) (verb lectures)))
    ((the student sleeps)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps)))))
