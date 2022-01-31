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

(test "interp-simple-with-amb-13"
  (run* (q) (evalo '(let ((l1 '(cat dog rat fox))
                          (l2 '(mouse fox hen cat))
                          (require
                           ;; this definition of 'require' is
                           ;; adapted from SICP (see examples
                           ;; below)
                           (lambda (p)
                             (if (not p)
                                 (amb)
                                 'ignore))))
                      (letrec ((pick-one (lambda (l)
                                           (let ((_ (require (not (null? l)))))
                                             (amb (car l)
                                                  (pick-one (cdr l)))))))
                        (let ((choice-1 (pick-one l1))
                              (choice-2 (pick-one l2)))
                          (let ((_ (require (equal? choice-1 choice-2))))
                            (list choice-1 choice-2)))))
                   q))
  '((fox fox)
    (cat cat)))

(test "interp-simple-with-amb-14"
  (run* (q) (evalo '(let ((flip-coin (lambda () (amb 'heads 'tails))))
                      (flip-coin))
                   q))
  '(heads
    tails))

(test "interp-simple-with-amb-15"
  (run* (q) (evalo '(let ((require
                           ;; this definition of 'require' is
                           ;; adapted from SICP (see examples
                           ;; below)
                           (lambda (p)
                             (if (not p)
                                 (amb)
                                 'ignore)))
                          (flip-coin (lambda () (amb 'heads 'tails))))
                      (let ((toss-1 (flip-coin))
                            (toss-2 (flip-coin)))
                        (let ((_ (require (equal? toss-1 toss-2))))
                          (list toss-1 toss-2))))
                   q))
  '((heads heads)
    (tails tails)))

(test "interp-simple-with-amb-16"
  (run* (q) (evalo '(let ((error (lambda () (car)))
                          (flip-coin (lambda () (amb 'heads 'tails))))
                      (let ((toss-1 (flip-coin))
                            (toss-2 (flip-coin)))
                        (if (equal? toss-1 toss-2)
                            (list toss-1 toss-2)
                            (error))))
                   q))
  '((heads heads)
    (tails tails)))

(test "interp-simple-with-amb-17"
  ;; In the relational interpreter, we don't need 'amb' to implement
  ;; 'require'.  However, we do need 'amb' to implement 'flip-coin',
  ;; unless we want to use an explicit logic variable within the
  ;; Scheme code.  'amb' gives us non-determinism within ground Scheme
  ;; programs.
  (run* (q) (evalo '(let ((error (lambda () (car))))
                      (let ((require
                             ;; alternate definition of 'require',
                             ;; invoking an error rather than 'amb'
                             (lambda (p)
                               (if (not p)
                                   (error)
                                   'ignore)))
                            (flip-coin (lambda () (amb 'heads 'tails))))
                        (let ((toss-1 (flip-coin))
                              (toss-2 (flip-coin)))
                          (let ((_ (require (equal? toss-1 toss-2))))
                            (list toss-1 toss-2)))))
                   q))
  '((heads heads)
    (tails tails)))

(test "interp-simple-with-amb-18"
  ;; Replacing 'amb' with a logic variable and 'if'.  However, this
  ;; doesn't work with recursive uses of 'amb', unless we play
  ;; trickier games.  'amb' is more convenient than a fresh logic
  ;; variable for expressing non-determinism is Scheme code in the
  ;; relational interpreter.
  (run* (q)
    (fresh (test)
      (evalo `(let ((error (lambda () (car))))
                (let ((require
                       ;; alternate definition of 'require',
                       ;; invoking an error rather than 'amb'
                       (lambda (p)
                         (if (not p)
                             (error)
                             'ignore)))
                      (flip-coin (lambda () (if ',test 'heads 'tails))))
                  (let ((toss-1 (flip-coin))
                        (toss-2 (flip-coin)))
                    (let ((_ (require (equal? toss-1 toss-2))))
                      (list toss-1 toss-2)))))
             q)))
  '((heads heads)
    (tails tails)))

(test "interp-simple-with-amb-19"
  ;; This use of 'amb' is awkward to replace by an 'if' and a fresh
  ;; logic variable, due to the recursive use of 'amb'.
  (run* (q) (evalo '(let ((error (lambda () (car))))
                      (let ((require
                             ;; alternate definition of 'require',
                             ;; invoking an error rather than 'amb'
                             (lambda (p)
                               (if (not p)
                                   (error)
                                   'ignore))))
                        (letrec ((select-from-list
                                  (lambda (l)
                                    (let ((_ (require (not (null? l)))))
                                      (amb (car l)
                                           (select-from-list (cdr l)))))))
                          (let ((l1 '(cat dog rat fish fox))
                                (l2 '(bat fox wolf cat squirrel)))
                            (let ((animal-1 (select-from-list l1))
                                  (animal-2 (select-from-list l2)))
                              (let ((_ (require (equal? animal-1 animal-2))))
                                (list animal-1 animal-2)))))))
                   q))
  '((fox fox)
    (cat cat)))


(test "type-inferencer-amb-1"
  (run 1 (q)
    (fresh (T1)
      (evalo `(let ((require
                     (lambda (p)
                       (if (not p)
                           (amb)
                           'ignore))))
                (letrec ((lookup (lambda (x gamma)
                                   (let ((_ (require (not (null? gamma)))))
                                     (match gamma
                                       [`((,y . ,t) . ,gamma^)
                                        (if (equal? x y)
                                            t
                                            (lookup x gamma^))]))))
                         (!- (lambda (gamma t)
                               (amb (!-var gamma t)
                                    (!-abs gamma t)
                                    (!-app gamma t))))
                         (!-var (lambda (gamma t)
                                  (let ((_ (require (symbol? t))))
                                    (lookup t gamma))))
                         (!-abs (lambda (gamma t)
                                  (match t
                                    [`(lambda (,x) ,e)
                                     (let ((t1 ,T1))
                                       (let ((t2 (!- `((,x . ,t1) . ,gamma) e)))
                                         (list t1 '-> t2)))]
                                    (else (amb)))))
                         (!-app (lambda (gamma t)
                                  (match t
                                    [`(,e1 ,e2)
                                     (let ((t1 (!- gamma e2)))
                                       (match (!- gamma e1)
                                         [`(,t2 -> ,t3)
                                          (let ((_ (require (equal? t1 t2))))
                                            t3)]))]
                                    (else (amb))))))                  
                  (!- '() '(lambda (z) (lambda (w) (lambda (v) (v w)))))))
             q)))
  '(((_.0 -> (_.0 -> ((_.0 -> _.1) -> _.1)))
     (num _.0)
     (absento (closure _.1) (primitive _.1)))))

(test "type-inferencer-amb-2"
  (run 1 (q)
    (fresh (T1)
      (evalo `(let ((require
                     (lambda (p)
                       (if (not p)
                           (amb)
                           'ignore))))
                (letrec ((lookup (lambda (x gamma)
                                   (let ((_ (require (not (null? gamma)))))
                                     (match gamma
                                       [`((,y . ,t) . ,gamma^)
                                        (if (equal? x y)
                                            t
                                            (lookup x gamma^))]))))
                         (!- (lambda (gamma t)
                               (match t
                                 [`(lambda (,x) ,e)
                                  (let ((t1 ,T1))
                                    (let ((t2 (!- `((,x . ,t1) . ,gamma) e)))
                                      (list t1 '-> t2)))]
                                 [`(,e1 ,e2)
                                  (let ((t1 (!- gamma e2)))
                                    (match (!- gamma e1)
                                      [`(,t2 -> ,t3)
                                       (let ((_ (require (equal? t1 t2))))
                                         t3)]))]
                                 [else (let ((_ (require (symbol? t))))
                                         (lookup t gamma))]))))
                  (!- '() '(lambda (z) (lambda (w) (lambda (v) (v w)))))))
             q)))
  '(((_.0 -> (_.0 -> ((_.0 -> _.1) -> _.1)))
     (num _.0)
     (absento (closure _.1) (primitive _.1)))))

(test "type-inferencer-3"
  (run 1 (q)
    (fresh (T1)
      (evalo `(let ((error (lambda () (car))))
                (letrec ((lookup (lambda (x gamma)
                                   (match gamma
                                     [`() (error)]
                                     [`((,y . ,t) . ,gamma^)
                                      (if (equal? x y)
                                          t
                                          (lookup x gamma^))])))
                         (!- (lambda (gamma t)
                               (match t
                                 [`(lambda (,x) ,e)
                                  (let ((t1 ,T1))
                                    (let ((t2 (!- `((,x . ,t1) . ,gamma) e)))
                                      (list t1 '-> t2)))]
                                 [`(,e1 ,e2)
                                  (let ((t1 (!- gamma e2)))
                                    (match (!- gamma e1)
                                      [`(,t2 -> ,t3)
                                       (if (equal? t1 t2)
                                           t3
                                           (error))]))]
                                 [else (if (symbol? t)
                                           (lookup t gamma)
                                           (error))]))))
                  (!- '() '(lambda (z) (lambda (w) (lambda (v) (v w)))))))
             q)))
  '(((_.0 -> (_.0 -> ((_.0 -> _.1) -> _.1)))
     (num _.0)
     (absento (closure _.1) (primitive _.1)))))

(test "type-inferencer-4"
  (run 1 (q T1)
    (evalo `(let ((error (lambda () (car))))
              (letrec ((lookup (lambda (x gamma)
                                 (match gamma
                                   [`() (error)]
                                   [`((,y . ,t) . ,gamma^)
                                    (if (equal? x y)
                                        t
                                        (lookup x gamma^))])))
                       (!- (lambda (gamma t)
                             (match t
                               [`(lambda (,x) ,e)
                                (let ((t1 ,T1))
                                  (let ((t2 (!- `((,x . ,t1) . ,gamma) e)))
                                    (list t1 '-> t2)))]
                               [`(,e1 ,e2)
                                (let ((t1 (!- gamma e2)))
                                  (match (!- gamma e1)
                                    [`(,t2 -> ,t3)
                                     (if (equal? t1 t2)
                                         t3
                                         (error))]))]
                               [else (if (symbol? t)
                                         (lookup t gamma)
                                         (error))]))))
                (!- '() '(lambda (z) (lambda (w) (lambda (v) (v w)))))))
           q))
  '((((_.0 -> (_.0 -> ((_.0 -> _.1) -> _.1)))
      (amb _.0 '(_.0 -> _.1) . _.2))
     (num _.0)
     (absento (closure _.1) (primitive _.1)))))


;; Tests/examples from SICP:

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node88.html
;; Variations on a Scheme--Nondeterministic Computing 


;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node89.html
;; Amb and Search 

(test "interp-simple-with-amb-sicp-1"
  (run* (q) (evalo '(list (amb 1 2 3) (amb 'a 'b)) q))
  '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b)))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node90.html
;; Examples of Nondeterministic Programs

(test "grammar-1"
  (run* (v)
    (evalo
     `(let ((nouns '(noun student professor cat class))
            (verbs '(verb studies lectures eats sleeps))
            (articles '(article the a))
            ;;
            (require
             (lambda (p)
               (if (not p)
                   (amb)
                   'ignore))))
        (letrec ((member
                  (lambda (x ls)
                    (cond
                      ((null? ls) #f)
                      ((equal? (car ls) x) ls)
                      (else (member x (cdr ls))))))                 
                 (parse-sentence
                  (lambda (unparsed)
                    (match (parse-noun-phrase unparsed)
                      [`(,np . ,unparsed)
                       (match (parse-word verbs unparsed)
                         [`(,v . ,unparsed)
                          (cons
                           (list 'sentence
                                 np
                                 v)
                           unparsed)])])))
                 (parse-noun-phrase
                  (lambda (unparsed)
                    (match (parse-word articles unparsed)
                      [`(,a . ,unparsed)
                       (match (parse-word nouns unparsed)
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
          (parse '(the cat eats))))
     v))
  '((sentence
     (noun-phrase (article the) (noun cat))
     (verb eats))))



(test "grammar-2"
  (run* (v)
    (evalo
     `(let ((nouns '(noun student professor cat class))
            (verbs '(verb studies lectures eats sleeps))
            (articles '(article the a))
            (prepositions '(prep for to in by with))
            ;;
            (require
             (lambda (p)
               (if (not p)
                   (amb)
                   'ignore))))
        (letrec ((member
                  (lambda (x ls)
                    (cond
                      ((null? ls) #f)
                      ((equal? (car ls) x) ls)
                      (else (member x (cdr ls))))))
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
                    (match (parse-word articles unparsed)
                      [`(,a . ,unparsed)
                       (match (parse-word nouns unparsed)
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
                    (match (parse-word prepositions unparsed)
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
                      (match (parse-word verbs unparsed)
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
          (parse '(the student with the cat sleeps in the class))))
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
     `(let ((nouns '(noun student professor cat class))
            (verbs '(verb studies lectures eats sleeps))
            (articles '(article the a))
            (prepositions '(prep for to in by with))
            ;;
            (require
             (lambda (p)
               (if (not p)
                   (amb)
                   'ignore))))
        (letrec ((member
                  (lambda (x ls)
                    (cond
                      ((null? ls) #f)
                      ((equal? (car ls) x) ls)
                      (else (member x (cdr ls))))))
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
                    (match (parse-word articles unparsed)
                      [`(,a . ,unparsed)
                       (match (parse-word nouns unparsed)
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
                    (match (parse-word prepositions unparsed)
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
                      (match (parse-word verbs unparsed)
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
          (parse '(the professor lectures to the student with the cat))))
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
(test "grammar-4"
  (length
   (run* (v)
     (evalo
      `(let ((nouns '(noun student professor cat class))
             (verbs '(verb studies lectures eats sleeps))
             (articles '(article the a))
             (prepositions '(prep for to in by with))
             ;;
             (require
              (lambda (p)
                (if (not p)
                    (amb)
                    'ignore))))
         (letrec ((member
                   (lambda (x ls)
                     (cond
                       ((null? ls) #f)
                       ((equal? (car ls) x) ls)
                       (else (member x (cdr ls))))))
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
                     (match (parse-word articles unparsed)
                       [`(,a . ,unparsed)
                        (match (parse-word nouns unparsed)
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
                     (match (parse-word prepositions unparsed)
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
                       (match (parse-word verbs unparsed)
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
           (parse '(the professor lectures to the student in the class with the cat))))
      v)))
  5)

;; Relational interpreter version of the Alyssa P. Hacker exercise at
;; the end of 'Examples of Nondeterministic Programs':
;;
;; We generate sentence/parse pairs.
(test "grammar-generate-1"
  (run 6 (s v)
    (evalo
     `(let ((nouns '(noun student professor cat class))
            (verbs '(verb studies lectures eats sleeps))
            (articles '(article the a))
            (prepositions '(prep for to in by with))
            ;;
            (require
             (lambda (p)
               (if (not p)
                   (amb)
                   'ignore))))
        (letrec ((member
                  (lambda (x ls)
                    (cond
                      ((null? ls) #f)
                      ((equal? (car ls) x) ls)
                      (else (member x (cdr ls))))))
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
                    (match (parse-word articles unparsed)
                      [`(,a . ,unparsed)
                       (match (parse-word nouns unparsed)
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
                    (match (parse-word prepositions unparsed)
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
                      (match (parse-word verbs unparsed)
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
          (parse ',s)))
     v))
  '(((the student studies)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)))
    ((the student lectures)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb lectures)))
    ((a student studies)
     (sentence (simple-noun-phrase (article a) (noun student)) (verb studies)))
    ((the student eats)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb eats)))
    ((a student lectures)
     (sentence (simple-noun-phrase (article a) (noun student)) (verb lectures)))
    ((the student sleeps)
     (sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps)))))
