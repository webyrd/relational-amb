(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "quines-interp-with-amb.scm")

(test "1 quine"
  (run 1 (q) (evalo q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))

 (test "2 quines"
   (run 2 (q) (evalo q q))
   '((((lambda (_.0) (list _.0 (list 'quote _.0)))
       '(lambda (_.0) (list _.0 (list 'quote _.0))))
      (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
      (sym _.0))
     (((lambda (_.0) (list (amb _.0 . _.1) (list 'quote _.0)))
       '(lambda (_.0) (list (amb _.0 . _.1) (list 'quote _.0))))
      (=/= ((_.0 amb)) ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
      (sym _.0)
      (absento (closure _.1)))))

(test "amb-quine-forward-1"
  (run* (q)
    (evalo '((lambda (_.0) (list (amb _.0) (list 'quote _.0)))
             '(lambda (_.0) (list (amb _.0) (list 'quote _.0))))
           q))
  '(((lambda (_.0) (list (amb _.0) (list 'quote _.0)))
     '(lambda (_.0) (list (amb _.0) (list 'quote _.0))))))

(test "amb-quine-forward-2"
  (run* (q)
    (evalo '((lambda (x) (list (amb x) (list 'quote x)))
             '(lambda (x) (list (amb x) (list 'quote x))))
           q))
  '(((lambda (x) (list (amb x) (list 'quote x)))
     '(lambda (x) (list (amb x) (list 'quote x))))))

(test "amb-quine-forward-3"
  (run* (q)
    (evalo '((lambda (x) (list (amb x y z) (list 'quote x)))
             '(lambda (x) (list (amb x y z) (list 'quote x))))
           q))  
  '(((lambda (x) (list (amb x y z) (list 'quote x)))
     '(lambda (x) (list (amb x y z) (list 'quote x))))))


(test "structured-amb-quine-1"
  (run 5 (q)
    (fresh (e1 e2)
      (== `((lambda (x) (amb . ,e1)) ,e2) q)
      (evalo q q)))
  '((((lambda (x) (amb (list x (list 'quote x)) . _.0))
      '(lambda (x) (amb (list x (list 'quote x)) . _.0)))
     (absento (closure _.0)))
    (((lambda (x) (amb _.0 (list x (list 'quote x)) . _.1))
      '(lambda (x) (amb _.0 (list x (list 'quote x)) . _.1)))
     (absento (closure _.0) (closure _.1)))
    (((lambda (x)
        (amb (list (amb x . _.0) (list 'quote x)) . _.1))
      '(lambda (x)
         (amb (list (amb x . _.0) (list 'quote x)) . _.1)))
     (absento (closure _.0) (closure _.1)))
    (((lambda (x)
        (amb (list x (list (amb 'quote . _.0) x)) . _.1))
      '(lambda (x)
         (amb (list x (list (amb 'quote . _.0) x)) . _.1)))
     (absento (closure _.0) (closure _.1)))
    (((lambda (x) (amb _.0 _.1 (list x (list 'quote x)) . _.2))
      '(lambda (x) (amb _.0 _.1 (list x (list 'quote x)) . _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)))))


(test "2 twines"
  (run 2 (p q)
    (=/= p q)
    (eval-expo p '() q)
    (eval-expo q '() p))
  '((('((lambda (_.0)
          (list 'quote (list _.0 (list 'quote _.0))))
        '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
      ((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
       '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ;;
    ((;; p
      '((lambda (_.0)
          (list 'quote (list (amb _.0 . _.1) (list 'quote _.0))))
        '(lambda (_.0)
           (list 'quote (list (amb _.0 . _.1) (list 'quote _.0)))))
      ;; q
      ((lambda (_.0)
         (list 'quote (list (amb _.0 . _.1) (list 'quote _.0))))
       '(lambda (_.0)
          (list 'quote (list (amb _.0 . _.1) (list 'quote _.0)))))
      )     
     (=/= ((_.0 amb)) ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))))

(test "3 twines absento"
  (run 3 (p q)
    (absento p q)
    (absento q p)
    (eval-expo p '() q)
    (eval-expo q '() p))
  '((((list
       '(lambda (_.0)
          (list 'list _.0 (list 'quote (list 'quote _.0))))
       '''(lambda (_.0)
            (list 'list _.0 (list 'quote (list 'quote _.0)))))
      ((lambda (_.0)
         (list 'list _.0 (list 'quote (list 'quote _.0))))
       ''(lambda (_.0)
           (list 'list _.0 (list 'quote (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ;;
    (((list
       '(lambda (_.0)
          (list
           'list
           (list 'quote _.0)
           (list 'quote (list 'quote _.0))))
       ''(lambda (_.0)
           (list
            'list
            (list 'quote _.0)
            (list 'quote (list 'quote _.0)))))
      ((lambda (_.0)
         (list
          'list
          (list 'quote _.0)
          (list 'quote (list 'quote _.0))))
       '(lambda (_.0)
          (list
           'list
           (list 'quote _.0)
           (list 'quote (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ;;
    ((;; p
      (list
       '(lambda (_.0)
          (list
           'list
           _.0
           (list (amb 'quote . _.1) (list 'quote _.0))))
       '''(lambda (_.0)
            (list
             'list
             _.0
             (list (amb 'quote . _.1) (list 'quote _.0)))))
      ;; q
      ((lambda (_.0)
         (list
          'list
          _.0
          (list (amb 'quote . _.1) (list 'quote _.0))))
       ''(lambda (_.0)
           (list
            'list
            _.0
            (list (amb 'quote . _.1) (list 'quote _.0)))))
      )
     (=/= ((_.0 amb)) ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))))
