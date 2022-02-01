(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")
(load "my-interp.scm")

(test "my-interp-1"
  (run 1 (q) (evalo '((lambda (z) z) (lambda (v) v)) q))
  '((closure v v ())))

(test "my-interp-2"
  (run 1 (q) (evalo '(amb) q))
  '())

(test "my-interp-3"
  (run 2 (q) (evalo '(amb 'cat 'dog) q))
  '(cat dog))

(test "my-interp-4"
  (run 2 (q) (evalo '(amb 'cat
                          ((lambda (x) x))
                          'dog)
                    q))
  '(cat dog))

(test "my-interp-5"
  (run 2 (q) (evalo '(amb
                      'cat
                      ((lambda (x) (x x)) (lambda (x) (x x)))
                      'dog)
                    q))
  '(cat dog))
