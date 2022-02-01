;; Modified version of the quines-generating interpreter from the 2012
;; Scheme Workshop paper, 'miniKanren, Live and Untagged: Quine
;; Generation via Relational Interpreters (Programming Pearl)'
;; (http://webyrd.net/quines/quines.pdf)

(define evalo
  (lambda (expr val)
    (eval-expo expr '() val)))

(define eval-expo
  (lambda (expr env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) expr)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== v val)))
      ((fresh (a*)
         (== `(list . ,a*) expr)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (proper-listo a* env val)))
      ((symbolo expr) (lookupo expr env val))
      ((fresh (e*)
         (== `(amb . ,e*) expr)
         (not-in-envo 'amb env)
         (ambo e* env val)))      
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) expr)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(closure ,x ,body ,env) val))))))

(define (ambo e* env val)
  (fresh (e e-rest)
    (== `(,e . ,e-rest) e*)
    (conde
      ((eval-expo e env val))
      ((ambo e-rest env val)))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== '() env)))))

(define proper-listo
  (lambda (expr env val)
    (conde
      ((== '() expr)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) expr)
         (== `(,t-a . ,t-d) val)
         (eval-expo a env t-a)
         (proper-listo d env t-d))))))

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))
