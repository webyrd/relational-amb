(load "mk-vicare.scm")
(load "mk.scm")

(define evalo
  (lambda (expr val)
    (eval-expro expr '() val)))

(define eval-expro
  (lambda (expr env val)
    (conde
      [(fresh (datum)
         (== (list 'quote datum) expr)
         (== datum val))]
      [(symbolo expr) (lookupo expr env val)]
      [(fresh (e*)
         (== (cons 'list e*) expr)
         (eval-listo e* env val))]
      [(fresh (x e)
         (== (list 'lambda (list x) e) expr)
         (== (list 'closure x e env) val))]
      [(fresh (e*)
         (== (cons 'amb e*) expr)
         (ambo e* env val))]
      [(fresh (e1 e2 x e env^ v)
         (== (list e1 e2) expr)
         (eval-expro e1 env (list 'closure x e env^))
         (eval-expro e2 env v)
         (eval-expro e (cons (cons x v) env^) val))])))

(define lookupo
  (lambda (x env val)
    (fresh (y v rest)
      (== (cons (cons y v) rest) env)
      (conde
        [(== x y) (== v val)]
        [(=/= x y) (lookupo x rest val)]))))

(define ambo
  (lambda (e* env val)
    (fresh (e rest)
      (== (cons e rest) e*)
      (conde
        [(eval-expro e env val)]
        [(ambo rest env val)]))))

(define eval-listo
  (lambda (e* env val)
    (conde
      [(== '() e*) (== '() val)]
      [(fresh (e e-rest v res)
         (== (cons e e-rest) e*)
         (== (cons v res) val)
         (eval-expro e env v)
         (eval-listo e-rest env res))])))
