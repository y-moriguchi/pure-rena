(define match
  (lambda (dest)
    (lambda (lst attr)
      (if (null? lst)
          '()
          (if (eq? (car lst) dest)
              (cons (cdr lst) attr)
              '())))))

(define epsilon
  (lambda ()
    (lambda (lst attr) (cons lst attr))))

(define concat
  (lambda (exp1 exp2)
    (lambda (lst attr)
      ((lambda (result)
         (if (null? result)
             '()
             (exp2 (car result) (cdr result))))
       (exp1 lst attr)))))

(define choice
  (lambda (exp1 exp2)
    (lambda (lst attr)
      ((lambda (result)
         (if (null? result)
             (exp2 lst attr)
             result))
       (exp1 lst attr)))))

(define action
  (lambda (exp1 action)
    (lambda (lst attr)
      ((lambda (result)
         (if (null? result)
             '()
             (cons (car result) (action (cdr result) attr))))
       (exp1 lst attr)))))

(define letrec-y
  (lambda (arg1)
    ((lambda (f h) (f h))
     (lambda (g) (g g))
     (lambda (p) (lambda (lst attr) ((arg1 (p p)) lst attr))))))

(define letrec-y2
  (lambda (arg1 arg2)
    ((lambda (f h) (car (f h)))
     (lambda (g) (g g))
     (lambda (p)
       (cons
         (lambda (lst attr) ((arg1 (car (p p)) (cdr (p p))) lst attr))
         (lambda (lst attr) ((arg2 (car (p p)) (cdr (p p))) lst attr)))))))

(define zero-or-more
  (lambda (exp1)
    (letrec-y
      (lambda (x) (choice (concat exp1 x) (epsilon))))))

(define opt
  (lambda (exp1)
    (choice exp1 (epsilon))))

