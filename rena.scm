(define (Rena)
  (lambda (match)
    (define (char-matcher chstart chend)
      (lambda (index attr)
        (if (>= index (string-length match))
            #f
            (let ((ch (string-ref match index)))
              (if (and (char>=? ch chstart) (char<=? ch chend))
                  (list (+ index 1) attr)
                  #f)))))

    (define (epsilon index attr)
      (list "" index attr))

    (define (concat exp1 exp2)
      (lambda (index attr)
        (let ((result1 (exp1 index attr)))
          (if result1
              (exp2 (car result1) (cadr result1))
              #f))))

    (define (choice . exps)
      (lambda (index attr)
        (let loop ((exps exps))
          (if (null? exps)
              #f
              (let ((result ((car exps) index attr)))
                (if result
                    result
                    (loop (cdr exps))))))))

    (define (lookahead-not exp1)
      (lambda (index attr)
        (let ((result (exp1 index attr)))
          (if result
              #f
              (list index attr)))))

    (define (zero-or-more exp1)
      (lambda (index attr)
        (let loop ((index-new index))
          (let ((result (exp1 index-new attr)))
            (if result
                (loop (car result))
                (list index-new attr))))))

    (define (action exp1 action)
      (lambda (index attr)
        (let ((result (exp1 index attr)))
          (if result
              (list (car result) (action (substring match index (car result)) (cadr result) attr))
              #f))))

    (lambda (message . exps)
      (cond ((eq? message 'char) (apply char-matcher exps))
            ((eq? message 'epslion) epsilon)
            ((eq? message 'concat) (apply concat exps))
            ((eq? message 'choice) (apply choice exps))
            ((eq? message 'not) (apply lookahead-not exps))
            ((eq? message 'zero-or-more) (apply zero-or-more exps))
            ((eq? message 'action) (apply action exps))
            ((eq? message 'one-or-more) (concat (car exps) (zero-or-more (car exps))))
            ((eq? message 'opt) (choice (car exps) epsilon))
            ((eq? message 'lookahead) (lookahead-not (lookahead-not (car exps))))))))

