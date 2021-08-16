#lang racket
(provide (all-defined-out))
(provide expr-compare)

;from piazza
(define LAMBDA (string->symbol "\u03BB"))

(define (make-bind x y)
 	(string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (valid-lambda x)
	(cond
		[(or (eqv? x 'lambda) (eqv? x LAMBDA)) #t]
		[else #f]	
	))
(define (second-valid x y)
	(cond
		[(or (eqv? x LAMBDA) (eqv? y LAMBDA)) LAMBDA]
		[else 'lambda]
	))

(define (fix-lambda x y diff xsym ysym)
	( cond 
		[(and (not(list? x)) (not(list? y)) (not(equal? x y))) (list (cons (make-bind x y) diff) (cons x xsym) (cons y ysym))]
		[(or 
			(or (not(list? x)) (not(list? y)) 
			(and (equal? y '()) (equal? x '())))) 
			(list diff xsym ysym)]
		[(and (equal? y '()) (equal? x '())) (list diff xsym ysym)]
		[(equal? (car x) (car y)) (fix-lambda (cdr x) (cdr y) diff xsym ysym) ]
		[ else
			(fix-lambda (cdr x) (cdr y) (cons (make-bind (car x) (car y)) diff) (cons (car x) xsym) (cons (car y) ysym))
		]
	))

(define (index i lst val) 
		(cond
			[(equal? val (car lst)) i]
			[else (index (+ i 1) (cdr lst) val)]
	))

(define (next-fix a fixed-head fixed-tail )
	(cond
		[(equal? a '()) '()]
		[(valid-lambda (car a)) a]
		[(list? (car a)) (cons (next-fix (car a) fixed-head fixed-tail ) (next-fix (cdr a) fixed-head fixed-tail ))]
		[(member (car a) fixed-tail) (cons (list-ref fixed-head (index 0 fixed-tail (car a))) (next-fix (cdr a) fixed-head fixed-tail ))]
		[else (cons (car a) (next-fix (cdr a) fixed-head fixed-tail ))]
	))

(define (lambda-check x y)
	(cond
		[ (or (and (and (list? (cadr x)) (list? (cadr y))) (equal? (length (cadr x)) (length (cadr y)))) 
			(and (not(list? (cadr x))) (not(list? (cadr y)))) )
			(let ((fixed (fix-lambda (cadr x) (cadr y) '() '() '() )))
				(let ((new-lamb (second-valid (car x) (car y))
					))
					(cons new-lamb (expr-compare 
						(next-fix (cdr x) (car fixed) (cadr fixed) ) 
						(next-fix (cdr y) (car fixed) (caddr fixed) ) 
				))

			))
		]
		[else (list 'if '% x y)]

	))

(define (other-check x y)
	(cond 
		[(or (equal? x '()) (equal? y '())) '() ]
		[else (cons (expr-compare (car x) (car y)) (other-check (cdr x) (cdr y)))]		
	))

(define (list-check x y)
	(cond
		[(equal? (length x) 0) (list 'if '% x y)]
		[(and (valid-lambda (car x)) (valid-lambda (car y))) (lambda-check x y)]
		[(xor (valid-lambda (car x)) (valid-lambda (car y))) (list 'if '% x y)]
		[(xor (eqv? (car x) 'if) (eqv? (car y) 'if)) (list 'if '% x y)]
		[(or (eqv? (car x) 'quote) (eqv? (car y) 'quote)) (list 'if '% x y)]
		[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
	))

;mostly from TA github, just moved outside the expr-compare bc is called multiple times
(define (compare-one x y)
	(cond
		;if both are equal
		[(equal? x y) x]
		;if both are booleans and not equal
		[ (and (boolean? x) (boolean? y)) (if x '% '(not %)) ]
		;default case
		[else (list 'if '% x y)]
	))

(define (expr-compare x y) 
	(cond
		[(equal? x y) x]
		[ (and (boolean? x) (boolean? y)) (if x '% '(not %)) ]
		[(or (equal? x '()) (equal? y '())) (list 'if '% x y)]
		;if both are lists
		[(and (list? x) (list? y))
			(cond
				;if equal lengths
				[(eqv? (length x) (length y))
					(list-check x y)
				]
				;unequal lengths
				[else (compare-one x y)]
			)
		]
		;otherwise 
		[else (compare-one x y)]	
	))

(define (test-expr-compare x y)
	(and 
		(equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) 
		(equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
	))

(define test-expr-x
	'(list
		((lambda (a b if) (* (+ a b) if)) 1 2 2)
		"a"
		12
		#t
		#f
		(quote (a b c))
		((λ (a b) (if b a b)) #t #f)
                (list 1 5 9)
                ''(b a)
	))

(define test-expr-y
	'(list
		((λ (c b x) (* (+ c b) x)) 3 2 3)
		"x"
		14
		#f 
		#f
		(quote (c b a))
		((lambda (a b) (if a b a )) #t #f )
                (list 9 5 1)
                ''(a b)

	))

