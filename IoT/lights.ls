(list (define initialStateNum 1) (define mult 5))
(define stNum initialStateNum)
(define cols '(red amber green))
(list (define redl   (lambda (n) (out 12 n))) (define amberl (lambda (n) (out 0 n))) (define greenl (lambda (n) (out 5 n))))
(define redld (lambda (n o) (list (redl n) (delay (* o mult)) (clearl))))
(define amberld (lambda (n o) (list (amberl n) (delay (* o mult)) (clearl))))
(define greenld (lambda (n o) (list (greenl n) (delay (* o mult)) (clearl))))
(define redPattern (lambda (n) (list (redld 1 50) (delay (* 10 mult)) (redld 1 20) ) ))
(define amberPattern (lambda (n) (list (amberld 1 50) (delay (* 10 mult)) (amberld 1 20) ) ))
(define greenPattern (lambda (n) (list (greenld 1 50) (delay (* 10 mult)) (greenld 1 20) ) ))
(define lights (lambda (m n o) (list (redl m) (amberl n) (greenl o))))
(list (define clearl (lambda () (lights 0 0 0 ))) (define stopl  (lambda () (lights 1 0 0))) )
(list (define readyl (lambda () (lights 1 1 0))) (define gol    (lambda () (greenPattern))) (define slowl  (lambda () (lights 0 0 1))))
(list (define stopc  '(redPattern)) (define readyc '(redl amberl)))
(list (define goc    '(greenl)) (define slowc  '(amberl)))
(define states '(stopc readyc goc slowc))
(define incf (lambda (m) (let ((xx (+ (eval m) 1))) (set m xx))))
(define decf (lambda (m) (let ((xx (- (eval m) 1))) (set m xx))))
(define nth (lambda (n xs) (cond ((eq n 1) (car xs)) (t (nth (- n 1) (cdr xs))))))
(define stateItem (lambda (n) (nth n states)))
(define loopStNum (lambda () (cond ((eq stNum (length states)) (set 'stNum 1)) (t (incf 'stNum)))))
(define backStNum (lambda () (cond ((eq stNum 1) (set 'stNum (length states))) (t (decf 'stNum)))))
(define setl (lambda (s) ((eval s) 1)))
(define showLights (lambda () (mapcar setl (eval (stateItem stNum)))))
(define changeLights (lambda () (list (loopStNum) (clearl) (showLights))))
(define backLights (lambda () (list (backStNum) (clearl) (showLights))))
(define upMult   (lambda () (cond ((eq mult 10) (set 'mult 10)) (t (incf 'mult)))))
(define downMult (lambda () (cond ((eq mult 1)  (set 'mult 1))  (t (decf 'mult)))))
(list (interrupt 2 2) (interrupt 4 2))
(list (define (int02 pin clicks count ms) (downMult)) (define (int04 pin clicks count ms) (upMult)))
(at -5000 (lambda () (changeLights)))