(define spt (lambda () (led_show 15 8 1 1 5)))
(define sptt (lambda () (led_show 15 8 1 0 5)))
(define ledd (lambda () (list (led_data '( 6 6 5 5 ) 0) (led_show 4 0 0 0 5) (led_show 1 0 0 0 5) (led_show 2 0 0 0 5) (sptt) )))
(ledd)
(define nth (lambda (n xs) (cond ((eq n 1) (car xs)) (t (nth (- n 1) (cdr xs))))))
(define drop (lambda (x xs) (cond ((eq x 0) xs) (t (drop (- x 1) (cdr xs))))))
(define take (lambda (x xs) (cond ((eq x 0) nil) (t (cons (car xs) (take (- x 1) (cdr xs)))))))
(define append (lambda (xs ys) (if (= (car xs) nil) ys (cons (car xs) (append (cdr xs) ys) ))))
(define rotate (lambda (n xs) (if (= (car xs) nil) nil (append (drop n xs) (take n xs)))))
(define incf (lambda (m) (let ((xx (+ (eval m) 1))) (set m xx))))
(list (define words 1) (define tries 0))
(set! words '( ( \"f,a,l,l\" \"f,a,l,l\" \"f,a,l,l\" \"f,a,l,l\" ) ( \"b,a,l,l\" \"f,i,l,l\" \"t,a,l,l\" \"f,e,l,l\" ) ( \"t,e,l,l\" \"t,o,l,l\" \"t,i,l,l\" \"t,a,i,l\" ) (\"r,o,l,l\" \"p,o,l,l\" \"t,o,i,l\" \"t,o,l,d\") (\"c,o,l,d\" \"c,o,l,d\" \"c,o,l,d\" \"c,o,l,d\") ))
(define curRow 0)
(define rowCount 5)
(define wordCount 4)
(define rotCount '(0 0 0 0 0))
(define answer '(2 1 3))
(define srcHelper (lambda (n v) (append (take n rotCount) (cons v (drop (+ n 1) rotCount)))))
(define setRotCount (lambda (n v) (let ((xx (cond ((and (< -1 n) (< n rowCount)) (srcHelper n v)) (t (append (take n rotCount) (cons v nil))) ))) (set 'rotCount xx))))
(define rotRow (lambda () (cond ((eq (nth (+ curRow 1) rotCount) (- wordCount 1)) (setRotCount curRow 0)) (t (setRotCount curRow (+ (nth (+ curRow 1) rotCount) 1))))))
(define nextRow (lambda () (cond ((eq curRow (- rowCount 1)) (set 'curRow 0)) (t (incf 'curRow)))))
(define getRow (lambda () (nth (+ curRow 1) words)))
(define wordAsNums (lambda () (mapcar char (split (nth ( + (nth (+ curRow 1) rotCount) 1) (getRow)) \\"))))
(define showDisp (lambda () (list (led_data (wordAsNums)) (led_data (list (+ curRow 1)) 4) (sptt))))
(interrupt 2 2)
(interrupt 4 2)
(define (int02 pin clicks count ms) (list (rotRow) (incf 'tries) (led_data (list (/ (- tries (% tries 10)) 10) (% tries 10)) 6)  (check) (showDisp)))
(define (int04 pin clicks count ms) (list (nextRow) (showDisp)))
(define zip (lambda (xs ys) (cond ((eq (car xs) nil) nil) ((eq (car ys) nil) nil) (t (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys) ))))))
(define minus (lambda (t) (- (car t) (nth 2 t)) ))
(define reduce (lambda (xs seed) (cond ((eq (car xs) nil) seed) (t (reduce (cdr xs) (cond ((eq (car xs) 0) seed) (t (+ seed 1)))) ))))
(define check (lambda () (cond ((eq (reduce (mapcar minus (zip answer (take (- rowCount 2) (drop 1 rotCount)))) 0) 0) (out 5 1)) (t (out 5 0)))))
