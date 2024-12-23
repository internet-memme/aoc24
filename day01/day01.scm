(use-modules (ice-9 rdelim)
	     (ice-9 textual-ports)
	     (ice-9 regex)
	     (srfi srfi-1))
(define input (get-string-all (open-file "input" "r")))

(define (lines file) (string-split file #\newline))

(define input-list
  (map (λ (x)  (map (λ (x) (string->number (match:substring x))) (list-matches  "[0-9]+" x))) (lines input))
  )

(define sortet-colums
(apply map list (list (sort (map car input-list)  < ) (sort  (map (λ (x) (car (cdr x))) input-list) <)))

)

(fold (λ (x) (apply + x)) (map (λ (x) (abs (- x)))))
(fold + 0 (map (λ (x)(abs (fold - 0 x))) sortet-colums)) ; 1320851

;; part two
;; for each value in the first colum find the number of occurences in the right and multipy the number in the first colum by its occurence in the right

(define first-colum  (map car input-list))
(define second-colum (map (λ (x) (car (cdr x))) input-list))

(define (occure num)
  (filter (λ (x) (= num x)) (map (λ (x) (car (cdr x))) input-list)))

(define (count-occ num) (length (occure num)))

(apply + 0 (map (λ (x) (* x (count-occ x))) first-colum)) ;  26859182

