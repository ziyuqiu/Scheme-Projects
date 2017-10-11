; Ziyu Qiu (ziyuqiu@brandeis.edu) 2016-02-25
; PS2-Computer Psychiatrist

;;;THIS IS THE GIVEN CODE
;; This is the code for ``Computer Psychiatrist'' (Doctor)

; for Racket users...
(#%require (only racket/base random))
; *******************

;;;modified in problem3
;(define (visit-doctor name)
;  (write-line (list 'hello name))
;  (write-line '(what seems to be the trouble?))
;  (doctor-driver-loop name))

;;;modiefied in problem 3
;(define (doctor-driver-loop name)
;  (newline)
;  (write '**)
;  (let ((user-response (read)))
;    (cond ((equal? user-response '(goodbye))
;             (write-line (list 'goodbye name))
;             (write-line '(see you next week)))
;          (else (write-line (reply user-response))
;                (doctor-driver-loop name)))))

;;;modified in problem 3
;(define (reply user-response)
;  (cond ((fifty-fifty)
;           (append (qualifier)
;                   (change-person user-response)))
;        (else (hedge))))

(define (fifty-fifty)
  (= (random 2) 0))

;;;modified in problem1
;(define (qualifier)
;  (pick-random '((you seem to think)
;                 (you feel that)
;                 (why do you believe)
;                 (why do you say))))

;;;modified in problem1
;(define (hedge)
;  (pick-random '((please go on)
;                 (many people have the same sorts of feelings)
;                 (many of my patients have told me the same thing)
;                 (please continue))))

;;;modified in problem 2
;(define (replace pattern replacement lst)
;  (cond ((null? lst) '())
;        ((equal? (car lst) pattern)
;           (cons replacement
;                 (replace pattern replacement (cdr lst))))
;        (else (cons (car lst)
;              (replace pattern replacement (cdr lst))))))

;;;modified in problem2
;(define (many-replace replacement-pairs lst)
;  (cond ((null? replacement-pairs) lst)
;         (else (let ((pat-rep (car replacement-pairs)))
;            (replace (car pat-rep)
;                     (cadr pat-rep)
;                     (many-replace (cdr replacement-pairs)
;                     lst))))))

;;;modified in problem 2
;(define (change-person phrase)
;  (many-replace '((i you) (me you) (am are) (my your))
;                phrase))

(define (pick-random lst)
  (nth (+ 1 (random (length lst))) lst))

;;******

(define (prob n1 n2)
  (< (random n2) n1))

(define (ask-patient-name)
  (write-line '(next!))
  (write-line '(who are you?))
  (car (read)))

(define (nth n lst)
  (if (= n 1) 
      (car lst)
      (nth (- n 1) (cdr lst))))
;;******

(define (atom? a) (not (pair? a)))

(define nil '())

(define (write-line x) (begin (write x) (newline)))

;;******



;;;HERE STARTS MY CODE

;;;Problem 1. Edit the qualifier and hedge procedures to increase the
;doctor’s repertoire of qualifying and hedging phrases.
(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say)
                 (it seems that you are worrying about)
                 (why do you think)
                 (it sounds like that you believe)
                 (what makes you feel))))

(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (do not worry you are not alone)
                 (calm down)
                 (I am here listening)
                 (I have heard some similiar experience before)
                 (please go ahead))))

;;;Problem 2. Having it not only change first person words to second person,
;but also second person to first.
(define (change-person phrase)
  (many-replace '((i you) (me you) (am are) (my your) (are am) (you i) (your my))
                phrase))

(define (replace replacement-pairs word)
  (cond ((null? replacement-pairs) word)
        ((equal? (car (car replacement-pairs)) word)
         (cadr (car replacement-pairs)))
        (else (replace (cdr replacement-pairs) word))))

(define (many-replace replacement-pairs lst)
  (cond ((null? lst) nil)
        (else (cons (replace replacement-pairs (car lst))
                    (many-replace replacement-pairs (cdr lst))))))
;see answer to questions in writeup

;;;Problem 3. Add a third method of generating responses by remembering
;everything the user said.("earlier you said...")
;Let the procedure doctor-driver-loop maintain a list of all user responses.
(define response-list nil)

;;;modified again in problem 4
;(define (doctor-driver-loop name response-list)
;  (newline)
;  (write '**)
;  (let ((user-response (read)))
;    (cond ((equal? user-response '(goodbye))
;             (write-line (list 'goodbye name))
;             (write-line '(see you next week)))
;          (else (write-line (reply user-response response-list))
;                (doctor-driver-loop name (cons user-response response-list))))))

;;;modified again in problem 4
;(define (visit-doctor name)
;  (write-line (list 'hello name))
;  (write-line '(what seems to be the trouble?))
;  (doctor-driver-loop name response-list));add parameter response-list

;Modify the program so that reply occasionally replies by picking a
;previous user response at random, changing person in that response,
;and prefixing the modified response with “earlier you said that.”
(define (reply user-response response-list)
  (if (null? response-list)
      (original-reply user-response)
      (new-reply user-response response-list)))

(define (original-reply user-response)
  (cond ((fifty-fifty)
           (append (qualifier)
                   (change-person user-response)))
        (else (hedge))))

(define (new-reply user-response response-list)
  (if(prob 1 3)(append '(earlier you said that)(change-person (pick-random response-list)))
          (original-reply user-response)))

;see listing in writeup

;;;Problem 4. Modify to let the doctor automatically sees a new patient
;after the old one goes away, and provide some way to tell the doctor
;when to stop.

;;;My methods let the doctor stop after the 5th patient.
(define count 0)

(define add1 
    (lambda (x)
        (+ x 1)))

(define (visit-doctor);delete parameter name
  (set! count (add1 count));add 1 to count for each visitor
  (newline)
  (write-line '(Next...))
  ;(write 'Patient-number: )
  ;(write-line count)
  (write-line '(what's your name?))
  (let ((name (car(read))))
  (write-line (list 'hello name))
  (write-line '(what seems to be the trouble?))
  (doctor-driver-loop name response-list)))

(define (doctor-driver-loop name response-list)
  (newline)
  (write '**)
  (let ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
             (write-line (list 'goodbye name))
             (write-line '(see you next week))
             (if (> 5 count)
                  (visit-doctor)
                  (write-line '(I can only take care of 5 patients a day. Come here tomorrow.))));add recursion
          (else (write-line (reply user-response response-list))
                (doctor-driver-loop name (cons user-response response-list))))))