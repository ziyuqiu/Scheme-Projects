; Ziyu Qiu (ziyuqiu@brandeis.edu) 2016-03-10
; PS3-Stable Marriage         

;;;THIS IS THE GIVEN CODE
;; This is the code for -- Stable Marriage

;modified in problem1
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

;(define (courtship unengaged-proposers proposers proposees) ... )

;(define (currently-unengaged list-of-people) ... )

;(define (send list-of-people message) ... )

;(define (couple? person1 person2) ...)

(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))
;;;modified in Problem2
(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            'we-are-engaged)
                     'no-one-loves-me)))
            ;((eq? message 'i-love-you) ... )
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            'dumped!)
                        (else 
                            'there-must-be-some-misunderstanding))))
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))

;;;HERE STARTS MY CODE
;;;See Problem 0 and 4-7 in pdf file
;Problem 1. Write the following procedures:
;A subset of the proposers (namely, the unengaged proposers) propose
;marriage to the proposees. Note that while we for the moment assume that
;⟨proposers⟩ is a list of men and ⟨proposees⟩ is a list of women, we are
;not assuming that men-procedures are structurally different from
;women-procedures, i.e., they are both produced by make-person. Do we
;really need this list of proposees? Maybe not. . .


;We do not really need the list of proposees, because we could always propose
;the first people on the unengaged list if there are unengaged proposers 
(define (courtship unengaged-proposers proposers)
  (if (null? unengaged-proposers)
      '()
      (begin ((car unengaged-proposers) 'propose)
             (courtship (currently-unengaged proposers) proposers))))
;since proposees are unnecessary, we could remove the parameter from match-make
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))
;a predicate telling whether two people are engaged to each other.
(define (couple? person1 person2)
  (and (equal? (person1 'intended) person2) (equal? (person2 'intended) person1)))
;a procedure returning a list of the people appearing as elements of
;⟨list⟩ who are not engaged.
(define (currently-unengaged list-of-people)
  (filter unengaged list-of-people))
(define (unengaged person)
    (null? (person 'intended)))
;sends a given message to each person in the list,for example (send list-of
;-men ’propose).
(define (send list-of-people message)
  (if (null? list-of-people)
      '()
      (begin ((car list-of-people) message)
             (send (cdr list-of-people) message))))
;A predicate telling whether the first argument is liked more than the
;second argument by a particular person/procedure. Note: this procedure
;has to be defined in the correct lexical context to be able to access
;and manipulate the proper data structures.
(define (i-like-more? person1 person2)
  (lambda (x) (rank (x 'loves) person1 person2)))
(define (rank list person1 person2)
        (cond ((null? list) '())
              ((equal? person1 (car list)) #t)
              ((equal? person2 (car list)) #f)
              (else (rank (cdr list) person1 person2))))


;Problem 2. Complete the part of make-person that handles the message
;i-love-you.
(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
;evaluates to the atom given as the name of the person/procedure
      (cond ((eq? message 'name) my-name)
;evaluates to the procedure denoting the intended significant other
            ((eq? message 'intended) current-intended)
;evaluates to the list of people the person would marry in order of preference
            ((eq? message 'loves) preference-list)
;evaluates to the list of people the person has not yet proposed to, but is
;still interested in, given in order of preference
            ((eq? message 'possible) possible-mates)
;reinitializes a person’s parameters, setting the current intended to ’()
;and possible-mates to preference-list: this allows the person to start
;proposing again beginning with his or her first choice, or start
;considering pro- posals with no obligations, unconstrained by previous
;romantic entanglements.
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
;assigns to preference-list a list of mates (in order of preference) to a
;person
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
;causes a procedure to propose to his or her “next” choice, i.e., the most
;desired individual to whom the person has not yet proposed.
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 (dialogue 'propose my-name (beloved 'name))
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            'we-are-engaged)
                     'no-one-loves-me)))
;a message sent to a beloved: it evaluates to a procedure P that is then
;applied to the sender of the proposal. The procedure P can access and
;change the same state information as the procedure representing the beloved. 
            ((eq? message 'i-love-you)
             (lambda (person)
             ;if intended list is empty, accept the proposal
               (if (null? (me 'intended))
                   (begin (set! current-intended person)
                          (dialogue 'i-love-you-too my-name (person 'name))
                          'i-love-you-too)
             ;if intended list is nonempty, call i-like-more
                   (let ((prev (me 'intended)))
                     (if ((i-like-more? person prev) me)
             ;if the current one is more favorable, dump the previous
                         (begin ((prev 'i-changed-my-mind) me)
                                (write-line (list (prev 'name) 'is 'now 'single))
                                (set! current-intended person)
                                (dialogue 'i-love-you-too my-name (person 'name))
                                'i-love-you-too)
             ;if the previous one is more favorable, refuse the current
                         (begin (dialogue 'deny my-name (person 'name))
                                 'buzz-off-creep))))))
;the message sent to a Mr. Right who in the course of the stable marriage
;algorithm has become Mr. Wrong, in order that he become Mr. Dumped.
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (begin(set! current-intended '())
                                  (dialogue 'i-change-my-mind my-name (lost-love 'name)))
                            'dumped!)
                        (else 
                            'there-must-be-some-misunderstanding))))
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))

;Problem 3. Add dialogue to the algorithm, so that (say) proposals,
;acceptances, and engagement breakings are printed at the terminal as the
;stable marriage algorithm continues.
(define (dialogue msg p1 p2)
  (cond ((equal? msg 'propose) 
         (write-line (list p1 'propose 'to p2)))
        ((equal? msg 'i-change-my-mind)
         (write-line (list p1 'and p2 'break-engagement)))
        ((equal? msg 'i-love-you-too)
         (write-line (list p1 'accepts p2)))
        ((equal? msg 'deny)
         (write-line (list p1 'denies p2)))))

(define (write-line x)
  (begin (write x)
         (newline)))


;; This is a test file for -- Stable Marriage
(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))

(match-make men women)
(match-make women men)

;;;This is test file for Problem 7
(define bob (make-person 'Bob))
(define carol (make-person 'Carol))
(define ted (make-person 'Ted))
(define alice (make-person 'Alice))
((bob 'load-preferences)(list carol alice))
((ted 'load-preferences)(list alice carol))
((carol 'load-preferences)(list ted bob))
((alice 'load-preferences)(list bob ted))
(define men (list bob ted))
(define women (list carol alice))
(match-make men women)
(match-make women men)