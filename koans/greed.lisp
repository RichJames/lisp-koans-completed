(defpackage #:greed
  (:use #:cl))

(in-package #:greed)

(defun roll (count)
  (check-type count (integer (0)))
  (loop :repeat count
        :collect (1+ (random 6))))

(defun score (&rest dice)
  (let* ((dice-list (if (listp (car dice)) (car dice) dice))
         (counts    (mapcar #'(lambda (x) (multiple-value-list (floor (count x dice-list) 3))) '(1 2 3 4 5 6)))
         (used-dice (mapcar #'(lambda (x y) (+ (if (> (first x) 0)
                                                   (* 3 (first x))
                                                   0)
                                               (if (or (= y 1) (= y 5))
                                                   (second x)
                                                   0)))
                            counts '(1 2 3 4 5 6))))

    (flet ((score-triples (lst idx)
             (cond ((= 1 idx) (* (first lst) 1000))
                   (t         (* (first lst) 100 idx))))

           (score-singles (lst idx)
             (cond ((= 1 idx) (* (second lst) 100))
                   ((= 5 idx) (* (second lst) 50))
                   (t 0))))

      (list (loop :for lst :in counts
                  :for i :from 1 :upto 6
                  :sum (+ (score-triples lst i) (score-singles lst i)))

            (- (length (if (listp (car dice)) (car dice) dice))
               (reduce #'+ used-dice))))))

(defclass player ()
  ((name :initarg :name :initform (error "Players must be given a name.") :accessor name)
   (points :initarg :points :initform 0 :accessor points)))

(defgeneric play-turn (player)
  (:documentation "Play a single turn for player."))

(defmethod play-turn ((p player))
  (with-slots (name points) p
    (say "~%~A, it's your turn.  Now rolling...~%" name points)

    (let ((turn-points  (player-rolls name 5 0)))

      (cond ((and (= points 0) (< turn-points 300))
             (say ":-( ~A, you need at least 300 points in a turn to get in the game. Try again next turn.~%" name))
            ((= points 0)
             (say ":-D Congratulations, ~A! You are now in the game with ~A points.~%"
                  name turn-points)
             (setf points turn-points))
            (t (say "~A, you add ~A to your total, which is now ~A.~%"
                    name turn-points (+ points turn-points))
               (incf points turn-points))))))

(defmethod print-player ((p player))
  (with-slots (name points) p
    (format t "~15A~5d~%" name points)))

(defun play-game ()
  (loop :with ending-game = nil
        :with players = (create-players (get-player-count))
        :for player :in players
        :for remaining-turns :downfrom most-positive-fixnum :to 1        
        :while (> remaining-turns 0)
        :do (progn
              (display-scores players)
              (play-turn player))
        :if (and (>= (slot-value player 'points) 3000)
                 (not ending-game))
          :do (progn
                (setf remaining-turns (circ-list-length players)
                      ending-game     t)
                (say "~%***** ~A has reached 3000 points.  We will play ~A more turns. *****~%~%"
                     (slot-value player 'name) (- remaining-turns 1)))
        :finally (return (display-game-results (get-winner players)))))

(defun get-winner (players)
  (loop :with winners = nil :and winning-score = 0
        :repeat (circ-list-length players)
        :for player :in players
        :do (with-slots (name points) player
              (cond ((> points winning-score) (setf winners       (list name)
                                                    winning-score points))
                    ((= points winning-score) (setf winners (cons name winners)))))
        :finally (return (cons winners (list winning-score)))))

(defun display-game-results (end-results)
  (format t "The winner~p ~:*~[~;is~:;are~] ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} with a score of ~a."
          (length (car end-results)) (car end-results) (cadr end-results)))

(defun circ-list-length (lst)
  (let ((count (loop :with first-item = (nth 0 lst)
                     :for i :upfrom 1
                     :while (not (equal (nth i lst) first-item))
                     :count i)))
    (+ count 1)))

(defun get-player-count ()
  (loop :for count = (prompt "How many players? ")
        :while (not (typep count 'integer))
        :finally (return count)))

(setf *print-circle* t)

(defun create-players (player-count)
  "Returns a circular list of the players."
  (let ((players (loop :for i :below player-count
                       :collect (create-player (+ i 1)))))
    (setf (cdr (last players)) players)
    players))

(defun create-player (n)
  (let ((pname (prompt "Who is player ~A? " n)))
    (make-instance 'player :name pname)))

(defun player-rolls (name dice turn-points)
  (let* ((results        (score (roll dice)))
         (points         (first results))
         (remaining-dice (if (= (second results) 0) 5 (second results))))

    (cond ((= points 0)
           (say ":-( ~A, your roll produced 0 points. Your turn ends with no points gained.~%" name)
           points)
          (t (say "~A, you scored ~A points on that roll and now have ~A points this turn.  You have ~A dice left.~%"
                  name points (+ turn-points points) remaining-dice)
             (let ((roll-again (prompt "Roll again?~%")))
               (cond ((eq roll-again 'y) (player-rolls name remaining-dice (+ turn-points points)))
                     (t (+ turn-points points))))))))

(defun display-scores (players)
  (say "~%Current scores:~%")
  (loop :repeat (circ-list-length players)
        :for player :in players
        :do (print-player player)))

(defun prompt (&rest args)
  "From On Lisp, pg 56."
  (apply #'format *query-io* args)
  (read *query-io*))

(defun say (&rest args)
  (apply #'format *standard-output* args))

