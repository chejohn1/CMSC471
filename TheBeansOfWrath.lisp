(defpackage :TheBeansOfWrath)
(in-package :TheBeansOfWrath)


(import '(user::BeanTypes
	  user::BeanConversion
	  user::game-deck-stats user::game-discards user::game-discard-stats
	  user::game-coin-stats user::game-players user::game-rounds
	  user::game-shuffles
	  user::player-name user::player-hand user::player-faceup
	  user::player-numfields user::player-fields user::player-coins
	  user::player-coin-stack
	  user::trade-from-player user::trade-from-card user::trade-from-pos
	  user::trade-from-score
	  user::trade-to-player user::trade-to-card user::trade-to-pos
	  user::trade-to-score user::trade-info
	  user::plant user::harvest user::bean-fits
	  user::legal-fields-to-harvest user::plant-card-at-front
	  user::harvest-rate user::buy-third-bean-field
	  user::is-singleton? user::is-multiple? user::is-empty?
	  user::is-planted? user::trade user::viable-trade))

;;; decides the best course of action for planting a card
(defun plant-card (player card game)
  ;; attempt to buy a third bean field
  (possibly-buy-third-bean-field player game)
  (cond
   ;; if there is no card, do nothing
   ((null card))
   ;; if there is a field that is occupied by the given card, plant
   ;; the card there
   ((plant-in-occupied-field player card))
   ;; if there is an empty field, plant the card there
   ((plant-in-empty-field player card))
   ;; otherwise, choose a best field to harvest and plant the card there
   (t
    (progn
      (setf best-field (best-field-to-harvest player))
      (harvest player best-field game)
      (plant card player best-field)))))

;;; decides whether or not to plant optional card
(defun optionally-plant-card (player game)
  ;; attempt to buy a third bean field
  (possibly-buy-third-bean-field player game)
  ;; plant the card if there is a field that contains it, if there is an
  ;; empty field, or if it's worth planting based on a utility function
  (when (or
	 (all-contains-bean? (car (player-hand player)) player)
	 (all-is-empty? player)
	 (worth-planting? player))
	 (plant-card player (pop (player-hand player)) game)))

;;;Returns true if it is possible for the top card to gain more coins than 
;;;the current worst field with only the cards in the player's hand
(defun worth-planting? (player)
  (setf current-best-field (nth (best-field-to-harvest player) (player-fields player)))
  (setf new-field nil)
  (setf card (car (player-hand player)))
  (setf num-in-hand (how-many-in-hand player card))
  ;;Populate the new field state and compare with current field states
  (loop for i from 0 to num-in-hand do
	(cons card new-field))
  (if (> (harvest-rate new-field) (harvest-rate current-best-field))
      t
    nil))

;;; plants face-up cards based on if there is already a field containing
;;; each card in play
(defun handle-face-up-cards (player game)
  ;; attempts to buy a third bean field
  (possibly-buy-third-bean-field player game)
  ;; if the first face-up card already exists in a given field, plant it first

  (trade (generate-trades player) player)

  (if (all-contains-bean? (first (player-faceup player)) player)
      (progn
	(plant-card player (pop (player-faceup player)) game)
	(plant-card player (pop (player-faceup player)) game))
    ;; otherwise, plant the other card first
    (progn
      (nreverse (player-faceup player))
      (plant-card player (pop (player-faceup player)) game)
      (plant-card player (pop (player-faceup player)) game))))

;;; buys third bean field if it hasn't already been bought and the deck has
;;; been shuffled less than 3 times
(defun possibly-buy-third-bean-field (player game)
  (when (or
	 (eq (list-length (game-players game)) 2)
	 (< (game-shuffles game) 3))
    (buy-third-bean-field player game)))

;;; returns true if the player has three fields, nil if only two
(defun third-field? (player)
  (eq (player-numfields player) 3))

;;; returns true if the field contains the given bean, nil if not
(defun contains-bean? (bean field)
  (eq bean (car field)))

;;; returns true if any field contains the given bean, nil if not
(defun all-contains-bean? (card player)
  (cond
   ((contains-bean? card (first (player-fields player)))
    t)
   ((contains-bean? card (second (player-fields player)))
    t)
   ((and
     (third-field? player)
     (contains-bean? card (third (player-fields player))))
    t)
   (t nil)))

;;; returns true if any field is empty, nil if not
(defun all-is-empty? (player)
  (cond
   ((is-empty? (first (player-fields player)))
    t)
   ((is-empty? (second (player-fields player)))
    t)
   ((and
     (third-field? player)
     (is-empty? (third (player-fields player))))
    t)
   (t nil)))

;;; attempts to plant a card in a field that already has the given card,
;;; returns nil if it can't
(defun plant-in-occupied-field (player card)
  (cond
   ((contains-bean? card (first (player-fields player)))
    (plant card player 0))
   ((contains-bean? card (second (player-fields player)))
    (plant card player 1))
   ((and
     (third-field? player)
     (contains-bean? card (third (player-fields player))))
    (plant card player 2))))

;;; attempts to plant a card in an empty field, returns nil if there are none
(defun plant-in-empty-field (player card)
  (cond
   ((is-empty? (first (player-fields player)))
    (plant card player 0))
   ((is-empty? (second (player-fields player)))
    (plant card player 1))
   ((and
     (third-field? player)
     (is-empty? (third (player-fields player))))
    (plant card player 2))))

;;; Utility Function for choosing the best field to harvest based on the amount
;;; of coins gained from that harvest. If all harvest values are the same the 
;;; first field is harvested.
(defun best-field-to-harvest (player)
  (setf fields (player-fields player))
  (setf legal-fields (legal-fields-to-harvest fields))
  (setf most-coins 0)
  (setf most-cards 0)
  (setf best (car legal-fields))
  (loop for x in legal-fields do
	(if (> (harvest-rate (nth x fields)) most-coins)
	    (progn
               (setf most-coins (harvest-rate (nth x fields)))
               (setf best x))
	  (progn
	    (if (eq (harvest-rate (nth x fields)) most-coins)
		(if (< (length (nth x fields)) (length (nth best fields)))
		    (progn
		      (if (eq x 2)
			  (if (third-field? player)
			      (setf best x))
			(setf best best))))))))
       
  best)



;;; Returns the number of a given card value in the player's hand
(defun how-many-in-hand (player card)
  (length
  (remove-if-not #'(lambda (x) (equal x card)) (player-hand player)))
)



(defun generate-trades (player &aux (trades nil) desired-cards)
  (setf desired-cards
	(remove-if #'null (mapcar #'car (player-fields player))))
  (loop for card in (player-faceup player)
	do (if (not (member card desired-cards))
	       (setf trades (append trades
				    (make-new-trades
				     player 'player-faceup card
				     desired-cards 1)))))
  (loop for i from 0 to (length (player-hand player))
	do (if (not (member (nth i (player-hand player)) desired-cards))
	       (setf trades (append trades
				    (make-new-trades
				     player i (nth i (player-hand player))
				     desired-cards (/ 1 (+ 1 i)))))))
  trades
  )

(defun make-new-trades (player loc bad goods value)
  (loop for card in goods
	collect (make-instance 'trade :from-player player
			       :from-card bad
			       :from-pos loc
			       :from-score value
			       :to-card card
			       :info nil)))


(defun evaluate-trades (player trades)
  (loop for trade in trades
	do (if (trade-to-card trade)
	       (let ((viable (viable-trade player trade)))
		 (if viable
		     (push (list player 1 (car viable))
			   (trade-info trade))
		   (push (list player 0) (trade-info trade))))
	     (push (list player 1) (trade-info trade))))
  )