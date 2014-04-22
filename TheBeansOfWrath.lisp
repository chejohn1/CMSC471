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
	  user::plant user::harvest user::bean-fits
	  user::legal-fields-to-harvest user::plant-card-at-front
	  user::harvest-rate user::buy-third-bean-field
	  user::is-singleton? user::is-multiple? user::is-empty?
	  user::is-planted?))

(defun plant-card (player card game)
  (buy-third-bean-field player game)
  (cond
   ((null card))
   ((plant-in-occupied-field player card))
   ((plant-in-empty-field player card))
   (t
    (progn
      (setf best-field (best-field-to-harvest player))
      (harvest player best-field game)
      (plant card player best-field)))))

(defun optionally-plant-card (player game)
  (buy-third-bean-field player game)
  (if (or
       (all-contains-bean? (car (player-hand player)) player)
       (all-is-empty? (car (player-hand player)) player)
       ; UTILITY FUNCTION CODE HERE)
       (plant-card player (pop (player-hand player)) game))))

;;; plants face-up cards based on if there is already a field containing
;;; each card in play
(defun handle-face-up-cards (player game)
  (buy-third-bean-field player game)
  (if (all-contains-bean? (first (player-faceup player)) player)
      (progn
	(plant-card player (pop (player-faceup player)) game)
	(plant-card player (pop (player-faceup player)) game))
    (progn
      (nreverse (player-faceup player))
      (plant-card player (pop (player-faceup player)) game)
      (plant-card player (pop (player-faceup player)) game))))

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
(defun all-is-empty? (card player)
  (cond
   ((is-empty? card (first (player-fields player)))
    t)
   ((is-empty? card (second (player-fields player)))
    t)
   ((and
     (third-field? player)
     (is-empty? card (third (player-fields player))))
    t)
   (t nil)))

;;; attempts to plant a card in a field that already has the given card,
;;; returns null if it can't
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

;;; attempts to plant a card in an empty field, returns null if there are none
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

(defun best-field-to-harvest (player)
  (setf legal-fields (player-fields player))
  (setf most-coins 0)
  (setf best 0)
  (loop for x from 0 to 2 do
	(if (> (harvest-rate (nth x legal-fields)) most-coins)
	    (progn
               (setf most-coins (harvest-rate (nth x  legal-fields)))
               (setf best x))))
  best)
