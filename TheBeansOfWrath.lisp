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
      (harvest player 0 game)
      (plant card player 0)))))

(defun optionally-plant-card (player game)
  (buy-third-bean-field player game)
  (plant-card player (pop (player-hand player)) game))

(defun handle-face-up-cards (player game)
  (buy-third-bean-field player game)
  (plant-card player (pop (player-faceup player)) game)
  (plant-card player (pop (player-faceup player)) game))

;;; returns true if the player has three fields, null if only two
(defun third-field? (player)
  (eq (player-numfields player) 3))

;;; returns true if the field contains the given bean, null if not
(defun contains-bean? (bean field)
  (eq bean (car field)))

;;; attempts to plant a card in a field that already has the given card,
;;; returns null if it can't
(defun plant-in-occupied-field (player card)
  (cond
   ((contains-bean? card (first (player-fields player)))
    (plant card player 0))
   ((contains-bean? card (second (player-fields player)))
    (plant card player 1))
   ((contains-bean? card (third (player-fields player)))
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