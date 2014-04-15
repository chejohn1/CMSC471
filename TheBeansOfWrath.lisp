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
	(buy-third-bean-field (player game)))
	
(defun optionally-plant-card (player game)
	(buy-third-bean-field (player game))
	(plant-card player (car plant-card) game))
	
(defun handle-face-up-cards (player game)
	(buy-third-bean-field (player game))
	(plant-card player (pop (player-faceup player)) game)
	(plant-card player (pop (player-faceup player)) game))
	
(defun harvest-best-field (player game)
	(let ((which (legal-fields-to-harvest (player-fields player))))
		(cond ((not which)
			(error "No legal fields to harvest"))
		      (t (harvest player (car which) game)))
		(car which)))
