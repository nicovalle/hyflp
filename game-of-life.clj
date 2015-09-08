;;MAURO GIORDANO
;;NICOLAS VALLEJO

;; MODO DE USO (game-of-life ancho alto celulas-vivas cantidad-iteraciones)
;;EJEMPLOS 
;; (game-of-life 5 5 #{[2 1] [2 2] [2 3]} 4)
;; (game-of-life 4 4 #{[1 1] [2 2] [1 2] [2 1]} 5)



(defn crear-mundo
	[ancho alto viven]
	(vec (for [y (range alto)]
			(vec (for [x (range ancho)]
					(if (contains? viven [x y]) 1 0)
				)

			)
		)	
	)
)

(defn cantidad-vecinos-vivos [mundo x y]
		
	(cond 
		(and (= x (- (count (first mundo)) 1)) (= y (- (count mundo) 1)))
			(+ (get (get mundo y ) (- x 1))
				(+ (get (get mundo (- y 1)) x) 
					(get (get mundo (- y 1)) (- x 1)) 
				)
			)
		
		(and (= x 0) (= y 0))
			(+ (get (get mundo y) (+ 1 x)) 
				(+ (get (get mundo (+ 1 y)) (+ 1 x)) 
					(get (get mundo (+ 1 y)) x) 
				)
			)

		(and (= x 0) (= y (- (count mundo) 1)))
			(+ (get (get mundo y) (+ 1 x))
				(+ (get (get mundo (- y 1)) (+ 1 x)) 
					(get (get mundo (- y 1)) x) 
				)
			)

		(and (= x 0) (> y 0))
			(+ (get (get mundo y) (+ 1 x)) 
					(+ (get (get mundo (+ 1 y)) (+ 1 x)) 
						(+ (get (get mundo (+ 1 y)) x) 
								(+ (get (get mundo (- y 1)) (+ 1 x)) 
									(get (get mundo (- y 1)) x) 
								)
						)
					)
			)
		
		(and (= x (- (count (first mundo)) 1)) (= y 0))
				(+ (get (get mundo y) (- x 1)) 
					(+ (get (get mundo (+ 1 y)) x) 
						(get (get mundo (+ 1 y)) (- x 1)) 
					)
				)

		(and (= x (- (count (first mundo)) 1)) (> y 0))
				(+ (get (get mundo y) (- x 1)) 
						(+ (get (get mundo (+ 1 y)) x) 
							(+ (get (get mundo (+ 1 y)) (- x 1)) 
									(+ (get (get mundo (- y 1)) x) 
										(get (get mundo (- y 1)) (- x 1)) 
									)
							)
						)
				
				)	

		(and (> x 0) (= y (- (count mundo) 1)))
			(+ (get (get mundo y) (+ 1 x)) 
					(+ (get (get mundo y) (- x 1)) 
						(+ (get (get mundo (- y 1)) (+ 1 x)) 
							(+ (get (get mundo (- y 1)) x) 
								(get (get mundo (- y 1)) (- x 1)) 
							)
						)
					)
			)
		
		(and (> x 0) (= y 0))
		(+ (get (get mundo y) (+ 1 x)) 
				(+ (get (get mundo y) (- x 1)) 
					(+ (get (get mundo (+ 1 y)) (+ 1 x)) 
						(+ (get (get mundo (+ 1 y)) x) 
							(get (get mundo (+ 1 y)) (- x 1)) 
						)
					)
				)
		)

		:else
			(+ (get (get mundo y) (+ 1 x)) 
				(+ (get (get mundo y) (- x 1)) 
					(+ (get (get mundo (+ 1 y)) (+ 1 x)) 
						(+ (get (get mundo (+ 1 y)) x) 
							(+ (get (get mundo (+ 1 y)) (- x 1)) 
								(+ (get (get mundo (- y 1)) (+ 1 x)) 
									(+ (get (get mundo (- y 1)) x) 
												(get (get mundo (- y 1)) (- x 1)) 
									)
								)
							)
						)
					)	
				)
			)
	)		
)

(defn step
	[mundo]
	(vec (for [y (range (count mundo))]
			(vec (for [x (range (count (first mundo)))]
					(if 
						(or 
							(= 3 (cantidad-vecinos-vivos mundo x y)) 
							(and (= 2 (cantidad-vecinos-vivos mundo x y)) (= 1  (get (get mundo x) y))))
						1 0
					)

				)
			)
		)	
	)
)


(defn game-of-life
	[alto ancho viven iteraciones]
	(take iteraciones (iterate step (crear-mundo alto ancho viven)))
)
	
	
