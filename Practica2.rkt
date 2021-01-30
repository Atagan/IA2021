#lang racket

(module+ test (require rackunit))

; Declaración de variables globales

(define board '((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()
                            (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()))
(define shoot-again true)
(define ops '((:primera-casilla 0)
              (:segunda-casilla 1)
              (:tercera-casilla 2)
              (:cuarta-casilla 3)
              (:quinta-casilla 4)
              (:sexta-casilla 5)
              (:septima-casilla 7)
              (:octaba-casilla 8)
              (:novena-casilla 9)
              (:decima-casilla 10)
              (:undecima-casilla 11)
              (:duodecima-casilla 12)
              ))
(define depuracion #f)

; Función que imprime el tablero del mancala, en cada casilla imprime el valor de la suma de las semillas que se encuentran en esa casilla
(define (print-board)
  (printf   "~% ~%       | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A | ~%"
            (apply + (list-ref board 12)) (apply + (list-ref board 11)) (apply + (list-ref board 10)) (apply + (list-ref board 9 )) (apply + (list-ref board 8)) (apply + (list-ref board 7)))
  (printf "~% | ~A |                                          | ~A | ~%"
          (apply + (list-ref board 13)) (apply + (list-ref board 6)))
  (printf   "~%       | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A | ~%~%"
            (apply + (list-ref board 0)) (apply + (list-ref board 1)) (apply + (list-ref board 2)) (apply + (list-ref board 3)) (apply + (list-ref board 4)) (apply + (list-ref board 5))))
         
; Funcion que reinicia el tablero a su estado original
(define (reset-game)
  (set! board '((1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                ()
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                (1 1 1 1 1)
                ()
                )
        )
  )
(module+ test (begin (reset-game)
                     (check-equal?
                      '((1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        ()
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        (1 1 1 1 1)
                        ())
                      board))
  )
;Predicado el cual valida si el juego ya terminó comprobando si alguna hilera esta completamente vacía
(define (game-ended?)
  (or (and
       (equal? null (list-ref board 0))
       (equal? null (list-ref board 1))
       (equal? null (list-ref board 2))
       (equal? null (list-ref board 3))
       (equal? null (list-ref board 4))
       (equal? null (list-ref board 5)))
      
      (and
       (equal? null (list-ref board 7))
       (equal? null (list-ref board 8))
       (equal? null (list-ref board 9))
       (equal? null (list-ref board 10))
       (equal? null (list-ref board 11))
       (equal? null (list-ref board 12))
       )
      )
  )

(module+ test (begin (reset-game)
                     (check-equal? #f (game-ended?))
                     )
  )

(define (ganador? estado)
  (if (positive? (heuristica-simple estado))
      1
      (begin
        (if (negative? (heuristica-simple estado))
            0
            "ninguno, ha sido empate")
        )
      )
  )

; Funcion que obtiene las semillas en una casilla
(define (get-balls casilla)
  (list-ref board casilla))

(module+ test (begin (reset-game)
                     (check-equal? 5 (apply + (get-balls 3)))
                     )
  )

(module+ test (begin (reset-game)
                     (check-equal? 0 (apply + (get-balls 13)))
                     )
  )

; Predicado que valida si es el operador seleccionado es valido
(define (valid-operator? operador estado jugador-actual)
  (let ((operador (car(cdr operador))))
    (if (equal? jugador-actual 0)
        (cond ((= operador 0) (if (equal? null (list-ref estado 0))
                                  #f
                                  #t))
              ((= operador 1) (if (equal? null (list-ref estado 1))
                                  #f
                                  #t))
              ((= operador 2) (if (equal? null (list-ref estado 2))
                                  #f
                                  #t))
              ((= operador 3) (if (equal? null (list-ref estado 3))
                                  #f
                                  #t))
              ((= operador 4) (if (equal? null (list-ref estado 4))
                                  #f
                                  #t))
              ((= operador 5) (if (equal? null (list-ref estado 5))
                                  #f
                                  #t))
              (#t
               #f)
              )
        (cond ((= operador 7) (if (equal? null (list-ref estado 7))
                                  #f
                                  #t))
              ((= operador 8) (if (equal? null (list-ref estado 8))
                                  #f
                                  #t))
              ((= operador 9) (if (equal? null (list-ref estado 9))
                                  #f
                                  #t))
              ((= operador 10) (if (equal? null (list-ref estado 10))
                                   #f
                                   #t))
              ((= operador 11) (if (equal? null (list-ref estado 11))
                                   #f
                                   #t))
              ((= operador 12) (if (equal? null (list-ref estado 12))
                                   #f
                                   #t))
              (#t
               #f)
              )
        )
    )
  )

(module+ test (begin (reset-game)
                     (check-equal? #t (valid-operator? (list-ref ops 1) board 0))
                     )
  )

(module+ test (begin (reset-game)
                     (check-equal? #t (valid-operator? (list-ref ops 7) board 1))
                     )
  )

; Funcion que aplica un operador de *ops* a un estado determinado
(define (apply-operator operador estado jugador-actual)
  (define casilla-actual (car(cdr operador)))
  (define-values (ops semillas-casilla estado-resultado)
    (values (car operador)  (get-balls casilla-actual) null))
  ;(printf "~a~%" ops)
  (if (equal? jugador-actual 0)
      (case ops
        [(:primera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:segunda-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:tercera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:cuarta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:quinta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:sexta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [else (printf "error")]
        )
      (case ops
        [(:septima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:octaba-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:novena-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:decima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:undecima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [(:duodecima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual semillas-casilla))]
        [else (printf "error")]
        )
      )
  estado-resultado
  )

(module+ test (begin (reset-game)
                     (check-equal?
                      '((() (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 0)
                      (apply-operator (list-ref ops 0) board 0)
                      )
                     )
  )

(module+ test (begin (reset-game)
                     (check-equal?
                      '(((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) ()) #f 7)
                      (apply-operator (list-ref ops 6) board 1)
                      )
                     )
  )


; Funcion que crea una copia del tablero (por motivos de seguridad)
(define (copy-board tablero)
  (let ((estado-copia null))
    (for ([elemento tablero])
      (set! estado-copia (cons elemento estado-copia)))
    (reverse estado-copia)))

(module+ test (begin (reset-game)
                     (check-equal? board (copy-board board))
                     )
  )
  
;función que mueve las semillas de la IA
(define (move-machine-balls tablero casilla-actual semillas-casilla)
  (define-values (semilla-a-meter cont estado semillas longitud-semillas estado-copia best-canca shoot-again casilla-target)
    (values null 0 null null 0 null 0 #f (+ 1 casilla-actual)))
  (set! estado-copia (copy-board tablero))
  (for ([can semillas-casilla])
    (set! semillas (cons can semillas))
    )
  (set! semillas (sort semillas >))
  (set! longitud-semillas (length semillas))
  
  (when (>= (length semillas) (- 13 casilla-actual))
    (begin
      (set! best-canca (car semillas))
      (set! best-canca (list-set estado-copia 13 (append estado-copia (list-ref estado-copia 13))))
      )
    )
  ;Si la longitud de tus semillas es igual a la semilla en la que te encuentras, la IA vuelve a tirar
  (when (= 0 (- (length semillas) (- 13 casilla-actual)))
    (set! shoot-again #t)
    (set! shoot-again #f))
  
  (for ([semilla semillas])
    (set! semilla-a-meter (car (list-ref estado-copia casilla-actual)))

    (if (and (equal? cont 0 ) (equal? best-canca semilla-a-meter))
        (set! cont (+ 1 cont))
        (begin
          (when (> casilla-target 13)
            (set! casilla-target 0))
          
          (set! estado-copia (list-set estado-copia casilla-target (append (list semilla-a-meter) (list-ref estado-copia casilla-target))))
          (set! casilla-target (+ 1 casilla-target)) 
          )
        )
    )
  (set! estado-copia (list-set estado-copia casilla-actual '()))
  (list estado-copia shoot-again casilla-actual)
  )

(module+ test (begin (reset-game)
                     (check-equal? '(((1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 2)
                                   (move-machine-balls board 2 (list-ref board 0))
                                   )                                   
                )
  )

;random function
(define (juega-random debug jugador)
  (define posibilidades (expand board jugador))
  (define rng (random (length posibilidades)))
  (set! board (car (list-ref posibilidades rng)))
  (when (equal? debug #t)
    (info-depuracion (list-ref (list-ref posibilidades rng) 2) jugador)
    )
  ;(printf "~a" (car (list-ref posibilidades rng)))
  )

(define (info-depuracion movimiento jugador-actual)
  (printf "Es el turno del jugador ~a.~%" jugador-actual)
  (printf "-----------------------------~%")
  (print-board)
  (printf "-----------------------------~%")
  (printf "Jugador ~a: elijo el movimiento: ~a~%" jugador-actual movimiento)
  (printf "La heuristica de este movimiento es: ~a~%" (heuristica-simple board))
  (printf "#############################~%")
  )

;heuristic-function. Mediante la operación (base1-base2)+(casillas1-casillas2)
(define (heuristica-simple estado)
  (define dif-casas (- (apply + (list-ref estado 6))
                       (apply + (list-ref estado 13))
                       ))
  (define dif-casillas(- (+ (apply + (list-ref estado 0))
                            (apply + (list-ref estado 1))
                            (apply + (list-ref estado 2))
                            (apply + (list-ref estado 3))
                            (apply + (list-ref estado 4))
                            (apply + (list-ref estado 5))
                            )
                         (+ (apply + (list-ref estado 7))
                            (apply + (list-ref estado 8))
                            (apply + (list-ref estado 9))
                            (apply + (list-ref estado 10))
                            (apply + (list-ref estado 11))
                            (apply + (list-ref estado 12))
                            )
                         ))
  (+ dif-casas dif-casillas)
  )

(module+ test (begin (reset-game)
                    (check-equal? 0 (heuristica-simple board))
                     )
  )

(module+ test (check-equal? 4 (heuristica-simple
                               '(() (1) (1) (1) (1) () (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                    () () () () () () (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
                               )
                            )
  )


;clase que encuentra el mejor movimiento mediante minmax
(define (min-max estado-tablero profundidad jugador)
  (define valorAct 0)
  (define cosaAux null)
  (define sucesores (expand estado-tablero jugador))
  (define mejor-mov null)
  ;(printf "~a~%" profundidad)
  (if (or (equal? profundidad 0) (equal? '() sucesores))
      (list estado-tablero 0 0)
      (begin
        (set! mejor-mov (car sucesores))
        (if (equal? jugador 0)
            (begin
              (for ([hijo sucesores])
                ;bucle que recorra todos los sucesores y pille el maximo
                (begin
                  (set! valorAct -1000000000)
                  (set! cosaAux (min-max (car hijo) (- profundidad 1) (change-player jugador)))
                  (when (> valorAct (heuristica-simple (car cosaAux)))
                    (begin
                      (set! valorAct (heuristica-simple (car cosaAux)))
                      (set! mejor-mov cosaAux)
                      )
                    )
                  )
                )
              ;(printf "mejor-mov: ~a~%" mejor-mov)
              mejor-mov
              )
            (begin
              (for ([hijo sucesores])
                ;bucle que recorra todos los sucesores y pille el minimo
                (begin
                  (set! valorAct 1000000000)
                  (set! cosaAux (min-max (car hijo) (- profundidad 1) (change-player jugador)))
                  (when (< valorAct (heuristica-simple (car cosaAux)))
                    (begin
                      (set! valorAct (heuristica-simple (car cosaAux)))
                      (set! mejor-mov cosaAux)
                      )
                    )
                  )
                )
              ;(printf "mejor-mov: ~a~%" mejor-mov)
              mejor-mov
              )
            )
        )
      )
  )

(module+ test (begin (reset-game)
                     (check-equal? (min-max board 2 0)
                                   '((() (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) ()
                                         (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 0)
                                   )
                     )
  )


(define (aplicar-min-max profundidad-max debug jugador-act)
  (define movimiento (min-max board profundidad-max jugador-act))
  (set! board (car movimiento))
  (when (equal? #t debug)
    (info-depuracion (list-ref movimiento 2) jugador-act)
    )
  
  )


(define (alfa-beta estado-tablero alfa beta profundidad jugador)
  (define valorAct 0)
  (define cosaAux null)
  (define sucesores (expand estado-tablero jugador))
  (define mejor-mov null)
  ;(printf "~a~%" profundidad)
  (if (or (equal? profundidad 0) (equal? '() sucesores))
      (list estado-tablero 0 0)
      (begin
        (set! mejor-mov (car sucesores))
        (if (equal? jugador 0)
            (begin
              (for ([hijo sucesores]
                    #:break (> beta alfa))
                ;bucle que recorra todos los sucesores y pille el maximo
                (begin
                  (set! valorAct -1000000000)
                  (set! cosaAux (min-max (car hijo) (- profundidad 1) (change-player jugador)))
                  (when (> valorAct (heuristica-simple (car cosaAux)))
                    (begin
                      (set! valorAct (heuristica-simple (car cosaAux)))
                      (set! alfa valorAct)
                      (set! mejor-mov cosaAux)
                      )
                    )
                  )
                )
              ;(printf "mejor-mov: ~a~%" mejor-mov)
              mejor-mov
              )
            (begin
              (for ([hijo sucesores]
                    #:break(> alfa beta))
                ;bucle que recorra todos los sucesores y pille el minimo
                (begin
                  (set! valorAct 1000000000)
                  (set! cosaAux (min-max (car hijo) (- profundidad 1) (change-player jugador)))
                  (when (< valorAct (heuristica-simple (car cosaAux)))
                    (begin
                      (set! valorAct (heuristica-simple (car cosaAux)))
                      (set! beta valorAct)
                      (set! mejor-mov cosaAux)
                      )
                    )
                  )
                )
              ;(printf "mejor-mov: ~a~%" mejor-mov)
              mejor-mov
              )
            )
        )
      )
  )

(module+ test (begin (reset-game)
                     (check-equal? '((() (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) ()
                                         (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 0)
                                   (alfa-beta board -1000 1000 2 0)
                                   )
                     )
  )

(define (aplicar-alfa-beta profundidad-max alfa beta debug jugador-act)
  (define movimiento (alfa-beta board alfa beta profundidad-max jugador-act))
  (set! board (car movimiento))
  (when (equal? #t debug)
    (info-depuracion (list-ref movimiento 2) jugador-act)
    )
  
  )

(define (change-player player)
  (if (equal? player 1)
      0
      1
      )
  )

(module+ test (check-equal? 0 (change-player 1)))
(module+ test (check-equal? 1 (change-player 0)))
    
;Funcion auxiliar para expand
(define (full-copy list)
  (if (null? list) 
      '() 
      (if (list? list) 
          (cons (full-copy (car list)) (full-copy (cdr list)))
          list)))

(module+ test (check-equal? board (full-copy board)))
     
;Funcion que aplica todos los operadores de *ops* a un estado determinado y los regresa en una lista
(define (expand estado jugador)
  (define-values (sucesores nuevo-estado estado-copia)
    (values null null (full-copy estado)))
  (for ([operador ops]) 
    (when (valid-operator? operador estado-copia jugador)
      (begin
        (set! nuevo-estado (apply-operator operador estado-copia jugador))
        ;(printf "~a " nuevo-estado)
        (set! sucesores (append sucesores (list nuevo-estado)))
        )
      )
    )
  sucesores
  )

(module+ test (begin (reset-game)
                     (check-equal? '(((() (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 0)
                                     (((1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 1)
                                     (((1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 2)
                                     (((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 3)
                                     (((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 4)
                                     (((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()) #f 5))
                                   (expand board 0)
                                   )
                     )
  )

(module+ test (begin (reset-game)
                     (check-equal? '((((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) ()) #f 7)
                                     (((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1)) #f 8)
                                     (((1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1)) #f 9)
                                     (((1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1 1 1 1 1 1) (1)) #f 10)
                                     (((1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1 1) (1)) #f 11)
                                     (((1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1)) #f 12))
                                   (expand board 1)
                                   )
                     )
  )

;play randomvsrandom
(define (play-random debug jugador1)
  (if (equal? (game-ended?) #f)
      (begin
        (juega-random debug jugador1)
        (play-random debug (change-player jugador1))
        )
      (begin
        (printf "Partida terminada, ganó el jugador: ~a~%" (ganador? board))
        (printf "Estado final del tablero:~%")
        (print-board)
        (printf "La victoria ha sido por una diferencia de ~a puntos" (heuristica-simple board))
        )
      )
  )

(define (prueba-random)
  (printf "Comienza el jugador 1, ambos con estratergia random~%")
  (printf "###########################~%")
  (play-random #t 1)
  )

(define (prueba-multi-random cantidad)
  
 )

;miniMax
(define (play-min-max debug profundidad-max jugador1)
  (if (equal? (game-ended?) #f)
      (begin
        (aplicar-min-max profundidad-max debug jugador1)
        (play-min-max debug profundidad-max (change-player jugador1))
        )
      (begin
        (printf "Partida terminada, ganó el jugador: ~a~%" (ganador? board))
        (printf "Estado final del tablero:~%")
        (print-board)
        )
      )
  )


;
(define (play-alfa-beta debug profundidad-max alfa beta jugador1)
  (if (equal? (game-ended?) #f)
      (begin
        (aplicar-alfa-beta profundidad-max alfa beta debug jugador1)
        (play-alfa-beta debug profundidad-max alfa beta (change-player jugador1))
        )
      (begin
        (printf "Partida terminada, ganó el jugador: ~a~%" (ganador? board))
        (printf "Estado final del tablero:~%")
        (print-board)
        )
      )
  )
