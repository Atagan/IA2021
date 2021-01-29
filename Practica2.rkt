#lang racket

; Declaración de variables globales

(define board '((1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) () (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) ()))
(define shoot-again true)
(define slots-shooted null)
(define alfa 10000000)
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
(define end-game null)
(define winner-player null)
(define IA1-points 0) 
(define IA2-points 0)
(define jugador-actual 0)
(define depuracion #f)

; Función que imprime el tablero del mancala, en cada casilla imprime el valor de la suma de las canicas que se encuentran en esa casilla
(define (print-board)
  (printf   "~% ~% | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A | ~%"
           (apply + (list-ref board 13))(apply + (list-ref board 12)) (apply + (list-ref board 11)) (apply + (list-ref board 10)) (apply + (list-ref board 9 )) (apply + (list-ref board 8)) (apply + (list-ref board 7)))
  (printf   "~% | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A |  | ~A | ~%~%"
           (apply + (list-ref board 0)) (apply + (list-ref board 1)) (apply + (list-ref board 2)) (apply + (list-ref board 3)) (apply + (list-ref board 4)) (apply + (list-ref board 5)) (apply + (list-ref board 6))))

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

; Funcion que obtiene las canicas en una casilla
(define (get-balls casilla)
  (list-ref board casilla))

; Funcion que inserta las canicas de la forma (canica canica canica ...) en las casillas aledañas
(define (insert-ball lista casilla)
  (define casilla-siguiente (+ 1 casilla))
  (for ([x lista])
    (set! board (list-set board casilla-siguiente (append (list x) (list-ref board casilla-siguiente))))
    (set! slots-shooted (list-set slots-shooted casilla-siguiente (append (list x) slots-shooted)))
    (set! casilla-siguiente (+ 1 casilla-siguiente))
    (when (> casilla-siguiente 13)
        (set! casilla-siguiente 0)
    )
   )
 )

; Predicado que valida si es el operador seleccionado es valido
(define (valid-operator? operador estado)
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


; Funcion que aplica un operador de *ops* a un estado determinado
(define (apply-operator operador estado)
  (define casilla-actual (car(cdr operador)))
  (define-values (ops canicas-casilla estado-resultado)
    (values (car operador)  (get-balls casilla-actual) null))
  ;(printf "~a~%" ops)
  (if (equal? jugador-actual 0)
      (case ops
        [(:primera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [(:segunda-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [(:tercera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [(:cuarta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [(:quinta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [(:sexta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
        [else (printf "error")]
        )
     (case ops
       [(:septima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [(:octaba-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [(:novena-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [(:decima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [(:undecima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [(:duodecima-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
       [else (printf "error")]
       )
     )
  estado-resultado
  )

; Funcion que crea una copia del tablero (por motivos de seguridad)
(define (copy-board tablero)
  (let ((estado-copia null))
  (for ([elemento tablero])
    (set! estado-copia (cons elemento estado-copia)))
    (reverse estado-copia)))
  
;función que mueve las canicas de la IA
(define (move-machine-balls tablero casilla-actual canicas-casilla)
  (define-values (canica-a-meter cont estado canicas longitud-canicas estado-copia best-canca shoot-again casilla-target)
    (values null 0 null null 0 null 0 #f (+ 1 casilla-actual)))
  (set! estado-copia (copy-board tablero))
  (for ([can canicas-casilla])
    (set! canicas (cons can canicas))
   )
  (set! canicas (sort canicas >))
  (set! longitud-canicas (length canicas))
  
  (when (>= (length canicas) (- 13 casilla-actual))
    (begin
      (set! best-canca (car canicas))
      (set! best-canca (list-set estado-copia 13 (append estado-copia (list-ref estado-copia 13))))
      )
    )
  ;Si la longitud de tus canicas es igual a la canica en la que te encuentras, la IA vuelve a tirar
  (when (= 0 (- (length canicas) (- 13 casilla-actual)))
      (set! shoot-again #t)
      (set! shoot-again #f))
  
  (for ([canica canicas])
    (set! canica-a-meter (car (list-ref estado-copia casilla-actual)))

    (if (and (equal? cont 0 ) (equal? best-canca canica-a-meter))
        (begin
         (set! cont (+ 1 cont)))
        (begin
          (when (> casilla-target 13)
           (set! casilla-target 0))
          
         (set! estado-copia (list-set estado-copia casilla-target (append (list canica-a-meter) (list-ref estado-copia casilla-target))))
         (set! casilla-target (+ 1 casilla-target)) 
         )
        )
   )
  (set! estado-copia (list-set estado-copia casilla-actual '()))
  (list estado-copia shoot-again casilla-actual)
 )

;random function
(define (juega-random debug)
  (define posibilidades (expand board))
  (define rng (random (length posibilidades)))
  (when (equal? debug #t)
    (info-depuracion (list-ref (list-ref posibilidades rng) 2))
    )
  
  ;(printf "~a" (car (list-ref posibilidades rng)))
  (set! board (car (list-ref posibilidades rng)))
  (print-board)
  )

(define (info-depuracion movimiento)
  (printf "Es el turno del jugador ~a.~%" jugador-actual)
  (printf "-----------------------------~%")
  (print-board)
  (printf "-----------------------------~%")
  (printf "Jugador ~a: elijo el movimiento: ~a~%" jugador-actual movimiento)
  (printf "#############################")
  )

;heuristic-function

; Funcion la cual cambia de jugador, si es 0 --> 1 (le toca a la IA1) y si es 1 --> 0 (le toca al IA2)
(define (change-player)
  (if (equal? jugador-actual 0)
      (set! jugador-actual 1)
      (set! jugador-actual 0)
      )
  )
    
;Funcion auxiliar para expand
(define (full-copy list)
(if (null? list) 
  '() 
  (if (list? list) 
      (cons (full-copy (car list)) (full-copy (cdr list)))
      list)))
     
;Funcion que aplica todos los operadores de *ops* a un estado determinado y los regresa en una lista
(define (expand estado)
  (define-values (sucesores nuevo-estado estado-copia)
    (values null null (full-copy estado)))
  (for ([operador ops]) 
    (when (valid-operator? operador estado-copia)
      (begin
        (set! nuevo-estado (apply-operator operador estado-copia))
        ;(printf "~a " nuevo-estado)
        (set! sucesores (append sucesores (list nuevo-estado)))
        )
      )
    )
  sucesores
  )

;play randomvsrandom
(define (play-random debug)
  (if (equal? (game-ended?) #f)
      ((juega-random debug)
       (change-player)
       (play-random #t))
      (printf "Partida terminada, ganó el jugador: ~a" jugador-actual)
      )
  )


;miniMax
