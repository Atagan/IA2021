#lang racket

; Declaración de variables globales

(define board '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()))
(define shoot-again true)
(define slots-shooted null)
(define alfa 10000000)
(define ops '((:primera-casilla 7)
                      (:segunda-casilla 8)
                      (:tercera-casilla 9)
                      (:cuarta-casilla 10)
                      (:quinta-casilla 11)
                      (:sexta-casilla 12)))
(define end-game null)
(define winner-player null)
(define IA1-points 0) 
(define IA2-points 0)

; Función que imprime el tablero del mancala, en cada casilla imprime el valor de la suma de las canicas que se encuentran en esa casilla
<<<<<<< HEAD
;(define (print-board)
;  (format  t   "~&~% | ~A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%"
;           (apply #'+ (list-ref 13 *board*))(apply #'+ (list-ref 12 *board*)) (apply #'+ (list-ref 11 *board*)) (apply #'+ (list-ref 10 *board*)) (apply #'+ (list-ref 9 *board*)) (apply #'+ (list-ref 8 *board*)) (apply #'+ (list-ref 7 *board*)))
;  (format  t   "~& | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%~%"
;           (apply #'+ (list-ref 0 *board*)) (apply #'+ (list-ref 1 *board*)) (apply #'+ (list-ref 2 *board*)) (apply #'+ (list-ref 3 *board*)) (apply #'+ (list-ref 4 *board*)) (apply #'+ (list-ref 5 *board*)) (apply #'+ (list-ref 6 *board*))))
=======
(define (print-board)
  (printf   "~&~% | ~A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%"
           (apply #'+ (list-ref board 13))(apply #'+ (list-ref board 12)) (apply #'+ (list-ref board 11)) (apply #'+ (list-ref board 10)) (apply #'+ (list-ref board 9 )) (apply #'+ (list-ref board 8)) (apply #'+ (list-ref board 7)))
  (printf   "~& | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%~%"
           (apply #'+ (list-ref board 0)) (apply #'+ (list-ref board 1)) (apply #'+ (list-ref board 2)) (apply #'+ (list-ref board 3)) (apply #'+ (list-ref board 4)) (apply #'+ (list-ref board 5)) (apply #'+ (list-ref board 6))))
>>>>>>> 4d6b2cb1242d31f5a6d47b698fa040c6b40deb17

; Funcion que reinicia el tablero a su estado original
(define (reset-game)
  (set! board '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)())))

; Predicado el cual valida si el juego ya terminó comprobando si alguna hilera esta completamente vacía
(define (game-ended?)
  (game-ended? (or (and
                     (null (list-ref board 0))
                     (null (list-ref board 1))
                     (null (list-ref board 2))
                     (null (list-ref board 3))
                     (null (list-ref board 4))
                     (null (list-ref board 5)))
                   (and
                    (null (list-ref board 7))
                    (null (list-ref board 8))
                    (null (list-ref board 9))
                    (null (list-ref board 10))
                    (null (list-ref board 11))
                    (null (list-ref board 12))))))

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

; Funcion que mueve las canicas de la casilla seleccionada


; Predicado que valida si es el operador seleccionado es valido
(define (valid-operator? operador estado)
  (let ((operador (second operador)))
    (cond ((= operador 7)
           (if (null (list-ref estado 7)) null true))
          ((= operador 8)
           (if (null (list-ref estado 8)) null true))
          ((= operador 9)
           (if (null (list-ref estado 9)) null true))
          ((= operador 10)
           (if (null (list-ref estado 10)) null true))
          ((= operador 11)
           (if (null (list-ref estado 11)) null true))
          ((= operador 12)
           (if (null (list-ref estado 12)) null true))
          (true null))))

; Funcion que aplica un operador de *ops* a un estado determinado
(define (apply-operator operador estado)
  (let* ((ops (first operador))
         (casilla-actual (second operador))
         (canicas-casilla (get-balls casilla-actual))
         (estado-resultado null))
    (case 'ops
      [(primera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [(segunda-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [(tercera-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [(cuarta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [(quinta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [(sexta-casilla) (set! estado-resultado (move-machine-balls estado casilla-actual canicas-casilla))]
      [else true "Error"])
    estado-resultado))

; Funcion que crea una copia del tablero (por motivos de seguridad)
(define (copy-board tablero)
  (let ((estado-copia null))
  (for ([elemento tablero])
    (set! estado-copia (cons elemento estado-copia)))
  (set! estado-copia  (reverse estado-copia))))
  
;función que mueve las canicas de la IA
(define (move-machine-balls tablero casilla-actual canicas-casilla)
  (define-values (canica-a-meter cont estado canicas longitud-canicas estado-copia best-canca shoot-again casilla-target)
    (values null 0 null null 0 null 0 null (+ 1 casilla-actual)))
  (set! estado-copia (copy-board tablero))

  (for ([can canicas-casilla])
    (set! canicas (cons can canicas))
   )
  (set! canicas (sort canicas >))
  (set! longitud-canicas (length canicas))
  
  (when (>= (length canicas) (- 13 casilla-actual))
    (begin
      (set! best-canca (car canicas))
      (set! best-canca (list-set best-canca 13 (append estado-copia (list-ref best-canca 13))))
      )
    )
  ;Si la longitud de tus canicas es igual a la canica en la que te encuentras, la IA vuelve a tirar
  (when (= 0 (- (length canicas) (- 13 casilla-actual)))
      (set! shoot-again #t)
      (set! shoot-again null))

  (for ([canica canicas])
    (set! canica-a-meter (car (list-ref  estado-copia casilla-actual)))

    (if (and (= cont 0 ) ( = best-canca canica-a-meter))
        (begin
         (set! cont (+ 1 cont)))
        (begin
          (when (> casilla-target 12)
           (set! casilla-target 0))
         
         (set! canica-a-meter (list-set canica-a-meter casilla-target (append estado-copia (list-ref canica-a-meter casilla-target))))
         (set! casilla-target (+ 1 casilla-target))
         )
        )
   )
  (list estado-copia shoot-again casilla-actual)
 )
;heuristic-function

; Funcion la cual cambia de jugador, si es 0 --> 1 (le toca a la IA1) y si es 1 --> 0 (le toca al IA2)
(define (change-player jugador)
  (case jugador
    [(0) 1]
    [(1) 0]))
    
;Funcion auxiliar para expand
(define (full-copy list)
(if (null? list) 
  '() 
  (if (list? list) 
      (cons (full-copy (car list)) (full-copy (cdr list)))
      list)))
     
;Funcion que aplica todos los operadores de *ops* a un estado determinado y los regresa en una lista
(define (expand estado)
  (let* ((sucesores null)
         (nuevo-estado null)
         (estado-copia (full-copy estado)))
    (for ([operador ops]) 
         (when (valid-operator? operador estado-copia)
             (begin
               (set! nuevo-estado (apply-operator operador estado-copia))
               (cons nuevo-estado sucesores))
             )
        sucesores
      )
    )
  )

;miniMax
