#lang racket

(module+ test (require rackunit))

(define (get-initial-state) (cons 1 1)) ;devuelve la casilla 1 1

(module+ test (check-equal? (get-initial-state) (cons 1 1)))

(define (get-end-state rows columns) (cons (- rows 2) (- columns 2))) ;devuelve la casilla n-2 n-2

(module+ test (check-equal? (get-end-state 10 10) (cons 8 8)))

(define (generate-positions rows columns) ;genera y devuelve una serie de coordenadas respectivas a todo el tablero
  (define all-positions (for*/list ([i rows][j columns]) (cons i j)));cambiar
  (let ([initial-state (get-initial-state)]
        [end-state (get-end-state rows columns)])
  (remove initial-state (remove end-state all-positions))))

(module+ test (test-begin
  (define positions (generate-positions 5 5))
  (check-equal? (length positions) 23)
  (check-false (member (get-initial-state) positions))
  (check-false (member (get-end-state 5 5) positions))))

(define (pick-random list n);lo que el nombre indica
           (for/fold
               ([source list]
                [picked '()])
                ([i n])
           (let* ([r (random (length source))]
                  [value (list-ref source r)])
               (values (remove value source) (cons value picked)))))

(module+ test (test-begin
   (define-values (remaining picked) (pick-random (list 1 2 3) 2))
   (check-equal? (length remaining) 1)
   (check-equal? (length picked) 2)
   (check-not-false (or (member 1 remaining) (member 1 picked)))
   (check-not-false (or (member 2 remaining) (member 2 picked)))
   (check-not-false (or (member 3 remaining) (member 3 picked)))
   ))

;(imprimirMapa(generarMapa 10 5 10 5))
(define (generarMapa filas columnas bloqueos puertas)
  (define-values (free-positions block-positions) (pick-random (generate-positions filas columnas) bloqueos))
  (define-values (final-free-positions door-positions) (pick-random free-positions puertas))

  (define mapa (make-list filas (make-list columnas "_")))

  
  (generarSala mapa block-positions door-positions (cons 0 0))
  )

(define (generarSala mapa bloqueos puertas coordenadas)  
  ;(imprimirMapa mapa)
  (if (equal? (car coordenadas) (length mapa)) mapa
      (cond [(member coordenadas bloqueos) (generarSala(sustituir mapa (car coordenadas ) (cdr coordenadas) "#") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 [(member coordenadas puertas) (generarSala(sustituir mapa (car coordenadas) (cdr coordenadas) ":") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 [(equal? coordenadas (get-initial-state)) (generarSala(sustituir mapa (car coordenadas ) (cdr coordenadas) "x") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 [(equal? coordenadas (get-end-state (length (car mapa)) (length mapa))) (generarSala(sustituir mapa (car coordenadas) (cdr coordenadas) "X") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 ;meter la condición de que haya acabado
                 [else (generarSala mapa bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa)) ])
  )
  )

(define (obtenerNuevaCoordenada coordenadas mapa) ;lo que el nombre indica, retorna una tupla que es la siguiente coordenada recorriendo el mapa de arriba a abajo de izquierda a derecha
  (cond[(> (cdr coordenadas) (- (length (car mapa)) 1 )) (cons (+ 1 (car coordenadas)) 0)]
   [else (cons (car coordenadas) (+ 1 (cdr coordenadas)))])
 )

(define (sustituir mapa fila col valor)
  (list-set mapa fila (list-set (list-ref mapa fila) col valor))
  )



(define (generate-room rows columns blocks doors) ;genera la sala, incluyendo el número de obstaculos y puertas??, seleccionados
  ;estas dos lineas tendrán que ir fuera del bloque recursivo y sus valores ser pasados como parametro
  (define-values (free-positions block-positions) (pick-random (generate-positions rows columns) blocks))
  (define-values (final-free-positions door-positions) (pick-random free-positions doors))
  
  (define empty-list (build-list (* rows columns) (lambda (x) 0)))
  (reverse (for*/fold ;cambiar
                 ([superlist '()]
                  [sublist '()]
                  #:result superlist);qué puto cojones
                 ([i rows][j columns])

         (define character
           (cond [(member (cons i j) block-positions) "#"]
                 [(member (cons i j) door-positions) ":"]
                 [(equal? (cons i j) (get-initial-state)) "x"]
                 [(equal? (cons i j) (get-end-state rows columns)) "X"]
                 [else "_"]));todo este cond ha de estar igual              
         (if (= j (- columns 1))
             (values (cons (reverse (cons character sublist)) superlist) empty); pone la sublista tras el nuevo caracter, le da la vuelta y lo pone frente a la superlista
             (values superlist (cons character sublist))))))

(define (imprimirMapa mapa)
  (printf "------------------")
  (printf "~a" "\n")
  (for* ([i (length mapa)]
         [j (length (car mapa))])
    
    (printf " ")
    (printf "~a" (list-ref (list-ref mapa i)j))
    (when (= j (- (length (car mapa)) 1))(printf "~a" "\n"))
    )
  (printf "------------------")
  (printf "~a" "\n")
  )


;Función que comprueba si es una casilla vacía
(define(checkPosicionVacia mapa fila columna)
  (if (string=? (list-ref(list-ref mapa fila) columna) "_") #t #f))

;Función que comprueba si hay una puerta
(define(checkPuerta mapa fila columna)
  (if (string=? (list-ref(list-ref mapa fila) columna) ":") #t #f))

;Función que comprueba si hay una pared
(define(checkPared mapa fila columna)
  (if (string=? (list-ref(list-ref mapa fila) columna) "#") #t #f))

;Función que comprueba si es meta
(define(checkMeta mapa fila columna)
  (if (string=? (list-ref(list-ref mapa fila) columna) "X") #t #f))



;Funcion auxiliar para ABIERTOS
(define (comprobarAbiertos coordenadas)
  (list (cons (car coordenadas) (- (cdr coordenadas) 1)) (cons (- (car coordenadas) 1) (cdr coordenadas)) (cons (car coordenadas) (+ (cdr coordenadas) 1)) (cons (+ (car coordenadas) 1) (cdr coordenadas)))
)


(define (busqAnchura coordenadas mapa abiertos cerrados)
  (if (> (list-ref (length (car mapa)) (car coordenadas))) #f
      ((list cerrados coordenadas);añadimos la posición actual a cerrados
      (list abiertos (comprobarAbiertos coordenadas));generamos todos los abiertos de la posición actual y los añadimos por el final a los anteriores
      )
      ;usar un for/or 
      )
  )