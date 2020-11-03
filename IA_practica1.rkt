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


(define (generarSala mapa bloqueos puertas coordenadas)
  (cond [(member coordenadas bloqueos) (generarSala(sustituir mapa (list-ref coordenadas 0) (list-ref coordenadas 1) "#") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa)))]
                 [(member coordenadas puertas) (generarSala(sustituir mapa (list-ref coordenadas 0) (list-ref coordenadas 1) ":") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 [(equal? coordenadas (get-initial-state)) (generarSala(sustituir mapa (list-ref coordenadas 0) (list-ref coordenadas 1) "x") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 [(equal? coordenadas (get-end-state (length (car mapa)) (length mapa))) (generarSala(sustituir mapa (list-ref coordenadas 0) (list-ref coordenadas 1) "X") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 ;meter la condición de que haya acabado
                 [else (sustituir mapa (list-ref coordenadas 0) (list-ref coordenadas 1) "_") ])
  )

(define (obtenerNuevaCoordenada coordenadas mapa) ;lo que el nombre indica, retorna una tupla que es la siguiente coordenada recorriendo el mapa de arriba a abajo de izquierda a derecha
  (cond [(< (list-ref coordenadas 1) (length (car mapa))) (cons (+ 1 (list-ref coordenadas 0)) (list-ref coordenadas 1) )]
        [else (cons (list-ref coordenadas 0) (+ 1 (list-ref coordenadas 1)))]
   )
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
  (for* ([i (length mapa)]
         [j (length (car mapa))])
    
    (printf " ")
    (printf "~a" (list-ref (list-ref mapa i)j))
    (when (= j (- (length (car mapa)) 1))(printf "~a" "\n"))
    )
  )