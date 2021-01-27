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

;funcion que cuenta el numero de ocurrencias de un caracter en una lista SOLO USADA PARA TESTS
(define (count-letters letter lst)
  (count (lambda (e) (string=? e letter))
         lst))

(module+ test (check-equal? (count-letters ":" (flatten (generarMapa 10 5 0 10))) 10));comprueba el numero de puertas generadas
(module+ test (check-equal? (count-letters "#" (flatten (generarMapa 4 8 5 0))) 5));comprueba el numero de paredes generadas
(module+ test (check-equal? (count-letters "x" (flatten (generarMapa 3 10 0 10))) 1));comprueba que se genera la posicion inicial
(module+ test (check-equal? (count-letters "X" (flatten (generarMapa 4 20 0 10))) 1));comprueba que se genera la posicion final

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
                 [(equal? coordenadas (get-end-state (length  mapa) (length (car mapa)))) (generarSala(sustituir mapa (car coordenadas) (cdr coordenadas) "X") bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa))]
                 ;meter la condición de que haya acabado
                 [else (generarSala mapa bloqueos puertas (obtenerNuevaCoordenada coordenadas mapa)) ])
  )
  )

(define (generarCamino mapa cerrados coordenadas)  
  ;(imprimirMapa mapa)
  (if (equal? (car coordenadas) (length mapa)) mapa
      (cond [(member coordenadas cerrados) (generarCamino(sustituir mapa (car coordenadas ) (cdr coordenadas) "o") cerrados (obtenerNuevaCoordenada coordenadas mapa))]
            [else (generarCamino mapa cerrados (obtenerNuevaCoordenada coordenadas mapa)) ])
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

;funcion auxiliar que añade los nuevos abiertos y elimina el nodo a extraer
(define (nuevosAbiertos abiertos coordenadas)
  (remove-duplicates(remove coordenadas (append abiertos (comprobarAbiertos coordenadas))))
 )


;función que elimina valores repetidos en dos listas (eliminarDobles? '((0 1) (1 0) (1 2) (2 1)) '((0 1)) '())
(define (eliminarDobles? lista1 lista2 lista3)
   (if (null? lista1) lista3
        (if (member (car lista1) lista2) (eliminarDobles? (rest lista1) lista2 lista3)
            (eliminarDobles? (rest lista1) lista2 (cons (car lista1) lista3))
            )
        )
  )
  
;(arreglarAbiertos (list (cons 0 1) ( cons 1 0) (cons 1 2) ( cons 2 1) ( cons -1 -1)) (list (cons 0 1)) (generarMapa 10 5 10 5))
;funcion auxliar que elimine nodos de abiertos si ya están en cerrados
(define (arreglarAbiertosCoste abiertos cerrados mapa)
  (set! abiertos (eliminarDobles? abiertos cerrados '()))
  (define abiertos2 abiertos)
  (for ([i (length abiertos)])
    (if(coordenadaValida mapa (list-ref abiertos i))#t
       (set! abiertos2 (remove (list-ref abiertos i) abiertos2));abiertos2.remove(abiertos.get(i))
     )
   )

  (define puertas '())
  (define vacios '())
  (for ([i (length abiertos2)]);esto lo que hace es crear una lista de nodos hijo ordenada ascencientemente por coste.
    (if(checkPuerta mapa (car(list-ref abiertos2 i))(cdr(list-ref abiertos2 i)))
       (set! puertas (append puertas (list (list-ref abiertos2 i))))
       (set! vacios (append vacios (list (list-ref abiertos2 i))))
       )
    )

  (append vacios puertas)
 )

(define (arreglarAbiertosNoCoste abiertos cerrados mapa)
  (set! abiertos (eliminarDobles? abiertos cerrados '()))
  (define abiertos2 abiertos)
  (for ([i (length abiertos)])
    (if(coordenadaValida mapa (list-ref abiertos i))#t
       (set! abiertos2 (remove (list-ref abiertos i) abiertos2))
     )
   )
 abiertos2
 )

;comprobar que unas coordenadas estén dentro del tablero
(define (coordenadaValida mapa coordenada) 
  (if(>(cdr coordenada) (- (length (car mapa)) 1))#f
     (if (>(car coordenada) (- (length mapa) 1))#f
         (if(> 0 (car coordenada))#f
            (if(> 0 (cdr coordenada))#f
               (if(checkPared mapa (car coordenada) (cdr coordenada))#f
                  #t
                  )
               )
            )
         )
     )
  )



(define (busqCosteUniforme coordenadas mapa abiertos cerrados)
   (if  (member (get-end-state (length  mapa) (length (car mapa))) cerrados) cerrados
      (if (eq? '() (arreglarAbiertosCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa))#f
          (busqCosteUniforme (list-ref (arreglarAbiertosCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) 0) mapa (arreglarAbiertosCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) (append cerrados (list(list-ref (arreglarAbiertosCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) 0))))
          )
      )
  )


(define (busqAnchura coordenadas mapa abiertos cerrados)
  ;(printf "entro en recursividad")
  ;(printf "~a" cerrados)
  ;(printf "~a" "\n")
  (if  (member (get-end-state (length  mapa) (length (car mapa))) cerrados) cerrados
      (if (eq? '() (arreglarAbiertosNoCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa))#f
          (busqAnchura (list-ref (arreglarAbiertosNoCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) 0) mapa (arreglarAbiertosNoCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) (append cerrados (list(list-ref (arreglarAbiertosNoCoste (nuevosAbiertos abiertos coordenadas) cerrados mapa) 0))))
          )
      )
  )

(define (comenzarBusqueda)
  (define mapa '(()))
  (set! mapa (generarMapa 7 5 7 5))
  (printf "Mapa Original")
  (printf "~a" "\n")
  (imprimirMapa mapa)
  (printf "Busqueda en Anchura Desinformada sin Costes")
  (printf "~a" "\n")
  (imprimirMapa(generarCamino mapa (busqAnchura (get-initial-state) mapa (arreglarAbiertosNoCoste (nuevosAbiertos '() (cons 1 1)) '() mapa) (list (get-initial-state))) (get-initial-state)))
  (printf "Algoritmo A* con función heuristica Costante")
  (printf "~a" "\n")
  (imprimirMapa(generarCamino mapa (busqCosteUniforme (get-initial-state) mapa (arreglarAbiertosNoCoste (nuevosAbiertos '() (cons 1 1)) '() mapa) (list (get-initial-state))) (get-initial-state)))
 )