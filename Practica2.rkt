#lang racket


(define (insert-ball lista casilla)
  (define casilla-siguiente (+ 1 casilla))
  (for ([x lista])
    (set! board (list-set board casilla-siguiente (append (list x) (list-ref board casilla-siguiente))))
    (set! slots-shooted (list-set slots-shooted casilla-siguiente (append (list x) slots-shooted)))
    (set! casilla-siguiente (+ 1 casilla-siguiente))
    (if (> casilla-siguiente 13)
        (setq casilla-siguiente 0)
    )
   )
 )

(defun move-ball (casilla-actual)
  "Funcion que mueve las canicas de la casilla seleccionada"
  (let ((canicas (get-balls casilla-actual))
        (movimiento nil))
    (format t "~& Canicas en casilla:  ~A ~%" canicas)
    (format t "~& Indique la casilla a la cual ira cada canica de la forma (canica canica canica ...)~%")
    (setq movimiento(read))
    (insert-ball movimiento casilla-actual)
    (loop for canica in canicas do
         (pop (nth casilla-actual *board*)))
    (print-board)))