(defun c:segundero ()
  (setq minutero (entget(handent "323")))
  (setq angulo 0)
  (setq time_s 60)
  

  (while (> time_s 0)

  (setq angulo (- angulo (/ (* pi 2) 60)))
  (setq cambio-angulo (subst (cons 50 angulo) (assoc 50 minutero) minutero))
  (entmod cambio-angulo)



  (setq time_s (- time_s 1))
  (command "delay" "1000")
  )


  ;(setq start-time (getvar "CDATE"))
  ;;(setq end-time (+ start-time 60.0))
  
;;  (setq interrupted nil) ; Variable para controlar la interrupción
  
 ;; (repeat 60
  ;;  (if (eq (getvar "LASTPROMPT") "c") ; Condición de interrupción
   ;;   (progn
   ;;     (setq interrupted t) ; Establece la bandera de interrupción
    ;;    (print "Bucle interrumpido.")
     ;; )
  ;;  )
    
  ;;  (if interrupted
   ;;   (return) ; Sale del bucle si se ha activado la interrupción
   ;; )
    
   ;; (command "'delay" "1") ; Retraso de 1 segundo sin bloquear la interfaz
   ;; (print "Hola, AutoCAD!") ; Mensaje cada segundo
;;  )
 ;; (princ)
)
