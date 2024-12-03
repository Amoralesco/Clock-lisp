(defun c:initClock ()
  (c:initAnalogicClock)
)


(defun mod (a b);calculo del modulo
  (rem a b))



(defun prependZero (str)
  (if (= (strlen str) 1)
    (setq str (strcat "0" str))
  )
  str ; Asegurarse de devolver la cadena modificada
)



(defun updateMod()
    ;ACTUALIZACIÓN RELOJ ANALOGICO
    (setq change-second (subst (cons 50 agl-second) (assoc 50 second_hand) second_hand)) ; cambia el angulo del segundero
    (setq change-minute (subst (cons 50 agl-minute) (assoc 50 minute_hand) minute_hand))    ; cambia el angulo del minutero
    (setq change-hour (subst (cons 50 agl-hour) (assoc 50 hour_hand) hour_hand)) ; cambia el angulo del horarrio
  
    (entmod change-second) ; actualiza el segundero
    (entmod change-minute)  ; actualiza el minutero
    (entmod change-hour) ; actualiza el horarrio

    ;ACTUALIZACIÓN RELOJ DIGITAL 

    (setq period_s (cond ((= period 0) "AM") (t "PM"))) ; cambia el periodo del reloj digital
    (setq day_of_week (nth h day_name)) ; cambia el dia de la semana del reloj digital

    (setq change-second-logic (subst (cons 1 (prependZero (itoa second))) (assoc 1 second_logic) second_logic)) ; cambia el segundo del reloj digital
    (setq change-minute-logic (subst (cons 1 (prependZero (itoa minute))) (assoc 1 minute_logic) minute_logic))    ; cambia el minuto del relol digital
    (setq change-hour-logic (subst (cons 1 (prependZero (itoa hour))) (assoc 1 hour_logic) hour_logic)) ; cambia el horario del reloj digital
    (setq change-period-logic (subst (cons 1 period_s) (assoc 1 period_logic) period_logic)) ; cambia el periodo del reloj digital
    (setq change-day-logic (subst (cons 1 (prependZero (itoa day))) (assoc 1 day_logic) day_logic)) ; cambia el dia del reloj digital
    (setq change-dayname (subst (cons 1 day_of_week) (assoc 1 dayname_logic) dayname_logic)) ; cambia el dia de la semana del reloj digital
    (setq change-month-logic (subst (cons 1 (nth month month_name)) (assoc 1 month_logic) month_logic)) ; cambia el mes del reloj digital
    (setq change-year-logic (subst (cons 1 (itoa year)) (assoc 1 year_logic) year_logic)) ;  cambia el año del reloj digital

    (entmod change-second-logic) ; actualiza el segundo del reloj digital
    (entmod change-minute-logic)  ; actualiza el minuto del reloj digital
    (entmod change-hour-logic) ; actualiza el horario del reloj digital
    (entmod change-period-logic) ; actualiza el periodo del reloj digital
    (entmod change-day-logic) ; actualiza el dia del reloj digital
    (entmod change-dayname) ; actualiza el dia de la semana del reloj digital
    (entmod change-month-logic) ; actualiza el mes del reloj digital
    (entmod change-year-logic) ; actualiza el año del reloj digital
)


(defun c:setActualTime (); obtiene la fecha y hora actual
    (setq string_date (rtos (* (getvar "CDATE") (expt 10 6)) 2 0)) ; obtiene el valor de la fecha en formato string
    (setq string_date "20241231235900") ; obtiene el valor de la hora en formato string

    (setq year (atoi (substr string_date 1 4))) ; obtiene el año en formato string
    (setq month (- (atoi (substr string_date 5 2)) 1)) ; obtiene el mes en formato string
    (setq day (atoi (substr string_date 7 2))) ; obtiene el dia en formato string
    (setq hour (mod (atoi (substr string_date 9 2)) 12)) ; obtiene la hora
    (setq period (cond ((> (atoi (substr string_date 9 2)) 12) 1) (T 0))) ; obtiene el periodo 0 es AM y 1 es PM
    (setq minute (atoi (substr string_date 11 2))) ; obtiene los minutos
    (setq second (atoi (substr string_date 13 2))) ; obtiene los segundos

    (setq month_name '("Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Septiembre" "Octubre" "Noviembre" "Diciembre")) ;meses
    (setq day_name '("Lunes" "Martes" "Miercoles" "Jueves" "Viernes" "Sabado"  "Domingo")) ;dias de la semana

    (setq second_hand (entget(handent "46E")));46E segundero
    (setq minute_hand (entget(handent "47F")));47F minutero
    (setq hour_hand (entget(handent "489")));489 horarrio
    (setq second_logic (entget(handent "53F"))); 53segundero del analogo
    (setq minute_logic (entget(handent "531"))); 531 minutero del analogo
    (setq hour_logic (entget(handent "52A"))); 52A horarrio del analogo
    (setq period_logic (entget(handent "538"))); 538 periodo del analogo
    (setq day_logic (entget(handent "5DA"))); 5DA dia del analogo
    (setq dayname_logic (entget(handent "51C"))); 51C semanario del analogo
    (setq month_logic (entget(handent "5EA"))); 5EA mes del analogo
    (setq year_logic (entget(handent "5FE"))); 5FE año del analogo
  
    (setq agl-second (* (float second) (/ pi -30.0))) ; calcula el ángulo del segundero inicial
    (setq agl-minute (+ (* (float minute) (/ pi -30.0)) (/ agl-second 60))) ; calcula el ángulo del minutero inicial
    (setq agl-hour (+ (* (float hour) (/ pi -6.0)) (/ agl-minute 12))) ; calcula el ángulo del horario inicial

    ;algoritmo de zeller para calcular que numero de dia es
    (setq m month)
    (setq y year)
    (if (< m 3)
      (progn
        (setq m (+ m 12))
        (setq y (- y 1))
      )
    )
    (setq K (mod y 100))
    (setq J (fix (/ y 100)))
    (setq h (mod (+ day (fix (/ (* 13 (+ m 1)) 5)) K (fix (/ K 4)) (fix (/ J 4)) (* 5 J)) 7))
    (setq day_of_week (nth h day_name))

    (updateMod)
)



(defun c:initAnalogicClock ()
  ;variables necesarias 
    (setq mints 60)
    (setvar "INTELLIGENTUPDATE" 0)
    (setq time_s (* 60 mints))
    (c:setActualTime)

  
    (while (> time_s 0);buclue principal
      
        (setq agl-second (- agl-second (/ (* pi 2) 60)));calcula el angulo del segundero
        (setq agl-minute (- agl-minute (/ (* pi 2) 3600))); calcula el angulo del minutero
        (setq agl-hour (- agl-hour (/ (* pi 2) 43200))); calcula el angulo del horarrio

        (setq second (mod (+ second 1) 60)) ; incrementa el segundo y le hace modulo 60
        (if (= second 0) ; si el segundo es igual a 0
          (setq minute (mod (+ minute 1) 60))) ; incrementa el minuto y le hace modulo 60
        (if (and (= second 0) (= minute 0)) ; si el segundo y el minuto son igual a 0
          (setq hour (mod (+ hour 1) 12)))
           (if (and (= second 0) (= minute 0)  (= hour 0))
            (setq period (mod (+ period 1) 2)))
         ; cambia el periodo horario
        (if (and (= second 0) (= minute 0) (= hour 0) (= period 0)) ; si el segundo, minuto, hora y periodo son iguales a 0
        (progn
        ;si es febrero
          (setq day (+ day 1))
          (setq h (mod (+ h 1) 7))
          (if (= month 2)
            ;si es multiplo de 4 y diferente del modulo de 100 o es multiplo de 400 
            (if (or (and (= (mod year 4) 0) (/= (mod year 100) 0)) (= (mod year 400) 0));
              ;entonces es bisiesto
              (setq days-in-month 29)
              ;sino 
              (setq days-in-month 28))
            ;si es abril, junio, septiembre o noviembre
            (if (member month '(4 6 9 11))
            ;entonces tiene 30 días
              (setq days-in-month 30)
              ;si no, tiene 31 días
              (setq days-in-month 31)))))
        (if (and (= second 0) (= minute 0) (= hour 0)  (= period 0) (> day days-in-month)) ; si el día supera los días del mes
          (setq day 1))
        (if (and (= second 0) (= minute 0) (= hour 0)  (= period 0) (= day 1)) 
          (setq month (mod (+ month 1) 12))) ; reinicia el mes
        (if (and (= second 0) (= minute 0) (= hour 0)  (= period 0) (= day 1) (= month 0)) ; si es enero
          (setq year (+ 1 year))) ; aumenta el año
        
        (updateMod)
      
        (setq time_s (- time_s 1))
        (command "delay" "1")
        (princ)
    )

    (setvar "INTELLIGENTUPDATE" 20)
)


