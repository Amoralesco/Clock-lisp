(defun c:initClock ()
  (setq date (getvar "CDATE"))
  (setq second_hand (entget(handent "46E"))); segundero
  (setq minute_hand (entget(handent "47F")));47F minutero
  (setq hour_hand (entget(handent "489")));489 horarrio
  (setvar "INTELLIGENTUPDATE" 0)
  (setq agl 0)
  (setq time_s 240)


  (while (> time_s 0)
    (setq agl (- agl (/ (* pi 2) 240)))
    (setq cambio-angulo (subst (cons 50 agl) (assoc 50 second_hand)second_hand))
    (entmod cambio-angulo)
    (setq time_s (- time_s 1))
    (command "delay" "250")
  )

  (setvar "INTELLIGENTUPDATE" 20)
)

