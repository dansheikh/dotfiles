(setf swm-gaps:*head-gaps-size* 0
      swm-gaps:*outer-gaps-size* 10
      swm-gaps:*inner-gaps-size* 5)

(when *initializing*
  (swm-gaps:toggle-gaps))
