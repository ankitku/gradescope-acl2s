(definec len3 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len3 (rest x)))))
