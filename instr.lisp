(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))
