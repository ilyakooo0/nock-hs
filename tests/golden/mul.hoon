!=
=>
|%
++  mul
  ~/  %mul
  ::    unsigned multiplication
  ::
  ::  a: multiplicand
  ::  b: multiplier
  |:  [a=`@`1 b=`@`1]
  ::  product
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
++  foo  1
++  baz  |=  x=@  x
++  dec
|=  a=@
=/  b  0
|-  ^-  @
?.  =(a +(b))  $(b +(b))  b
++  bar  2
++  add
  :: ~/  %add
  ::    unsigned addition
  ::
  ::  a: augend
  ::  b: addend
  |=  [a=@ b=@]
  ::  sum
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
-- 
(mul 3 5)
