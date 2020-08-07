:: tests for datoms
/-  know-sur=know
/+  *test, know
|%
++  e-val
  |=  x=@
  ^-  datom:know-sur
  =+  dat=(new-datom:know)
  dat(e x)
::
++  da  (new-datom:know)
::
++  test-correct-bunt
  %+  expect-eq
    !>  ^-  datom:know-sur  
    d=[e=0 a=%$ v=0 t=0x0 added=%.y tx=0 hash=~zod]
    !>  (new-datom:know)
::
++  test-correct-order-e
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know-sur (dtoi:know da(e 1)) (dtoi:know da(e 2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e 2)) (dtoi:know da(e 1)))
  ==
::
++  test-correct-order-eavt
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know-sur (dtoi:know da(e 0, v 1)) (dtoi:know da(e 0, v 2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e 1, v 1)) (dtoi:know da(e 0, v 2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e 1, v 1)) (dtoi:know da(e 1, v 0)))
  ==
::
++  test-correct-order-avet
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e 0)) (dtoi:know da(v 2, e 0)))
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e 1)) (dtoi:know da(v 2, e 0)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e 1)) (dtoi:know da(v 0, e 1)))
  ==
++  test-search-eavt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %.  %+  turn  (gulf 1 10) 
          |=  x=@
          d(v x)
    sy
  =+  basic-index=(build-indexs:know datoms) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  ~[d(v 6)]
      !>  (search-eavt:know indexer(v `6) basic-index)
  ==

++  test-search-eav-
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e 1, a %test, v 1, tx 1)
       d(e 2, a %test, v 8, tx 3)
       d(e 3, a %test, v 7, tx 4)
       d(e 1, a %test, v 2, tx 2)
       d(e 1, a %test, v 1, tx 2)
       d(e 1, a %test, v 4, tx 9)
     ==
  =/  expected=(list datom:know-sur)
     :~  d(e 1, a %test, v 1, tx 1)
       d(e 1, a %test, v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  expected
      !>  (search-eav-:know indexer(v `1, e `1, a `%test) basic-index)
  ==
++  test-search-e-vt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e 1, a %test, v 1, tx 2)
       d(e 1, a %boop, v 1, tx 1)
       d(e 1, a %bop, v 1, tx 1)
       d(e 1, a %test, v 2, tx 2)
       d(e 2, a %test, v 1, tx 2)
       d(e 2, a %top, v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 1, a %bop, v 1, tx 1)
       d(e 1, a %boop, v 1, tx 1)
      ==
      !>  (search-e-vt:know indexer(e `1, a ~, v `1, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 2, a %top, v 1, tx 2)
       d(e 2, a %test, v 1, tx 2)
      ==
      !>  (search-e-vt:know indexer(e `2, a ~, v `1, tx `2) basic-index)
  ==

++  test-search-ea-t
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e 1, a %test, v 1, tx 1)
       d(e 1, a %boop, v 1, tx 1)
       d(e 1, a %bop, v 1, tx 1)
       d(e 1, a %test, v 2, tx 1)
       d(e 1, a %test, v 3, tx 1)
       d(e 2, a %test, v 1, tx 2)
       d(e 2, a %test, v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 1, a %test, v 1, tx 1)
       d(e 1, a %test, v 2, tx 1)
       d(e 1, a %test, v 3, tx 1)
      ==
      !>  (search-ea-t:know indexer(e `1, a `%test, v ~, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 2, a %test, v 1, tx 2)
      ==
      !>  (search-ea-t:know indexer(e `2, a `%test, v ~, tx `2) basic-index)
  ==

++  test-search-ea--
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e 1, a %test, v 1, tx 1)
       d(e 1, a %boop, v 1, tx 1)
       d(e 1, a %bop, v 1, tx 1)
       d(e 1, a %test, v 2, tx 2)
       d(e 1, a %test, v 3, tx 3)
       d(e 2, a %test, v 1, tx 2)
       d(e 2, a %test, v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 1, a %test, v 1, tx 1)
       d(e 1, a %test, v 2, tx 2)
       d(e 1, a %test, v 3, tx 3)
      ==
      !>  (search-ea--:know indexer(e `1, a `%test, v ~, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e 2, a %test, v 1, tx 2)
      ==
      !>  (search-ea--:know indexer(e `2, a `%test, v ~, tx ~) basic-index)
  ==


--
