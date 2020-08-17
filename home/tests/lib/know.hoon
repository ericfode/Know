:: tests for datoms
/-  know-sur=know
/+  *test, know, *strandio
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
    d=[e=0 a=[%$ %$] v=0 t=0x0 added=%.y tx=0 hash=~zod]
    !>  (new-datom:know)
::
++  test-correct-order-e
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know-sur (dtoi:know da(e -1)) (dtoi:know da(e -2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e -2)) (dtoi:know da(e -1)))
  ==
::
++  test-correct-order-eavt
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know-sur (dtoi:know da(e --0, v 1)) (dtoi:know da(e --0, v 2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e -1, v 1)) (dtoi:know da(e --0, v 2)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know-sur (dtoi:know da(e -1, v 1)) (dtoi:know da(e -1, v 0)))
  ==
::
++  test-correct-order-avet
  =+  da=(new-datom:know)
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e --0)) (dtoi:know da(v 2, e --0)))
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e -1)) (dtoi:know da(v 2, e --0)))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-avet:know-sur (dtoi:know da(v 1, e -1)) (dtoi:know da(v 0, e -1)))
  ==
++  test-search-eavt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %.  %+  turn  (gulf 1 10) 
          |=  x=@
          d(v x)
    sy
  =+  basic-index=(build-indexs:know datoms ~) 
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
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -2, a [%test %test], v 8, tx 3)
       d(e -3, a [%test %test], v 7, tx 4)
       d(e -1, a [%test %test], v 2, tx 2)
       d(e -1, a [%test %test], v 1, tx 2)
       d(e -1, a [%test %test], v 4, tx 9)
     ==
  =/  expected=(list datom:know-sur)
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%test %test], v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  expected
      !>  (search-eav-:know indexer(v `1, e `-1, a `[%test %test]) basic-index)
  ==
++  test-search-e-vt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e --1, a [%test %test], v 1, tx 2)
       d(e --1, a [%boop %boop], v 1, tx 1)
       d(e --1, a [%bop %bop], v 1, tx 1)
       d(e --1, a [%test %test], v 2, tx 2)
       d(e --2, a [%test %test], v 1, tx 2)
       d(e --2, a [%top %top], v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e --1, a [%bop %bop], v 1, tx 1)
       d(e --1, a [%boop %boop], v 1, tx 1)
      ==
      !>  (search-e-vt:know indexer(e `--1, a ~, v `1, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e --2, a [%top %top], v 1, tx 2)
       d(e --2, a [%test %test], v 1, tx 2)
      ==
      !>  (search-e-vt:know indexer(e `--2, a ~, v `1, tx `2) basic-index)
  ==

++  test-search-ea-t
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%boop %boop], v 1, tx 1)
       d(e -1, a [%bop %bop], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 1)
       d(e -1, a [%test %test], v 3, tx 1)
       d(e -2, a [%test %test], v 1, tx 2)
       d(e -2, a [%test %test], v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 1)
       d(e -1, a [%test %test], v 3, tx 1)
      ==
      !>  (search-ea-t:know indexer(e `-1, a `[%test %test], v ~, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e -2, a [%test %test], v 1, tx 2)
      ==
      !>  (search-ea-t:know indexer(e `-2, a `[%test %test], v ~, tx `2) basic-index)
  ==

++  test-search-ea--
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%boop %boop], v 1, tx 1)
       d(e -1, a [%bop %bop], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 2)
       d(e -1, a [%test %test], v 3, tx 3)
       d(e -2, a [%test %test], v 1, tx 2)
       d(e -2, a [%test %test], v 1, tx 2)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 2)
       d(e -1, a [%test %test], v 3, tx 3)
      ==
      !>  (search-ea--:know indexer(e `-1, a `[%test %test], v ~, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
       d(e -2, a [%test %test], v 1, tx 2)
      ==
      !>  (search-ea--:know indexer(e `-2, a `[%test %test], v ~, tx ~) basic-index)
  ==

++  test-search-e-v-
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%boop %boop], v 1, tx 1)
       d(e -1, a [%bop %bop], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 2)
       d(e -1, a [%test %test], v 3, tx 3)
       d(e -2, a [%test %test], v 1, tx 2)
       d(e -2, a [%test %test], v 1, tx 3)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 1)
      ==
      !>  (search:know indexer(e `-1, a ~, v `1, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -2, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 3)
      ==
      !>  (search:know indexer(e `-2, a ~, v `1, tx ~) basic-index)
  ==

++  test-search-e--t
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
     %-  sy  
     :~  d(e -1, a [%test %test], v 1, tx 1)
       d(e -1, a [%boop %boop], v 1, tx 1)
       d(e -1, a [%bop %bop], v 1, tx 1)
       d(e -1, a [%test %test], v 2, tx 2)
       d(e -1, a [%test %test], v 3, tx 3)
       d(e -2, a [%test %test], v 1, tx 2)
       d(e -2, a [%test %test], v 1, tx 3)
     ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 1)
      ==
      !>  (search:know indexer(e `-1, a ~, v ~, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 2, tx 2)
      ==
      !>  (search:know indexer(e `-1, a ~, v ~, tx `2) basic-index)
  ==

++  test-search-e---
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 2, tx 2)
      d(e -1, a [%test %test], v 3, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms ~) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 1)
        d(e -1, a [%test %test], v 2, tx 2)
        d(e -1, a [%test %test], v 3, tx 3)
      ==
      !>  (search:know indexer(e `-1, a ~, v ~, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -2, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 3)
      ==
      !>  (search:know indexer(e `-2, a ~, v ~, tx ~) basic-index)
  ==

++  test-search--avt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 1, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a `[%test %test], v `1, tx `2) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 1)
      ==
      !>  (search:know indexer(e ~, a `[%boop %boop], v `1, tx `1) basic-index)
  ==


++  test-search--av-
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 1, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -1, a [%test %test], v 1, tx 3)
        d(e -2, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 3)
      ==
      !>  (search:know indexer(e ~, a `[%test %test], v `1, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a `[%boop %boop], v `1, tx ~) basic-index)
  ==

++  test-search--a-t
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 1, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a `[%test %test], v ~, tx `2) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 1)
      ==
      !>  (search:know indexer(e ~, a `[%boop %boop], v ~, tx `1) basic-index)
  ==

++  test-search--a--
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 1, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -1, a [%test %test], v 1, tx 3)
        d(e -2, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 3)
      ==
      !>  (search:know indexer(e ~, a `[%test %test], v ~, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a `[%boop %boop], v ~, tx ~) basic-index)
  ==

++  test-search---vt
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 1, tx 3)
      d(e -2, a [%test %test], v 1, tx 2)
      d(e -2, a [%test %test], v 1, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 1)
      ==
      !>  (search:know indexer(e ~, a ~, v `1, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 2)
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a ~, v `1, tx `2) basic-index)
  ==

++  test-search---v-
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 2, tx 3)
      d(e -2, a [%test %test], v 2, tx 2)
      d(e -2, a [%test %test], v 2, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 2)
        d(e -1, a [%test %test], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a ~, v `1, tx ~) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%test %test], v 2, tx 3)
        d(e -2, a [%test %test], v 2, tx 2)
        d(e -2, a [%test %test], v 2, tx 3)
      ==
      !>  (search:know indexer(e ~, a ~, v `2, tx ~) basic-index)
  ==

++  test-search----t
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  d(e -1, a [%test %test], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
      d(e -1, a [%bop %bop], v 1, tx 1)
      d(e -1, a [%test %test], v 1, tx 2)
      d(e -1, a [%test %test], v 2, tx 3)
      d(e -2, a [%test %test], v 2, tx 2)
      d(e -2, a [%test %test], v 2, tx 3)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%bop %bop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%test %test], v 1, tx 1)
      ==
      !>  (search:know indexer(e ~, a ~, v ~, tx `1) basic-index)
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 2)
        d(e -1, a [%test %test], v 1, tx 2)
        d(e -2, a [%test %test], v 2, tx 2)
      ==
      !>  (search:know indexer(e ~, a ~, v ~, tx `2) basic-index)
  ==

++  test-search-----
  =+  indexer=(dtoi:know (new-datom:know))
  =+  d=(new-datom:know)
  =/  datoms=(set datom:know-sur)
    %-  sy  
    :~  
      d(e -1, a [%boop %boop], v 1, tx 1)
      d(e -1, a [%boop %boop], v 1, tx 2)
    ==
  =+  basic-index=(build-indexs:know datoms (sy `(list a:know-sur)`[[%test %test] ~])) 
  ;:  weld
    %+  expect-eq
      !>  ^-  (list datom:know-sur)  :~ 
        d(e -1, a [%boop %boop], v 1, tx 1)
        d(e -1, a [%boop %boop], v 1, tx 2)
      ==
      !>  (search:know indexer(e ~, a ~, v ~, tx ~) basic-index)
  ==

++  test-assert-schema-once
  =/  se  *schema-entry:know-sur
  =/  se1  se(ident boop/%boop, mark ~, doc 'test')
  =/  sch  (ses-to-schema:know ~[se1])
  =/  d   *db:know-sur
  =/  d   d(schema sch)
  =+  dt=(new-datom:know)
  =/  dt  dt(e -1, a [%boop %boop], v 1, tx 1)
  =/  byks=[p=@ta q=@ta d=@ta ~]  [~.~zod ~.home ~.~2020.8.12..19.44.34..4188 ~]
  =/  byk=[p=@ta q=@ta d=@ta]  [p.byks q.byks d.byks]
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (assert-schema-once:know d dt byk)
     
  ==

++  test-collect-and-replace-tempids
  =/  se  *schema-entry:know-sur
  =/  se1  se(ident boop/%boop, mark ~, doc 'test')
  =/  sch  (ses-to-schema:know ~[se1])
  =/  db   *db:know-sur
  =/  db   db(schema sch)
  =+  d=(new-datom:know)
  =/  datoms=(list datom:know-sur)
  :~
    d(e -1, a [%boop %boop], v 2, tx 0)
    d(e -2, a [%boop %boop], v 2, tx 0)
    d(e -2, a [%boop %boop], v 3, tx 0)
    d(e -3, a [%boop %boop], v 1, tx 0)
    d(e -4, a [%boop %boop], v 1, tx 0)
    d(e -1, a [%boop %boop], v 1, tx 0)
    d(e -1, a [%boop %boop], v 1, tx 0)
    d(e -1, a [%boop %boop], v 1, tx 0)
  ==
  =/  expected-datoms=(list datom:know-sur)
  :~
    d(e --1, a [%boop %boop], v 2, tx 0)
    d(e --2, a [%boop %boop], v 2, tx 0)
    d(e --2, a [%boop %boop], v 3, tx 0)
    d(e --3, a [%boop %boop], v 1, tx 0)
    d(e --4, a [%boop %boop], v 1, tx 0)
    d(e --1, a [%boop %boop], v 1, tx 0)
    d(e --1, a [%boop %boop], v 1, tx 0)
    d(e --1, a [%boop %boop], v 1, tx 0)
  ==
  =/  result=[res-db=db:know-sur res=(list datom:know-sur) =tempids:know-sur]   
    (collect-and-replace-temp-ids:know db datoms *tempids:know-sur)
    
  ;:  weld
    %+  expect-eq
      !>  expected-datoms
      !>  res.result
    %+  expect-eq
      !>  ^-  tempids:know-sur  (my ~[[-1 --1] [-2 --2] [-3 --3] [-4 --4]])
      !>  tempids.result
    %+  expect-eq
      !>  --4
      !>  maxid.res-db.result
  ==


++  test-transact-add
  =/  se  *schema-entry:know-sur
  =/  se1  se(ident boop/%boop, mark ~, doc 'test')
  =/  se2  se(ident boop/%bop, mark ~, doc 'test')
  =/  se3  se(ident boop/%top, mark ~, doc 'test')
  =/  sch  (ses-to-schema:know ~[se1 se2 se3])
  =/  db   *db:know-sur
  =/  db   db(schema sch)
  =+  d=(new-datom:know)
  =/  datoms=(list (list datom:know-sur))
  :~ 
    :~
      d(a boop/%boop, v 1)
      d(a boop/%bop, v 1)
    ==
    :~
      d(a boop/%bop, v 2)
      d(a boop/%boop, v 2)
    ==
  == 
  =/  byks=[p=@ta q=@ta d=@ta ~]  [~.~zod ~.home ~.~2020.8.12..19.44.34..4188 ~]
  =/  byk=[p=@ta q=@ta d=@ta]  [p.byks q.byks d.byks]
  =/  transaction=transaction:know-sur  
    [tx-data=[%add datoms] tx=0]
  =/  expected-datoms=(list datom:know-sur)
  :~
    d(e --1, a [%boop %boop], v 1, tx 1)
    d(e --1, a [%boop %bop], v 1, tx 1)
    d(e --2, a [%boop %bop], v 2, tx 1)
    d(e --2, a [%boop %boop], v 2, tx 1)
  ==
  =/  result=(each [db:know-sur transaction-report:know-sur] schema-errors:know-sur)
    (transact:know db transaction `@uv`0 byk)
  ?>  ?=(%& -.result)
  =/  [=db:know-sur txr=transaction-report:know-sur]  p.result
  ;:  weld
    %+  expect-eq
      !>  %.y 
      !>  -:result
    %+  expect-eq
      !>  (zing datoms)
      !>  before.txr
    %+  expect-eq
      !>  expected-datoms
      !>  after.txr
    %+  expect-eq
     !>  (my [-594.882.946.332.110 --2] [-1.586.380.539.752.686 --1] ~)
     !>  tempids.txr

  ==


--
