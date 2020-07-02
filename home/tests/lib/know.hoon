:: tests for datoms
/-  know-sur=know
/+  *test, know
|%
++  e-val
  |=  x=@
  ^-  datom:know-sur
  =+  dat=(new-datom:know)
  dat(e x)
++  da  (new-datom:know)
++  test-correct-bunt
  %+  expect-eq
    !>  ^-  datom:know-sur  
    d=[e=0 a=%$ v=0 t=0x0 added=%.y tx=0 hash=~zod]
    !>  (new-datom:know)
++  test-correct-order-e
  =+  da=(dtoi:know (new-datom:know))
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know da(e 1) da(e 2))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know da(e 2) da(e 1))
  ==


++  test-correct-order-eavt
  =+  da=(dtoi:know (new-datom:know))
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-eavt:know da(e 0, v 1) da(e 0, v 2))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know da(e 1, v 1) da(e 0, v 2))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-eavt:know da(e 1, v 1) da(e 1, v 0))
  ==

++  test-correct-order-avet
  =+  da=(dtoi:know (new-datom:know))
  ;:  weld
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know da(v 1, e 0) da(v 2, e 0))
    %+  expect-eq
      !>  ^-  flag  %.y
      !>  (cmp-avet:know da(v 1, e 1) da(v 2, e 0))
    %+  expect-eq
      !>  ^-  flag  %.n
      !>  (cmp-avet:know da(v 1, e 1) da(v 0, e 1))
  ==
--