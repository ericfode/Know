/-  *know

|%  
++  tx0  ^-  tx  0
++  txmax ^-  tx  0xffff.ffff.ffff.ffff
++  new-datom
  |=  [=e =a =v =tx]
  ^-  datom
  =/  data  *datom
  %_  data
    e  e
    a  a 
    v  v 
    tx  tx
  ==
::
++  hash-datom
  |=  [=e =a =v]
  ^-  @p
  (mug [e a v])
::
++  ncmp
  |*  [a=* b=*]
  ?~  a  &
  ?~  b  &
  =(a b)
++  cmp-datom
  |*  [[a1=* a2=* a3=* a4=*] [b1=* b2=* b3=* b4=*]]
  ::  TODO assert the types of each pair are the same 
  ^-  ?
  ?:  (ncmp a1 b1)
    ?:  (ncmp a2 b2)
      ?:  (ncmp a3 b3)
        ?:  (ncmp a4 b4)
          &
        (dor a4 b4)
      (dor a3 b3)
    (dor a2 b2)
  (dor a1 b1)
::
++  cmp-eavt
  |=  [a=indexer b=indexer]
  ^-  ?
  (cmp-datom [e.a a.a v.a tx.a] [e.b a.b v.b tx.b])
::
++  cmp-aevt
  |=  [a=indexer b=indexer]
  ^-  ?
  (cmp-datom [a.a e.a v.a tx.a] [a.b e.b v.b tx.b])
::
++  cmp-avet
  |=  [a=indexer b=indexer]
  ^-  ?
  (cmp-datom [a.a v.a e.a tx.a] [a.b v.b e.b tx.b])
::
++  dtoi
  |=  =datom
  ^-  indexer
  [e=e.datom a=a.datom v=v.datom tx=tx.datom]
::
::  These are doors with functions to work on indexs
::  That are ordered each of these ways
::  Look in zuse.hoon for the source. There are no docs
::  But the comments are epic
::
++  eavt  ((ordered-map ,indexer ,datom) cmp-eavt)
++  avet  ((ordered-map ,indexer ,datom) cmp-aevt)
++  aevt  ((ordered-map ,indexer ,datom) cmp-avet)
::
++  datoms-to-items
  |=  [datoms=(set datom)]
  ^-  (list item)
  =/  dlist  ~(tap in datoms)
  %+  turn  dlist 
  |=  [=datom]
  ^-  item
  (item (dtoi datom) datom) 
::
++  build-indexs
  |=  [datoms=(set datom)]
  ^-  indexs
  =/  items  (datoms-to-items datoms)
  =/  indexs  *indexs
  %_  indexs
    eavt  [%eavt idx=(gas:eavt *index items)]
    avet  [%avet idx=(gas:avet *index items)]
    aevt  [%aevt idx=(gas:aevt *index items)]
  ==
--
++  slice-index
  |=  [=index start=indexer-unit stop=indexer-unit]
  ^-  index
  *(tree datom)

++  filter-index
  |=  [=index =filter]
  ^-  index
  =/  om  cor.idx
  (:om)

++  search
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (set datom)
  ?-  q
    [* * * *]  (slice-index eavt.indexs q q)
    [* * * ~]  (slice-index eavt.indexs q(tx tx0) q(tx txmax))
    [* * ~ *]  =. all-e-a  (slice-index eavt.indexs q(tx tx0) q(tx txmax))
               (filter-index all-e-a |=(d=datom  -^  ?  ?:  ?=(tx.d (got tx))))
    [* * ~ ~]  (slice-index eavt.indexs q(tx tx0) q(tx txmax))
    [* ~ * *]
    [* ~ * ~]
    [* ~ ~ *]
    [* ~ ~ ~]
    [~ * * *]
    [~ * * ~]
    [~ * ~ *]
    [~ * ~ ~]
    [~ ~ * *]
    [~ ~ * ~]
    [~ ~ ~ *]
    [~ ~ ~ ~]
  ==
  
  

