/-  *know

|%  
++  tx0  ^-  tx  0
++  txmax  ^-  tx  0xffff.ffff.ffff.ffff
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

++  filter-index
  |=  [=index =filter]
  ^-  index
  =/  om  cor.idx
  index

++  search-eav-
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (set datom)
  (subset:eavt eavt.indexs q(tx tx0) q(tx txmax))
 
++  search-ea-t
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (set datom)
  =.  all-e-a  
    (subset:eavt eavt.indexs q(tx tx0) q(tx txmax))
  %-  (traverse:eavt ,(set datom))
    :*  all-e-a 
        state=~
        |=  [s=(list datom) k=indexer v=datom] 
        ?:  ?=((need tx) tx.d)
        [datom s]
        s
    ==
++  search-ea--
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (set datom)
  (subset:eavt eavt.indexs q(tx tx0) q(tx txmax))




++  search
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (set datom)
  ?-  q
    [* * * *]  (slice-index eavt.indexs q q)
    [* * * ~]  (search-eav- q indexs) 
    [* * ~ *]  (search-ea-t q)
    [* * ~ ~]  (search-ea-- )
  ::  [* ~ * *]
  ::  [* ~ * ~]
  ::  [* ~ ~ *]
  ::  [* ~ ~ ~]
  ::  [~ * * *]
  ::  [~ * * ~]
  ::  [~ * ~ *]
  ::  [~ * ~ ~]
  ::  [~ ~ * *]
  ::  [~ ~ * ~]
  ::  [~ ~ ~ *]
  ::  [~ ~ ~ ~]
  ==
  
--  

