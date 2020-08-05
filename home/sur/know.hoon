::
::::  /sur/know/hoon 
  ::

|%
+$  e      @             :: Entity id
+$  a      term          :: Attribute a @tas / like %attr
+$  v      noun          :: A value
+$  t      @d            :: Time
+$  added  ?             :: If it's been added
+$  tx     @             :: Transaction ID
+$  hash   @p            :: A hash

+$  datom
  $:  =e
      =a
      =v
      =t
      =added
      =tx
      =hash
  ==
+$  indexer-unit
  $:  e=(unit e)
      a=(unit a)
      v=(unit v)
      tx=(unit tx)
  ==
+$  filter  
  $~  |=([d=datom] &)
  $-(datom ?)
+$  indexer
  $:  =e
      =a
      =v
      =tx
  ==
--
|%
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
++  eavt  ((ordered-map indexer datom) cmp-eavt)
++  avet  ((ordered-map indexer datom) cmp-aevt)
++  aevt  ((ordered-map indexer datom) cmp-avet)
--
|%
+$  item  (mk-item indexer datom)
+$  index  
  $%  [%eavt idx=(tree item) cor=_eavt]
      [%avet idx=(tree item) cor=_avet]
      [%aevt idx=(tree item) cor=_aevt]
  ==
+$  indexs
  $:  eavt=index 
      avet=index
      aevt=index
  ==
--