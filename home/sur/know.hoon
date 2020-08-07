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
+$  indexer
  $:  e=(unit e)
      a=(unit a)
      v=(unit v)
      tx=(unit tx)
  ==
+$  filter  
  $~  |=([d=datom] &)
  $-(datom ?)
--
|%
::  If either data is null return true
::  we want this so that we can null out values in indexers to
::  select slices of data.
++  ncmp
  |*  [a=(unit *) b=(unit *)]
  ?~  a  &
  ?~  b  &
  =(u.a u.b)

++  cmp-datom
  |*  $:  
      [a1=(unit *) a2=(unit *) a3=(unit *) a4=(unit *)] 
      [b1=(unit *) b2=(unit *) b3=(unit *) b4=(unit *)]
      ==
  ::  TODO assert the types of each pair are the same 
  ^-  ?
  ?:  (ncmp a1 b1)
    ?:  (ncmp a2 b2)
      ?:  (ncmp a3 b3)
        ?:  (ncmp a4 b4)
          &
        (dor (need a4) (need b4))
      (dor (need a3) (need b3))
    (dor (need a2) (need b2))
  (dor (need a1) (need b1))
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