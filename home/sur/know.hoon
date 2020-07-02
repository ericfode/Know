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
  $~  |=(=datom %y)
  $-(datom ?)
+$  indexer
  $:  =e
      =a
      =v
      =tx
  ==
+$  item  (mk-item indexer datom)
+$  index  
  $%  [%eavt idx=(tree item) cor=_((ordered-map ,indexer ,datom) cmp-avet)]
      [%avet idx=(tree item) cor=_((ordered-map ,indexer ,datom) cmp-aevt)]
      [%aevt idx=(tree item) cor=_((ordered-map ,indexer ,datom) cmp-avet)]
  ==
==

+$  indexs
  $:  eavt=index 
      avet=index
      aevt=index
  ==
--