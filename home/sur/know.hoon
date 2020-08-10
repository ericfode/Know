::
::::  /sur/know/hoon 
  ::

|%
+$  e      @s                    :: Entity id, negitive ids are temp positive are assigned
+$  a      $:([n=term v=term])   :: Namespaced Attr
+$  v      noun                  :: A value
+$  t      @d                    :: Time
+$  added  ?                     :: If it's been added
+$  tx     @                     :: Transaction ID
+$  hash   @p                    :: A hash

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
+$  eavt-index 
  $:  idx=(tree item) 
      cor=_eavt
  ==
+$  avet-index 
  $:  idx=(tree item) 
      cor=_avet
      attrs=(set a)
  ==
+$  aevt-index 
  $:  idx=(tree item) 
      cor=_aevt
  ==
+$  indexs
  $:  eavt=eavt-index
      avet=avet-index
      aevt=aevt-index
  ==
+$  schema-entry
  $:  ident=a
      =mold
      cardinality=?(%one %many)
      doc=cord
      unique=?(%not %value %identity)
      pred=(unit $-(vase ?))
      index=?
      is-component=?
      no-history=?
  ==
+$  avase  
  $:  =a
      =vase
  ==
+$  error-no-entry
  $:  =avase
      =schema
      =a
  ==
+$  error-nest-fail
  $:  =avase
      expected=type
      =schema-entry
  ==
+$  schema-error
  $%  [%no-entry =error-no-entry]
      [%nest-fail =error-nest-fail]
  ==

+$  schema  (map a schema-entry)

::  :db/add, :db/retract, :db.fn/call, :db.fn/retractAttribute, :db.fn/retractEntit 
+$  tx-add  (list (list avase))
+$  tx-data   
  $%  [%add tx-add]
  ==


+$  transaction  
  $:  =tx             :: this will be garbage until after the transaction is run
      =tx-data       :: seems overkill to make it a unit though
      
  ==

+$  transaction-report
  $:  before=db            :: the database before changes
      after=db             :: the database after changes
      datoms=(list datom)  :: The datoms that were built out of the avs passed
      tempids=(map e e)
  ==
      
+$  db
  $:  
    =indexs
    =schema
    maxtx=tx
    maxid=e
    =hash
  ==
--