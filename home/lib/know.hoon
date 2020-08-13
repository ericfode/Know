/-  *know

|%  
++  e0     ^-  e   --0
++  emax   ^-  e   --999.999.999.999.999  ::Change to max signed
++  tx0    ^-  tx  0
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
  [e=`e.datom a=`a.datom v=`v.datom tx=`tx.datom]
::
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
++  datom-has-attr
  |=  [=a =datom]
  ^-  ?
  =(a.datom a)
::
++  datoms-by-attrs
  |=  [datoms=(set datom) indexed-attrs=(set a)]
  ^-  (set datom)
  %.  %+  skim  ~(tap in datoms)
  |=(d=datom (~(has in indexed-attrs) a.d))
  sy

++  is-indexed
  |=  [=a =indexs]  
  ^-  ?
  (~(has in attrs.avet.indexs) a)
::
++  build-indexs
  |=  [datoms=(set datom) indexed-attrs=(set a)]
  ^-  indexs
  =/  items=(list item)       (datoms-to-items datoms)
  =/  indexed=(set datom)     (datoms-by-attrs datoms indexed-attrs)
  =/  avet-items=(list item)  (datoms-to-items indexed)
  =/  indexs   *indexs
  %_  indexs
    idx.eavt    (gas:eavt ~ items)
    idx.avet    (gas:avet ~ avet-items)
    attrs.avet  indexed-attrs
    idx.aevt    (gas:aevt ~ items)
  ==
::
++  itemtod  |=([=indexer =datom] datom)
::
::
++  search-eavt
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  itm=(unit datom)  (get:eavt idx.eavt.indexs q)
  ?~  itm  ~
  ~[u.itm]

++  search-eav-
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)             `q(tx `tx0)
  =/  eq=(unit indexer)             `q(tx `txmax)
  =/  sub=(tree [indexer datom])    (subset:eavt idx.eavt.indexs sq eq)
  =/  items=(list [indexer datom])  (tap:eavt sub)
  (turn items itemtod)
:: 
::
:: (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ v tx
::              (filter (fn [^Datom d] (and (= v (.-v d))
::                                          (= tx (datom-tx d))))))
++  search-e-vt
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=e.q a=~ v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=e.q a=~ v=~ tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:eavt idx.eavt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
  %-  (traverse:eavt (list datom))          :: Now filter it for matchting vts
    :*  all-e
        state=~
        |=  [s=(list datom) i=[* d=datom]] 
        ^-  [(unit datom) ? (list datom)]
        ?:  &(=((need tx.q) tx.d.i) =((need v.q) v.d.i))
            [`d.i | [d.i s]]
          [`d.i | s]
    ==
  (flop -.res)
::
++  search-ea-t
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)   `q(tx `tx0) 
  =/  eq=(unit indexer)   `q(tx `txmax) 
  =/  all-ea=(tree item)                ::  Filted to the right EA
    (subset:eavt idx.eavt.indexs sq eq) 
  =/  res=[(list datom) (tree item)]
    %-  (traverse:eavt ,(list datom))   ::  now grab all of the v in the tx
      :*  all-ea 
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need tx.q) tx.d.i)
            [`d.i | [d.i s]]
          [`d.i | s]
      ==
  :: (turn (tap:eavt +.res) itemtod)    is this better?
  (flop -.res)
::
++  search-ea--
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `q(v ~, tx `tx0)
  =/  eq=(unit indexer)  `q(v ~, tx `txmax)
  =/  items  (subset:eavt idx.eavt.indexs sq eq)
  (turn (tap:eavt items) itemtod)
::         (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ v _
::              (filter (fn [^Datom d] (= v (.-v d)))))
++  search-e-v-
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=e.q a=~ v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=e.q a=~ v=~ tx=`txmax]
   =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:eavt idx.eavt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
  %-  (traverse:eavt (list datom))          :: Now filter it for matchting ds
    :*  all-e
        state=~
        |=  [s=(list datom) i=[* d=datom]] 
        ^-  [(unit datom) ? (list datom)]
        ?:  =((need v.q) v.d.i)
            [`d.i | [d.i s]]
          [`d.i | s]
    ==
  (flop -.res) 

::         (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ _ tx
::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search-e--t
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=e.q a=~ v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=e.q a=~ v=~ tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:eavt idx.eavt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
  %-  (traverse:eavt (list datom))          :: Now filter it for matchting ds
    :*  all-e
        state=~
        |=  [s=(list datom) i=[* d=datom]] 
        ^-  [(unit datom) ? (list datom)]
        ?:  =((need tx.q) tx.d.i)
            [`d.i | [d.i s]]
          [`d.i | s]
    ==
  (flop -.res) 
::         (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))       ;; e _ _ _
++  search-e---
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=e.q a=~ v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=e.q a=~ v=~ tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:eavt idx.eavt.indexs sq eq)
  (turn (tap:eavt all-e) itemtod)
 
 
 ::           (->> (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))      
 ::                (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search--avt-indexed
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=`e0 a=a.q v=v.q tx=`tx0]
  =/  eq=(unit indexer)  `[e=`emax a=a.q v=v.q tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:avet idx.avet.indexs sq eq)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:avet (list datom))          :: Now filter it for matchting ds
      :*  all-e
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need tx.q) tx.d.i)
              [`d.i | [d.i s]]
            [`d.i | s]
      ==

  (flop -.res) 

 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                            (= tx (datom-tx d)))))))
++  search--avt-simple
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=`e0 a=a.q v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=`emax a=a.q v=~ tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:aevt idx.aevt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:aevt (list datom))          :: Now filter it for matchting ds
      :*  all-e
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  &(=((need v.q) v.d.i) =((need tx.q) tx.d.i))
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res) 


++  search--avt
  |=  [q=indexer =indexs]
  ^-  (list datom)
  ?:  (is-indexed (need a.q) indexs)
    (search--avt-indexed q indexs)
  (search--avt-simple q indexs)
 
 ::         (if (indexing? db a)                                                   ;; _ a v _
 ::           (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))
 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (= v (.-v d))))))
++  search--av-
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=indexer  [e=`e0 a=a.q v=v.q tx=`tx0]
  =/  eq=indexer  [e=`emax a=a.q v=v.q tx=`txmax]
  ?:  (is-indexed (need a.q) indexs)
    =/  items  %.  (subset:avet idx.avet.indexs `sq `eq)  tap:avet
    (turn items itemtod)
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:aevt idx.aevt.indexs `sq(v ~) `eq(v ~))
  =/  res=[(list datom) (tree item)]
    %-  (traverse:aevt (list datom))          :: Now filter it for matchting ds
      :*  all-e
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need v.q) v.d.i) 
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res) 
 
 
 ::         (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))  ;; _ a _ tx
 ::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search--a-t
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[e=`e0 a=a.q v=~ tx=`tx0]
  =/  eq=(unit indexer)  `[e=`emax a=a.q v=~ tx=`txmax]
  =/  all-e=(tree [indexer datom])          :: the  subset of all avts for this e
    (subset:aevt idx.aevt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:aevt (list datom))          :: Now filter it for matchting ds
      :*  all-e
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need tx.q) tx.d.i)
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res) 
 
 ::         (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))       ;; _ a _ _
++  search--a--
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)             `q(e `e0, tx `tx0)
  =/  eq=(unit indexer)             `q(e `emax, tx `txmax)
  =/  sub=(tree [indexer datom])    (subset:aevt idx.aevt.indexs sq eq)
  =/  items=(list [indexer datom])  (tap:aevt sub)
  (turn items itemtod)
 ::         (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                     (= tx (datom-tx d)))) eavt)                ;; _ _ v tx
++  search---vt
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:eavt (list datom))          :: Now filter it for matchting ds
      :*  idx.eavt.indexs
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  &(=((need v.q) v.d.i) =((need tx.q) tx.d.i))
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res)
 
 ::         (filter (fn [^Datom d] (= v (.-v d))) eavt)                            ;; _ _ v _
++  search---v-
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:eavt (list datom))          :: Now filter it for matchting ds
      :*  idx.eavt.indexs
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need v.q) v.d.i)
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res)
 
 ::      (filter (fn [^Datom d] (= tx (datom-tx d))) eavt)                      ;; _ _ _ tx
++  search----t
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:eavt (list datom))          :: Now filter it for matchting ds
      :*  idx.eavt.indexs
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =((need tx.q) tx.d.i)
              [`d.i | [d.i s]]
            [`d.i | s]
      ==
  (flop -.res)
 
 ::      eavt])))                                                               ;; _ _ _ _
++  search-----
  |=  [q=indexer =indexs]
  ^-  (list datom)
  (turn (tap:eavt idx.eavt.indexs) itemtod)
 
:: 

++  search
  |=  [q=indexer =indexs]
  ^-  (list datom)
  ?-  q
  :: e a v t
    [~ ~ ~ ~]  (search----- q indexs)
    [~ ~ ~ *]  (search----t q indexs)
    [~ ~ * ~]  (search---v- q indexs)
    [~ ~ * *]  (search---vt q indexs)
    [~ * ~ ~]  (search--a-- q indexs) 
    [~ * ~ *]  (search--a-t q indexs) 
    [~ * * ~]  (search--av- q indexs) 
    [~ * * *]  (search--avt q indexs)  
    [* ~ ~ ~]  (search-e--- q indexs)  
    [* ~ ~ *]  (search-e--t q indexs)  
    [* ~ * ~]  (search-e-v- q indexs) 
    [* ~ * *]  (search-e-vt q indexs)  
    [* * ~ ~]  (search-ea-- q indexs)  
    [* * ~ *]  (search-ea-t q indexs)
    [* * * ~]  (search-eav- q indexs) 
    [* * * *]  (search-eavt q indexs)
  ==

::  bek /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)  
++  assert-schema-once
  |=  [=db =avase bek=[p=@ta q=@ta d=@ta]]
  ^-  (unit schema-error)
  =/  entry=(unit schema-entry)  (~(get by schema.db) a.avase)
  ?~  entry
    `[%no-entry [avase schema.db a.avase]]
  ?~  mark.u.entry
    ~
  =/  =dais:clay
    .^  =dais:clay
       %cb
       /[p.bek]/[q.bek]/[d.bek]/[u.mark.u.entry]
  ==
  ?.  =(%& -:(mule |.((vale:dais +:avase))))
    `[%nest-fail [avase (need entry)]]   
  ~
  :: TODO assert unique
  :: TODO assert predicate

++  assert-schema
  |=  [=db avs=(list avase) bek=[p=@ta q=@ta d=@ta]]
  ^-  (list schema-error)    :: return a list of errors paired with the invalid datom
  (murn avs |=([=avase] (assert-schema-once db avase bek)))
  
++  update-indexs-add    :: gas takes a list of new values so we'll do it all at once
  |=  [=db datoms=(list datom)]
  ^-  ^db
  =/  datom-set               (sy datoms)
  =/  indexed=(set datom)     (datoms-by-attrs datom-set attrs.avet.indexs.db)
  =/  items                   (datoms-to-items datom-set)
  =/  avet-items=(list item)  (datoms-to-items indexed)
  %_  db
    idx.eavt.indexs    (gas:eavt idx.eavt.indexs.db items)
    idx.avet.indexs    (gas:avet idx.avet.indexs.db avet-items)
    idx.aevt.indexs    (gas:aevt idx.avet.indexs.db items)
  ==



++  av-to-datom
  |=  [=avase tempid=e]
  ^-  datom
  (new-datom tempid a.avase +.vase.avase *t)

++  avs-to-datoms
  |=  avs=(list [a vase])
  ^-  (list datom)
  (turn avs av-to-datom)

++  assign-id
  |=  [datoms=(list datom) id=e]
  ^-  (list datom)
  %+  turn  datoms  |=([d=datom] d(e id))

++  assign-tx-id
  |=  [=db =transaction]
  ^-  [^db ^transaction]
  :: [db(maxtx +(maxtx.db)) transaction(tx (maxtx.db))] 
  !!

++  ses-to-schema
  |=  [ses=(list schema-entry)]
  ^-  schema
  ~&  ses
  =/  ses-items=(list (pair a schema-entry))
    %+  turn  ses
    |=  [se=schema-entry]
    [ident.se se]
  (my ses-items)


++  transact-add
  |=  [d=db t=transaction]
  ^-  transaction-report
  =/  [d=db t=transaction]  (assign-tx-id d t)
  =/  tx-r=transaction-report  *transaction-report
  !!


 ++  transact
  |=  [=db =transaction]
  ^-  transaction-report
  !!

--  

