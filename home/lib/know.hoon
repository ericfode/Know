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
  |=  [=db =datom bek=[p=@ta q=@ta d=@ta]]
  ^-  (unit schema-error)
  =/  entry=(unit schema-entry)  (~(get by schema.db) a.datom)
  ?~  entry
    `[%no-entry [datom schema.db a.datom]]
  ?~  mark.u.entry
    ~
  =/  =dais:clay
    .^  =dais:clay
       %cb
       /[p.bek]/[q.bek]/[d.bek]/[u.mark.u.entry]
  ==
  ?.  =(%& -:(mule |.((vale:dais v.datom))))
    `[%nest-fail [datom (need entry)]]   
  ~
  :: TODO assert unique
  :: TODO assert predicate

++  assert-schema
  |=  [=db avs=(list datom) bek=[p=@ta q=@ta d=@ta]]
  ^-  (list schema-error)    :: return a list of errors paired with the invalid datom
  (murn avs |=([=datom] (assert-schema-once db datom bek)))
  
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

++  assign-txids
  |=  [datoms=tx-add =tx]
  ^-  tx-add
  %+  turn  datoms
    |=  [inner-datoms=(list datom)]
    ^-  (list datom)
    %+  turn  inner-datoms
      |=([d=datom] ^-(datom d(tx tx)))



++  assign-tx-id
  |=  [=db =transaction]
  ^-  [^db ^transaction]
  [db(maxtx +(maxtx.db)) transaction(tx +(maxtx.db))] 
  

++  collect-and-replace-temp-ids
  |=  [=db datoms=(list datom) =tempids]
  ^-  [db=^db datoms=(list datom) ^tempids]
  =/  [datoms-with-ids=(list datom) s=[db=^db tempids=^tempids]]
    %^  spin  datoms  [db=db tempids=tempids]
    |=  [d=datom s=[db=^db tempids=^tempids]]
    ?:  (syn:si e.d)                                       :: is it a tempid
      [d s]                                                :: if it's not just return
    =/  assigned-id  (~(get by tempids.s) e.d)             :: if it is try to look it up
    ?.  ?=($~ assigned-id)                                 :: has it already been replaced
      [d(e u.assigned-id) s]                               :: just update the datom
    =/  new-id  (sum:si maxid.db.s --1)                    :: otherwise get a new one
    =/  new-tempids  (~(put by tempids.s) e.d new-id)
    =/  new-db  db.s(maxid new-id)
    =/  new-datom  d(e new-id)
    [new-datom s=[db=new-db tempids=new-tempids]]
  [db.s datoms-with-ids tempids.s]
      

++  maybe-assign-ids
  |=  [datoms=(list datom) id=e]
  ^-  (list datom)
  %+  turn  datoms  
    |=  [d=datom] 
    ^-  datom 
    ?:  =(--0 e.d)
      d(e id)
    d

++  add-temp-ids
  |=  [=tx-add eny=@uv]
  ^-  ^tx-add
  =/  [updated-tx=^tx-add *]
  =/  rng  ~(. og eny)
  %^  spin  tx-add  rng
    |=  [datoms=(list datom) rng=_rng]
    =^  new-num  rng  (rads:rng emax)
    =/  =e  (new:si | new-num)
    [(maybe-assign-ids datoms e) rng]
  updated-tx

++  update-tx-add-ids
  |=  [=db =tx-add]                           :: Takes the current state of the db and a tx
  ^-  [=^db =tempids =^tx-add]                :: Returns a new db, with the maxtx updated
  =/  [tx-with-ids=^tx-add s=[=^db =tempids]]  :: as well as the updated tx
    %^  spin  tx-add  [db *tempids]           :: To construct this we spin over the tx
    |=  [ds=(list datom) s=[=^db =tempids]]   :: Taking the db and the current list of tempids
    ^-  [(list datom) _s]                     :: Each time return the current list of datoms and new state
    =/  [db=^db datoms=(list datom) =tempids] :: we construct that by collection any tempids
      (collect-and-replace-temp-ids db.s ds tempids.s)    :: that are already in the datoms and replacing them with realids
    [datoms s=[db tempids]]                   :: then returning those datoms and updated db
  [db.s tempids.s tx-with-ids]                :: then return the whole thing


++  ses-to-schema
  |=  [ses=(list schema-entry)]
  ^-  schema
  =/  ses-items=(list (pair a schema-entry))
    %+  turn  ses
    |=  [se=schema-entry]
    [ident.se se]
  (my ses-items)


++  transact-add
  |=  [d=db t=transaction eny=@uv bek=[p=@ta q=@ta d=@ta]]
  ^-  (each [db transaction-report] schema-errors)
  =/  [d=db t=transaction]        (assign-tx-id d t)
  =/  =tx-add                     (assign-txids +:tx-data.t tx.t)
  =.  tx-add                      (add-temp-ids tx-add eny)
  =/  [d=db =tempids =^tx-add]    (update-tx-add-ids d tx-add)
  =/  datoms-before=(list datom)  (zing +:tx-data.t)
  =/  datoms-after=(list datom)   (zing tx-add)
  =/  schema-errors               (assert-schema d datoms-after bek)
  ?:  =($~ schema-errors)  |+schema-errors
  =/  tx-r=transaction-report  *transaction-report
  :+  %.y  (update-indexs-add d datoms-after)
    %_  tx-r
      before   datoms-before
      after    datoms-after
      tempids  tempids  
    ==


 ++  transact
  |=  [=db =transaction eny=@uv bek=[p=@ta q=@ta d=@ta]]
  ^-  (each [^db transaction-report] schema-errors)
  ?-  tx-data.transaction
    [%add *]  (transact-add db transaction eny bek)
  ==

:::  EVEYTHING FROM HERE DOWN IS DRAFT
:::  I don't really know what i'm doing so i'm trying explaing
:::  various solutions.


:: q
::  {find ~[%star-date @da],
::   where [[%bin /pastebin-entry/author "alice"]
::          [%bin /starred/date %star-date] ~]}
::   we want to build something that takes that ^
::   and turns it into a function that can be applied
::   to a dataset that returns the values needed.
::   that function should be partially applicable
::   so that parts of it can be execution in multiple
::   places.

::   We are going to start with
::   find this is a list of pairs of %symbol and types.
::   It's possible that we might just want to do symbols,
::   or use the marks in the schema associated with the key
::   ... We'll start with that.
::   It should therefor have a type like
++   finder  $-(find-clause find-combinator)


::   where
::   This is a list of combinators that will get pipelined with
::   the ;: com    
::   the output of all of this needs to end up as a noun
::   that noun will be pairs of [key value]
::   keys that have multiple values will be turned into a list
|=  [a=filter b=$-(clause filter)]  `filter`?~(a ~ (b a))
;:  query
  (find ~[%star-date @da])             :: check all ofd the capture maps for %star-date
                                       :: and and the results to the output map 
                                       :: adds a symbol to the output map 
  (with ~[%bin])                       :: adds a symbol to the intermidates map
  ;;  and                              :: takes datoms sets and unions thems                     
    (where (simp [%bin /pasebin-entry/author "alice"]))   :: checks intermidates map for
    :: %bin, if it's present the (query [`bin-value `/pastebin-entry/author `"alice" ~])
    :: accumulate the result of applying doing that with each %bin.
    :: this is the new working set. the top level function should turn this into a db
    :: and pass it to the next funtion

    :: if %bin is not present then do (query [~ `/pastebin-entry/author `"alice" ~])
    :: save all of the resulting e values in the e-caputres (map @tas e)
    :: don't save any a there was a litteral a, rather then an @tas
    :: don't save any v  there was a litteral v, rather then a @tas
    :: don't save any t  there was a nil (or rather it was elided and we normalized to have a nil)

    (where (simp [%bin /pastebin-entry/date %star-date]))  
    ::  check the working e-capatures for %bin
    ::  if they are present use them in a query in a moment
    ::  use the literal a no @tas was mentioned
    ::  
    ::
    ::  really the result of this is a set of query tuples (or gates that return query tuples given a db) 
    ::  and captures what to caputure from each from each of the
    ::  but these query tuples are ordered. or rather they results are composed.

    ::  we get something like this
    +$  state
      $:  query-actions
      working-set
      captures
      ==
    
    ++  caputure-e
    |=  [query] 
    ^-  context
    ?:  ?=(@tas e.query)
      |=  [context]
      ^-  context 
      =/  e-cap  (unit (list e))  (~(get by e.captures) e.query)
      ?~  e-cap
        (query working-set [~ a.query ~ ~])

    ++  e-is-lit
    ++  a-is-lit
    ++  v-is-lit
    ++  t-is-lit

    ++  context-has-e
    ++  context-has-a
    ++  context-has-v
    ++  context-has-t

+$  to-capture
  $:  e=@tas
      a=@tas
      v=@tas
      t=@tas
  ==

+$  found  (map @tas *)
  


+$  pending-queries  (list indexer)

 +$  context
      $:  pending-queries
      working-db
      to-capture
      found
      ==
++  reduce-db
  |=  [=context]
  ^-  (list datom)
  =/  res=(list (list datom))
    %+  roll  pending-queries.context
    |=  [=indexer]
    (query working-db.context indexer)
  (zing res)


++  where
  |=  [query-spec]
  |=  [context]
  ^-  context
  =^  context  (maybe-update-with-es query-spec context)
  =^  context  (maybe-update-with-as query-spec context)
  =^  context  (maybe-update-with-vs query-spec context)
  =^  context  (maybe-update-with-ts query-spec context)
  =/  results  (reduce-db context)
  =/  context  (resolve-captures context results)
  [results context]

    

  
    ++  e-queries
    |=  [context]



    (where (simp [%bin /pasebin-entry/author "alice"]))

    (where (simp [%bin /pastebin-entry/date %star-date]))  
    :: needs to turn into a query, one for each %bin found in the last query
    [~ a.query ~ ~]
    :: Then the result of that query needs to have the as and the vs captured

    :: have a combining function that checks to see if there has to be more querying
    :: done and does it if it has to.

    ++  maybe-reduce
    |=  [query (list query-tuples) state db]
    :: First it will check to see if there is anything left in the query
    ?~  query
      :: Then we simply run all our queries 
      =/  res=(set datom)  (reduce query-tuples db)
      :: 
      ::  execute all of the captures
      ::  in the state
      =/  state-with-caputures  (apply-caputures state res)
      ::  then return 

    ::  then it will check to see if there are any more unsatisfied
  

    ::  The goal is to reduce the query to identity eventually



    (query query-tuypesl)
    (this-func query-tuples state)
    ::  query tuples

++  result-function
  
$-(db result)
+$  result  
  $:  datoms=(list datom)
      bound-symbols=(map @tas *)
      
::   where [[%bin /pastebin-entry/author "alice"]
::          [%bin /starred/date %star-date] ~]}
)
--  

::  |=([a=tape b=$-(char tape)] `tape`?~(a ~ (weld (b i.a) t.a)))