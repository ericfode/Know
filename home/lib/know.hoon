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
++  build-indexs
  |=  [datoms=(set datom)]
  ^-  indexs
  =/  items=(list item)  (datoms-to-items datoms)
  =/  indexs  *indexs
  %_  indexs
    idx.eavt  (gas:eavt ~ items)
    idx.avet  (gas:avet ~ items)
    idx.aevt  (gas:aevt ~ items)
  ==
::
++  itemtod  |=([=indexer =datom] datom)
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
  =/  sq=(unit indexer)  `q(tx `tx0)
  =/  eq=(unit indexer)  `q(tx `txmax)
  =/  sub=(tree [indexer datom])   (subset:eavt idx.eavt.indexs sq eq)
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
  =/  all-e=(tree [indexer datom])   
    (subset:eavt idx.eavt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
  %-  (traverse:eavt (list datom))
    :*  all-e
        state=~
        |=  [s=(list datom) i=[* d=datom]] 
        ^-  [(unit datom) ? (list datom)]
        ?:  &(=(t tx.d.i) =(v v.d.i))
            [`d.i & [d.i s]]
          [~ & s]
    ==
  -.res
::
++  search-ea-t
  |=  [q=indexer =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)   `q(tx `tx0) 
  =/  eq=(unit indexer)   `q(tx `txmax) 
  =/  all-ea=(tree item)  (subset:eavt idx.eavt.indexs sq eq)
  =/  res=[(list datom) (tree item)]
    %-  (traverse:eavt ,(list datom))
      :*  all-ea 
          state=~
          |=  [s=(list datom) i=[* d=datom]] 
          ^-  [(unit datom) ? (list datom)]
          ?:  =(tx tx.d.i)
            [`d.i & [d.i s]]
          [~ & s]
      ==
  -.res
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
  !!
 
 ::         (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ _ tx
 ::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search-e--t
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))       ;; e _ _ _
++  search-e---
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (if (indexing? db a)                                                   ;; _ a v tx
 ::           (->> (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))      
 ::                (filter (fn [^Datom d] (= tx (datom-tx d)))))
 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                            (= tx (datom-tx d)))))))
++  search--avt
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (if (indexing? db a)                                                   ;; _ a v _
 ::           (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))
 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (= v (.-v d))))))
++  search--av-
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))  ;; _ a _ tx
 ::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search--a-t
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))       ;; _ a _ _
++  search--a--
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                     (= tx (datom-tx d)))) eavt)                ;; _ _ v tx
++  search---vt
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::         (filter (fn [^Datom d] (= v (.-v d))) eavt)                            ;; _ _ v _
++  search---v-
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::      (filter (fn [^Datom d] (= tx (datom-tx d))) eavt)                      ;; _ _ _ tx
++  search----t
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
 ::      eavt])))                                                               ;; _ _ _ _
++  search-----
  |=  [q=[e=indexer] =indexs]
  ^-  (list datom)
  !!
 
:: 

++  search
  |=  [q=indexer =indexs]
  ^-  (list datom)
  ?-  q
    [~ ~ ~ ~]  *(list datom)    
    [~ ~ ~ *]  *(list datom)
    [~ ~ * ~]  *(list datom)
    [~ ~ * *]  *(list datom)
    [~ * ~ ~]  *(list datom)
    [~ * ~ *]  *(list datom)
    [~ * * ~]  *(list datom)
    [~ * * *]  *(list datom)
    [* ~ ~ ~]  *(list datom)
    [* ~ ~ *]  *(list datom)
    [* ~ * ~]  *(list datom)
    [* ~ * *]  *(list datom)
    [* * ~ ~]  (search-ea-- q indexs)
    [* * ~ *]  (search-ea-t q indexs)
    [* * * ~]  (search-eav- q indexs) 
    [* * * *]  (search-eavt q indexs)
  ==
--  

