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
++  itod  |=([=indexer =datom] datom)
::

++  search-eavt
  |=  [q=[=e =a =v =t] =indexs]
  ^-  (list datom)
  =/  itm=(unit datom)  (get:eavt idx.eavt.indexs q)
  ?~  itm
    ~
  ~[u.itm]

++  search-eav-
  |=  [q=[=e =a =v *] =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[=e =a =v tx=tx0]
  =/  eq=(unit indexer)  `[=e =a =v tx=tx0]
  =/  sub=(tree [indexer datom])   (subset:eavt idx.eavt.indexs sq eq)
  =/  items=(list [indexer datom])  (tap:eavt sub)
  (turn items itod)
:: 
::
:: (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ v tx
::              (filter (fn [^Datom d] (and (= v (.-v d))
::                                          (= tx (datom-tx d))))))
++  search-e-vt
  |=  [q=[=e * =v =tx] =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[=e a=*a v=*a tx=tx0]
  =/  eq=(unit indexer)  `[=e a=*a v=*v tx=txmax]
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
  |=  [q=[=e =a * =tx] =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)   `[=e =a v=~ tx=tx0]
  =/  eq=(unit indexer)   `[=e =a v=~ tx=txmax]
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
  |=  [q=[=e =a * *] =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `[=e =a v=~ tx=tx0]
  =/  eq=(unit indexer)  `[=e =a v=~ tx=txmax]
  =/  items  (subset:eavt idx.eavt.indexs sq eq)
  (turn (tap:eavt items) itod)
 
::         (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ v _
::              (filter (fn [^Datom d] (= v (.-v d)))))
++  search-e-v-
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ _ tx
 ::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search-e--t
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))       ;; e _ _ _
++  search-e---
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (if (indexing? db a)                                                   ;; _ a v tx
 ::           (->> (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))      
 ::                (filter (fn [^Datom d] (= tx (datom-tx d)))))
 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                            (= tx (datom-tx d)))))))
++  search--avt
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (if (indexing? db a)                                                   ;; _ a v _
 ::           (set/slice avet (datom e0 a v tx0) (datom emax a v txmax))
 ::           (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))
 ::                (filter (fn [^Datom d] (= v (.-v d))))))
++  search--av-
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (->> (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))  ;; _ a _ tx
 ::              (filter (fn [^Datom d] (= tx (datom-tx d)))))
++  search--a-t
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (set/slice aevt (datom e0 a nil tx0) (datom emax a nil txmax))       ;; _ a _ _
++  search--a--
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                     (= tx (datom-tx d)))) eavt)                ;; _ _ v tx
++  search---vt
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::         (filter (fn [^Datom d] (= v (.-v d))) eavt)                            ;; _ _ v _
++  search---v-
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::      (filter (fn [^Datom d] (= tx (datom-tx d))) eavt)                      ;; _ _ _ tx
++  search----t
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
 ::      eavt])))                                                               ;; _ _ _ _
++  search-----
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
:: 

++  search
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
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
    [* * ~ ~]  (search-ea-- [e=(need e.q) a=(need a.q) v=~ tx=~] indexs)
    [* * ~ *]  (search-ea-t [e=(need e.q) a=(need a.q) v=~ tx=(need tx.q)] indexs)
    [* * * ~]  (search-eav- [e=(need e.q) a=(need a.q) v=(need v.q) tx=~] indexs) 
    [* * * *]  
  (search-eavt [e=(need e.q) a=(need a.q) v=(need v.q) tx=(need tx.q)] indexs)
 
  ==
  
--  

