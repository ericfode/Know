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
++  eavt  ((ordered-map indexer datom) cmp-eavt)
++  avet  ((ordered-map indexer datom) cmp-aevt)
++  aevt  ((ordered-map indexer datom) cmp-avet)
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

++  search-eav-
  |=  [q=[=e =a =v tx=(unit tx)] =indexs]
  ^-  (list datom)
  =/  sq=(unit indexer)  `q(tx tx0)
  =/  eq=(unit indexer)  `q(tx txmax)
  =/  idx=(tree [indexer datom])  idx.eavt.indexs
  =/  sub=(tree [indexer datom])   (subset:eavt idx sq eq)
  =/  items=(list [indexer datom])  (tap:eavt sub)
  %+  turn  items  |=([=indexer =datom] datom)   
 
++  search-ea-t
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  =.  all-e-a  
    (subset:eavt eavt.indexs q(tx tx0) q(tx txmax))
  %-  (traverse:eavt ,(list datom))
    :*  all-e-a 
        state=~
        |=  [s=(list datom) k=indexer v=datom] 
        ?:  ?=((need tx) tx.d)
        [datom s]
        s
    ==
++  search-ea--
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  (tap:eavt (subset:eavt eavt.indexs q(tx tx0) q(tx txmax)))

:: (->> (set/slice eavt (datom e nil nil tx0) (datom e nil nil txmax))  ;; e _ v tx
 ::              (filter (fn [^Datom d] (and (= v (.-v d))
 ::                                          (= tx (datom-tx d))))))
++  search-ea--
  |=  [q=[e=(unit e) a=(unit a) v=(unit v) tx=(unit tx)] =indexs]
  ^-  (list datom)
  !!
 
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
    [* * * *]  (get:eavt eavt.indexs q)
    [* * * ~]  (search-eav- q(e u.e.q, a u.a.q, v u.v.q, tx ~) indexs) 
    [* * ~ *]  (search-ea-t q)
    [* * ~ ~]  (search-ea-- q indexs)
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

