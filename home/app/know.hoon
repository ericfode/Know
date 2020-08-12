/+  know, default-agent, dbug
~%  %know-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0  [%0 db:know]
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %know-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch  on-watch:def
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
