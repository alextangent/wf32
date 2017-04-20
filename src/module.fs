\ --------------------------- Change Block -------------------------------
\
\
\ ------------------------- End Change Block -----------------------------
\
\ Experimental: a fully optimising, STC based, ANS Forth compliant kernel
\
\
\    Copyright [c] 2005, 2017 by Alex McDonald (alex at rivadpm dot com)
\
\    Parts copyright others; borrowed from public domain Win32Forth 
\    and heavily modified.
\
\ ------------------------------------------------------------------------

cr .( Loading module support... ) sourcefilename type

only forth also hidden definitions

0 value in-module?        \ previous vocabulary

defer hidden-voc ' hidden is hidden-voc

create save-order-buff #vocs 2 + cells allot

: save-order    ( -- )
                get-current get-order
                dup 2 + 0 ?do
                  save-order-buff i cells+ !
                loop
                ;

: restore-order ( -- )
                0 save-order-buff @ 1+ ?do
                  save-order-buff i cells+ @
                -1 +loop
                set-order set-current
                ;

previous definitions also hidden

: (private)     ( xt-of-voc-- )  \ set vocabulary for internal definitions
                is hidden-voc ;

: private       ( -<voc>- )      \ set vocabulary for internal definitions
                ' (private) ;

\ internal adds hidden to context and sets as current

: internal      ( -- )           \ start the internal list of words for a module
                in-module? not if
                  save-order
                  also hidden-voc
                then definitions 
                true to in-module? ;
                

: external      ( -- )           \ start the external list of words for a module
                in-module? if
                  previous definitions also hidden-voc
                else
                  save-order
                  also definitions hidden-voc
                then 
                true to in-module? ;

: module        ( -- )           \ complete the module
                in-module? if
                  restore-order
                then
                false to in-module?
                ['] hidden (private) ;


only forth also definitions



\S


(PRIVATE)    xt-of-voc --

.. Use vocab as hidden vocabulary for next module ..

PRIVATE -<vocab>-

.. Use -<vocab>- as hidden vocabulary for next module ..

INTERNAL

.. internal definitions ..

EXTERNAL

.. externally available definitions ..

MODULE

.. back to whatever vocabulary we started in ..



