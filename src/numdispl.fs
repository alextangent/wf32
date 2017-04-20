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
\    Redistribution and use in source and binary forms, with or without
\    modification, are permitted provided that the following conditions are
\    met:
\
\    1. Redistributions of source code must retain the above copyright
\    notice, this list of conditions and the following disclaimer.
\
\    2. Redistributions in binary form must reproduce the above copyright
\    notice, this list of conditions and the following disclaimer in the
\    documentation and/or other materials provided with the distribution.
\
\    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
\    IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
\    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
\    PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
\    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
\    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
\    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
\    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
\    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (
\    INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
\    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. ))
\
\
\
\ ------------------------------------------------------------------------

\ ----------------------- Floating point ---------------------------------------

\ defined in FLOAT.F

\ ----------------------- Number display ---------------------------------------

: (xud,.)       ( ud commas -- a1 n1 )
    >r
    <#                      \ every 'commas' digits from right
    r@ 0 do # 2dup d0= ?leave loop
    begin   2dup d0= 0=     \ while not a double zero
    while   44 ( , ) hold
            r@ 0 do # 2dup d0= ?leave loop
    repeat  #> rdrop ;

: (ud,.)        ( ud -- a1 n1 )
    base @             \ get the base
    dup  #10 =         \ if decimal use comma every 3 digits
    swap  #8 = or      \ or octal   use comma every 3 digits
    #4 + (xud,.) ;     \ display commas every 3 or 4 digits

: ud,.r         ( ud l -- )        \ right justified, with ','
    >r (ud,.) r> over - spaces type ;

: u,.r          ( n1 n2 -- )       \ display double unsigned, justified in field
    0 swap ud,.r ;

: ud.           ( ud -- )          \ display double unsigned
    0 ud,.r ;

: ud.r          ( ud l -- )        \ right justified, without ','
    >r 16 (xud,.) r> over - spaces type ;

: (d.#)         ( d1 n1 -- a1 n1 ) \ display d1 with n1 places behind dp
    >r <#              \ n1=negative will display'.' but no digits
    r> ?dup            \ if not zero, then display places
    if      0 max 0 ?do # loop '.' hold
    then    #s #> ;

: d.r.#         ( d1 n1 n2 -- )    \ print d1 in a field of n1 characters,
    swap >r (d.#) r> over - spaces type ; \ display with n2 places behind dp

: .r.1          ( n1 n2 -- )       \ print n1 right justified in field of n2
    0 swap 1 d.r.# ;   \ display with one place behind dp



