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

require sortsearch

cr .( Loading export support... ) sourcefilename type

: export: ( n -- )                             \ generate export
         ['] imports >body swap-current >r
         callback: texp tfa!
         r> set-current                        \ original definitions
         ;

: exp-name-store ( -- count )           \ build name table at here
    align
    0 [: dup tfa@ texp = if
          , 1+ true 
         else 
           drop true
         then
      ;]  [ ' imports >body ] literal traverse-wordlist
    ;

: exp-name-table ( -- count )           \ store name addresses
    here exp-name-store dup>r
    ['] cstr-ascend is precedes         \ sort them by name
    sort r> ;

: exp-ord-table ( entries -- )           \ generate the ordinal table
    0 ?do i w, loop ;

: exp-ptr-table ( name-table entries -- )
    cells bounds ?do                    \ for each ptr-table entry
      i @ name>interpret appinst - ,           \ fetch nfa from name-table, calc xt rva
    cell +loop ;

: exp-rva-fix ( name-table entries -- ) \ fix the name table to rva and zstrings
    cells bounds ?do
      1 appinst - i +!                  \ rva it (1 to make cstr into zstr)
    cell +loop ;

begin-structure edt%      \ export dictionary table
  field: edt.flags        \ export flags
  field: edt.timedate     \ time date of binding
  field: edt.version      \ version
  field: edt.dllname      \ name rva
  field: edt.ordbase      \ ordinal base
  field: edt.#eat         \ # eat entries
  field: edt.#names       \ # name pointers
  field: edt.rva-addr     \ address table rva
  field: edt.rva-name     \ name pointer table rva
  field: edt.rva-ord      \ ordinal table rva
end-structure

: exp-tables ( -- edt len )     \ fixme
    align
    here >r edt% allot
    here appinst - r@ edt.rva-name !
    here exp-name-table                  \ generate name table, return #entries
    dup r@ edt.#eat !                    \ number of entries
    dup r@ edt.#names !

    here appinst - r@ edt.rva-ord !      \ ordinal rva
    dup exp-ord-table                    \ generate n ordinals
    
    here appinst - r@ edt.rva-addr !     \ rva of pointers
    2dup exp-ptr-table                   \ generate pointers
    exp-rva-fix                          \ fixup rva in name table
    r> here over -                       \ calc length
    ;

4 import: VirtualProtect

: exp-vp ( attr -- old-attr )
    >r 0 sp@ r> 1 appinst VirtualProtect 0= abort" VP failed" ;

: exp-install ( addr len -- )           \ install exports section
     PAGE_READWRITE exp-vp >r
     [ also vimage ]
     [ dos% pe32% + ] literal appinst + ( point at PE DDICT section )
     dup>r pedd.export @ ?dup if
       appinst + dup release ( *** error as this is alloted )
     then
     swap appinst - r> pedd.export 2!
     [ previous ]
     r> exp-vp drop ;

library (self.dll)
appinst (self.dll) lib.handle ! \ correct self.dll, mark ep as loaded

: export-init ( -- )
    exp-tables exp-install ;

' export-init init-chain chain+

\ export-init ( issue this once exports all defined )

