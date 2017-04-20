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

cr .( Loading Image Headers)

: fld        ( basevar offset len -<name>- -- basevar offset+len )  \ definer for fields
    create
    3dup + 2>r               \ n1 n2 n3 r: n1 n2+n3
    drop                     \ n1 n2 r: n1 n2+n3
    , , 2r>                  \ build n2 n1
    does>  ( -- baseaddr )
      dup @                  \ fetch n2
      swap cell+ @ @ + ;     \ fetch n1 value, add

: fldbase    ( -<name>- )    \ set field base name (starts field)
    create here 0 , 0  ;     \ base of record

\ ----------------------- pecoff structure ------------------------
1 constant exe
1 constant gui
1 constant graphical
2 constant dll
3 constant cui
((

fldbase base-sect
         8 fld         sect-name        \ name, null terminated (if not= 8)
         4 fld         sect-virtsize    \ size in memory
         4 fld         sect-rva         \ address in memory
         4 fld         sect-datasize    \ size of data
         4 fld         sect-fp-data     \ file pointer to data
         4 fld         sect-fp-relocs   \ file pointer to relocs (0)
         4 fld         sect-fp-line#    \ fp to linenums
         2 fld         sect-#relocs     \ set to zero for exe
         2 fld         sect-#line#      \ set to zero
         4 fld         sect-character   \ characteristics, see sectiontype
   constant len-sect drop
))
((
fldbase base-edt                        \ export dictionary table
         4 fld         edt-flags        \ export flags
         4 fld         edt-timedate     \ time date of binding
         4 fld         edt-version      \ version
         4 fld         edt-dllname      \ name rva
         4 fld         edt-ordbase      \ ordinal base
         4 fld         edt-#eat         \ # eat entries
         4 fld         edt-#names       \ # name pointers
         4 fld         edt-rva-addr     \ address table rva
         4 fld         edt-rva-name     \ name pointer table rva
         4 fld         edt-rva-ord      \ ordinal table rva
   constant len-edt drop

fldbase base-reloc                      \ relocation
         4 fld         reloc-rva-page   \ rva of page for relocation table
         4 fld         reloc-blocklen   \ length of the reloc section (from reloc-rva-page)
         2 fld         reloc-fixup      \ 4 bit type, 12 bit offset
   value reloc-len drop                 \ length, variable

$0000 constant reloc-abs               \ absolute (a noop)
$3000 constant reloc-hilo              \ high/low relocation (32 bit relocation)
))


