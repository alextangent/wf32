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
\
\ Marker Empty and Anew
\
\ ------------------------------------------------------------------------

cr .( Loading Forget Wordset...)

\ NOTE
\ 1) Forget no longer works.
\ 2) Markers now reset the user area.

\ Structure of Mark and Marker pfa

((
begin-structure mark%
  field: mark.me
  field: mark.adp
  field: mark.cdp
  field: mark.sdp
  field: mark.user
  field: mark.extra
end-structure
))

0   cell +field >me
    cell +field >pre-adp
    cell +field >pre-cdp
    cell +field >pre-sdp
    cell +field >pre-user
0        +field >forget-extra drop

variable fence                          \ cannot forget below this address

: in-space?     ( addr ?dp -- f )
                dup cell+ @ swap 2 cells+ @ between ;

: (sys-trim?)   ( nfa addr -- addr pre-?dp ?dp-top )
                swap name>interpret >body
                over adp in-space? if >pre-adp @ adp 2 cells+ @ exit then
                over cdp in-space? if >pre-cdp @ cdp 2 cells+ @ exit then
                over sdp in-space? if >pre-sdp @ sdp 2 cells+ @ exit then
                drop 0 -1 ; \ dirty trick

: sys-trim?     ( nfa addr -- f )
                (sys-trim?) between ;

defer trim? ' sys-trim? is trim?

: full-trim      ( nfa link -- )         \ Order-independent trim
        begin   ( nfa link^ )
                dup @
        while   ( nfa link^ )
                2dup @ trim?
                if      dup @ @ over !  \ Delete chain member
                else    @               \ Retain chain member
                then
        repeat  2drop ;

: trim-chains   ( nfa -- nfa )          \ trim down the chain linked list
        chain-link
        begin   @ ?dup
        while   2dup -2 cells+ full-trim
        repeat  dup chain-link full-trim ;

: trim-WinLibs    ( nfa -- nfa )
          dup lib-link full-trim ;

' trim-WinLibs forget-chain chain+ 

: vtrim ( nfa voc-thread -- ) \ trim VOC-THREAD back to nfa
        dup voc#threads 0
        do      2dup i cells+ full-trim
        loop    2drop ;

: (forget) ( pfa -- )
        dup >me @
        forget-chain chain-iterate   \ do forget-chain before trimming it
        trim-chains
        context #vocs cells+ context
        do      dup i @ trim?
                if      [ ' forth >body ] literal i !
                then
                cell +loop swap
        dup >pre-adp @ adp !
        dup >pre-cdp @ cdp !
        dup >pre-sdp @ sdp !
        >pre-user @ next-user !
        voc-link 2dup full-trim
        begin   @ ?dup
        while   2dup
                vtrim
        repeat  drop
        ;

: Get-marking-info    ( -- user sdp cdp adp )
                      next-user @ sdp @ cdp @ adp @ ;

: Save-marking-info   ( user sdp cdp adp -- )
                      , , , , ;

: Save-search-order   ( -- )
        get-current ,                          \ save current
        get-order dup , 0 ?do , loop ;         \ save context search list

: (Restore-search-order) ( addr n -- wid1...widn )
        -if swap dup @ >r cell+ swap 1- recurse r>
        else 2drop then ;

: Restore-search-order ( addr -- )
        >forget-extra dup @ set-current         \ restore current
        cell+ dup @ dup>r swap cell+ swap
        (Restore-search-order) r> set-order  ;  \ restore context search list

: do-marker ( -- )
        does> dup cell+ @ fence @ <
        abort" Attempt to forget in protected part of the dictionary!"
        dup Restore-search-order (forget) ;

: mark  ( -<name>- )
        get-section 2>r create last @ ,
        get-current get-order only forth also definitions
        Get-marking-info rot >forget-extra 2 cells+
        get-order dup>r 0 ?do drop loop r> cells+ -rot save-marking-info
        save-search-order set-order set-current
        do-marker 2r> set-section ;

: marker ( -<name>- ) ( ANS)
        get-section 2>r
        Get-marking-info create last @ , Save-marking-info
        save-search-order do-marker 2r> set-section ;

: possibly  ( "name" -- )
  defined? ?dup if name>interpret execute  then ;

: anew  ( "name" -- )( run: -- )
  >in @ possibly  >in ! marker ;

here fence !
