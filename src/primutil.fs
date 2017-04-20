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

cr .( Loading prim util support... ) sourcefilename type

decimal                                 \ start everything in decimal

\ -------------------- Chains -----------------------------------------------

variable chain-link 0 chain-link ! \ linked list of chains

: +chain ( xt chain_address -- ) \ for normal forward chains
    link, , ;

: chain+ ( xt chain_address -- ) \ add chain item at the end
    begin dup @ dup while nip repeat drop
    +chain ;

: chain: ( <name> -- )
    create 0 ,
    ['] noop ,
    chain-link link,
    last @ , ;

: chain-iterate ( chain_address -- ) @ [: cell+ perform ;] list-apply ;

\ -------------------------- Chain definitions ---------------------------

chain: reset-stack-chain  \ chain for stack reset
chain:        init-chain  \ chain of things to initialize
chain:      forget-chain  \ chain of types of things to forget

:noname         ( -- )         \ chain for init
                init-chain chain-iterate ; ' init-forth defer! \ install in kernel word

' _reset-stacks reset-stack-chain chain+

:noname         ( ?? -- )      \ chain for stack reset
    reset-stack-chain chain-iterate ; ' reset-stacks defer! \ install in kernel word

' init-title init-chain +chain
init-title

\ ---------------- Operating System Checking -----------------------------

library kernel32.dll
library user32.dll

1 import: GetVersionEx

3 constant win2k      ( 5.0 )
4 constant winxp      ( 5.1 )
5 constant win2003    ( 5.2 )
6 constant winvista   ( 6.0 )
7 constant win7       ( 6.1 )
8 constant win8       ( 6.2 )

\ To check for a version, say Win2K or greater, try WINVER WIN2K >=

: winver ( -- )                \ get windows version
    148 buf-allot dup>r !       \ set length of structure
    r@ GetVersionEx drop         \ call os for version
    r> cell+ 2@
    5 - 3 * + 3 + ;

3 import: GetModuleFileName
1 import: PathStripPath

: program-name  ( -- c-addr len )
    buf-allot dup maxcounted over appinst
    GetModuleFileName drop  \ get exe name
    PathStripPath drop zcount ;

\ -------------------- Line and source file --------------------------------

\ To get the line number for an xt, fetch the vfa. To get the filename for this
\ xt, the files vocabulary is iterated; each file name when executed returns a ptr.
\ See include-start and include-end for structure built when a file is included.
\ For example, for a file A which includes B and C, the code addresses [ ] might be

\  [----------------- file A -----------------------]
\          [----- file B -----]    [- file C -]
\     X                     Y                         Z
 
\ X is defined in A; Y is defined in B. We iterate over the files and their
\ range information looking for the smallest span that contains the xt.
\ Y is in both A and B, but B's span is smaller. Z was defined from the
\ console, which has a dummy range that covers all of the addressable memory.

: xt-in-range ( xt filext -- span | -1 ) \ check against both ranges
    execute 2@                        \ xt low high
    2dup swap - >r within r> and      \ return the span (high-low)
    ?dup 0= if -1 then ;              \ return huge span if not found

: xt-in-file ( bestnfa span myxt nfa -- bestnfa' -span' myxt true )
    2dup 2>r name>interpret xt-in-range \ check if myxt is in file range
    2dup u> if                        \ if original span smaller
      nip nip 2r> -rot true           \ replace with new bestnfa and span
    else drop 2r> then ;              \ drop span' (nfa acts as true)

: >ffa@ ( nfa -- ffa )                \ get the file field
    c" (console)" -1                  \ default is console, all of memory
    rot name>interpret                \ get the xt to search for
    ['] xt-in-file                    \ iterator
    [ ' files >body ] literal traverse-wordlist 2drop ;     \ files vocab

: (.viewinfo) ( nfa -- line# file count )       \ fetch line #, file name
    dup head.vfa w@ swap >ffa@ count ;

: .viewinfo ( nfa -- ) space (.viewinfo) (.viewtype) ; \ print the file, line#

: where ( <-name-> ) ' >name .viewinfo ;  \ show where a line is defined


