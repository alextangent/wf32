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


cr .( Loading locals support... ) sourcefilename type

decimal

((

\ ----------------------- Locals support -------------------------------
 
    Restrictions
      DO NOT mix LOCALS| ... | and {: ... :} together
      DO NOT declare locals inside control structures
    
    Extensions
      Recognises >r r> etc and when generating the code to fetch/set a local

    Syntax is (where [ and ] represent meta-symbols for optional parameters)
      {: [a b [t] c] [ | [d e]] [-- [comments]] :}
    The following are valid examples;
      {: :}   {: a :}   {: a -- :}   {: a -- comment :}
      {: a b | c :}     {: | c :}    {: a | c -- comment :}

    Local variables can have any name except | -- or :}
    Avoid local names that end in “:”, “[”, “^” or that are a 
    single non-alphabetic character
    NOTE: {: a -<name>- :} declares two locals; it doesn't indicate a parsing word
          {: a -- -<name>- :} is the only correct method

    [t] <name> is a typed name. This matches gforth's typing of locals
    Specifier   Type    Access
    W:          Cell    value       The default
    W^          Cell    address
    D:          Double  value
    D^          Double  address
    F:          Float   value
    F^          Float   address
    C:          Char    value
    C^          Char    address

    Generation of the code is split into several parts
    : foo ... {: a | b :} ... {: c | d :} ... ;
    1         2  X 3 X 4      2  X 3 X 4      5
    1 Initialise, done by kernel defined defer LOCALS-INIT
        Sets all variables to zero
        locincr <-- 1
    2 {: start locals declaration
        locinits <-- 0
        localoff <-- 0
    X declare local based on loctotal
        locinits <-- locinits + locincr
        localoff <-- localoff + 1
        loctotal <-- loctotal + 1
    3 | end of declared
        locincr <-- 0
    4 :} ends locals declaration
        generate locinits moves from stack to rstack
        adjust rstack by locinits + localoff
    (Step 2)
      For each initialised local , the value is taken from the stack and pushed
      on the rstack. (There's a bit of code to handle top of stack because
      it's in eax.)
    (Step 3)
      EBP and TOS are adjusted to account for any stack to rstack movement.

    Nothing is generated for {: :} (null locals). Step 2 is omitted if there are
    no initialised locals as in {: | a b :}. Step 1 is omitted if there are no
    uninitialised locals as in {: a b :}.

\ ---------------------------------------------------------------------------
))

only forth definitions

1 #vocabulary loc-types         \ local types (see C^ etc)

create ldp  0 , 0 , 0 , dp-link link, ," *loc-d"
create mdp  0 , 0 , 0 , dp-link link, ," *loc-c"

1 #vocabulary locals            \ locals vocab
' (common-header)                        \ build head in locals area & voc
  ' locals >body voc.head !              \ set as locals vocabulary header create

\ IN-LOC is used to switch HERE ALLOT , W, etc to point
\ to the locals data areas. Public because IMAGEMAN uses it
: in-loc         ( -- ) ldp mdp  set-section ;

also hidden definitions

: init-local  ( -- )                          \ init the required locals area
    #8192 malloc ( allocate the area )
      dup [ ldp data.here   ] literal !
      dup [ ldp data.origin ] literal !
    #4096 +
      dup [ ldp data.top    ] literal !
      dup [ mdp data.here   ] literal !
      dup [ mdp data.origin ] literal !
    #4096 +
          [ mdp data.top    ] literal ! ;

' init-local init-chain +chain
init-local

\ local-gen generates the locals header based on several pieces of info   
\ collected during {: ... :} or LOCALS| ... | parsing.                    

create (locvars)
    0 ,  \ locadjst        adjustment for word that use rstack
    0 ,  \ loctotal        number of declared locals in this word              
    0 ,  \ locstart        first local in this {: :} section                   
    0 ,  \ locinits        number of initialised locals in this {: :} section  
    0 ,  \ locorder        direction; 0 = {: :} , -1 = LOCALS| |
    0 ,  \ locincr         1 in {: x | section, 0 in | x :} section
here (locvars) - constant (locvars-len)

(locvars)          constant locadjst \ offset when in do loop or inside a scope
(locvars) 1 cells+ constant loctotal \ number of declared locals in this word
(locvars) 2 cells+ constant locstart \ number of locals at start of {: :} section
(locvars) 3 cells+ constant locinits \ number of initialised locals in this {: :} section
(locvars) 4 cells+ constant locorder \ direction; 0 = {: :} , -1 = LOCALS| |
(locvars) 5 cells+ constant locincr  \ 0 when uninitialised, 1 when from stack

:noname ( -- )
    sync-code
    [ ldp data.origin ] literal @ ldp !           \ reset origin
    [ mdp data.origin ] literal @ mdp !           \ reset origin
    [ ' locals >body voc.#0 ] literal off  \ clean thread in vocabulary
    (locvars) (locvars-len) erase
    ;
  is locals-init           \ in kernel

:noname  2 locadjst +! [ ' do      >comp @ xt-call, ] ; compiles-for do
:noname  2 locadjst +! [ ' ?do     >comp @ xt-call, ] ; compiles-for ?do
:noname  2 locadjst +! [ ' 2>r     >comp @ xt-call, ] ; compiles-for 2>r
:noname  1 locadjst +! [ ' >r      >comp @ xt-call, ] ; compiles-for >r
:noname  1 locadjst +! [ ' dup>r   >comp @ xt-call, ] ; compiles-for dup>r
:noname  1 locadjst +! [ ' [:      >comp @ xt-call, ] ; compiles-for [:
:noname -1 locadjst +! loctotal @ >r loctotal off      \ stops unnest destroying locals
                       [ ' ;]      >comp @ xt-call, ]
                       r> loctotal !                  ; compiles-for ;]
:noname -1 locadjst +! [ ' r>      >comp @ xt-call, ] ; compiles-for r>
:noname -1 locadjst +! [ ' r>drop  >comp @ xt-call, ] ; compiles-for r>drop
:noname -2 locadjst +! [ ' 2r>     >comp @ xt-call, ] ; compiles-for 2r>
:noname -2 locadjst +! [ ' 2rdrop  >comp @ xt-call, ] ; compiles-for 2rdrop
:noname -2 locadjst +! [ ' loop    >comp @ xt-call, ] ; compiles-for loop
:noname -2 locadjst +! [ ' +loop   >comp @ xt-call, ] ; compiles-for +loop
:noname -2 locadjst +! [ ' -loop   >comp @ xt-call, ] ; compiles-for -loop

\ add-ebp,n defined in the kernel
: add-esp,n ( n -- ) >r asm[ add     esp r>              a;] ;

: totaladj ( -- )
    loctotal @ ?dup if cells add-esp,n loctotal off then ;

:noname totaladj       [ ' unnest  >comp @ xt-call, ] ; compiles-for unnest
:noname totaladj       [ ' (;code) >comp @ xt-call, ] ; compiles-for (;code)
:noname loctotal @ dup>r if 0 to tail-call then
                       [ ' exit    >comp @ xt-call, ]
                       r> loctotal !                  ; compiles-for exit

: eax>r     (   -- )    asm[ push  eax                 a;] ;
: {n}>r     ( n -- ) >r asm[ push  { r> ebp }          a;] ;
: {n}@      ( n -- ) >r asm[ mov   eax { r> ebp }      a;] ;
: locn&     ( n -- ) >r asm[ lea   eax { r> esp }      a;] ;
: locn@     ( n -- ) >r asm[ mov   eax { r> esp }      a;] ;
: locn!     ( n -- ) >r asm[ mov   { r> esp } eax      a;] ;
: locn+!    ( n -- ) >r asm[ add   { r> esp } eax      a;] ;
: locc@     ( n -- ) >r asm[ movzx eax byte { r> esp } a;] ;

$04 constant tlvl     \ local value
\ $06 constant tl2v     \ local 2value, unsupported as yet

: local-1cellsz ( -- )                 \ cell sized
    locincr @ locinits +!              \ total count init off stack
    loctotal 1+! ;                     \ total in entire word

: local-2cellsz ( -- )                 \ double cell sized
    local-1cellsz local-1cellsz ;      \ two cells

: create-local ( addr len -- )
    ['] create execute-parsing         \ create from passed buffer
    loctotal @ negate ,                \ declare time: negative offset
    immediate ;                        \ mark as immediate & a local

: local-off ( body -- n )              \ calculate offset up stack
    @ loctotal @ +                     \ declare time + current total
    locadjst @ + cells                 \ adjust for do loop etc
    sync-code ;

: local-ref ( body -- n )
    postpone dup local-off ; \ generate dup to save tos

\ A local name builds an action; fetch, address or (for TO and +TO) store
\ at a calculated offset up the return stack. TO and +TO only work for
\ C: and W: types, since we don't support D: yet. Only required in the compile
\ table, as locals not available when interpreting.

: local-w: ( addr len -- ) \ generate fetch
    local-1cellsz create-local tlvl tfa!
    does> local-ref locn@ ;
: local-w^ ( addr len -- ) \ generate address
    local-1cellsz create-local           
    does> local-ref locn& ; 
: local-c: ( addr len -- ) \ generate fetch or store
    local-1cellsz create-local tlvl tfa! 
    does> local-ref locc@ ;
: local-d^ ( addr len -- ) \ generate address
    local-2cellsz create-local
    does> local-ref locn& ;

:noname ( body -- ) 
    local-off locn!  postpone drop ;
    (to-compile)  tlvl 1- cells + !
:noname ( body -- ) 
    local-off locn+! postpone drop ;
    (+to-compile) tlvl 1- cells + !
   
: local-gen ( -- ) \ generate code to move data from stack to rstack
  ( step 1)     locinits @ ?dup if                    \ any declared?
                  dup 1- dup                        \ n n-1 n-1
                  locorder @ if                       \ order; LOCALS| or {:
    ( step 2a)      eax>r                           \ LOCAL| |last one; push TOS
                    0 ?do i cells {n}>r loop        \ n-1 loops store the offset
                  else
    ( step 2b)      dup 1- swap 0max                \ {: :}
                    0 ?do dup i - cells {n}>r loop drop \ n-1 loops store the offset
                    eax>r                           \ first one; push TOS
                  then
   ( step 3)      locstart @ - cells {n}@             \ n-1 PICK
                  cells add-ebp,n                   \ n cells drop
                then
                loctotal @ locstart @ - locinits @ -
                  ?dup if negate cells add-esp,n          \ reserve n cells space
                then ;

' locals >body voc.srch constant locals-srch  \ search function in locals voc

:noname ( addr len -- nfa | 0)        \ as in gforth, return name token
    loctotal @ if                       \ only search if any locals
      also locals
      (find-context)
      previous                        \ temp code; see above
    else (find-context) then ;
  is find-name                        \ in kernel

: in-loctype ( -- warning latestxt last last-link
               find-name voc-srch current )
    in-loc
    warning @
    latestxt @ last @ last-link @       \ save last (we wipe out)
    ['] find-name defer@
    locals-srch @
    get-current ;
      
: out-loctype ( warning latestxt last last-link
               find-name voc-srch current -- )
    in-app
    previous previous
    set-current
    locals-srch !
    ['] find-name defer!
    last-link ! last ! latestxt !
    warning ! ;
      
-361 constant throw_lnotype
throw_msgs link, throw_lnotype , ," local type unsupported"

: local-notype ( -- )
    out-loctype throw_lnotype throw ;

also forth definitions

: {: ( switch to locals vocabs )
    only-compiles>
      sync-code
      in-loctype
      warning off
      [: ( addr len wid -- nfa )
         drop local-w: [ ' noop >name ] literal
      ;] locals-srch !
      ['] (find-context) is find-name
      also locals definitions
      also loc-types
      1 locincr !
      locinits off     
      loctotal @ locstart ! ;
      
' {: alias {

also loc-types definitions

: :} ( switch out of locals vocab )
    only-compiles>
      sync-code
      out-loctype
      local-gen ;

' :} alias }

: -- ( -- ) \ skip everything up to } or :}
    postpone :}
    begin
      definite-word 
      2dup s" :}" istr= if 2drop exit then
           s" }"  istr= if       exit then
    again ; immediate

: | ( -- ) \ uninitialised {: | x :} or end of LOCALS| x |
    locorder @ if postpone :} else locincr off then ; immediate

: w: ( cell sized local value )     parse-name local-w: ; immediate
: w^ ( cell sized local variable )  parse-name local-w^ ; immediate
synonym c^ w^
: c: ( char sized local value )     parse-name local-c: ; immediate
: d^ ( 2cell sized local variable ) parse-name local-d^ ; immediate
: d: ( 2cell sized local 2value )   local-notype ; immediate
: f: ( 2cell sized local float )    local-notype ; immediate
: f^ ( 2cell sized local float )    local-notype ; immediate

only forth definitions
also hidden also loc-types  

: locals| ( -- ) \ ans standard locals reversed stack order
    only-compiles> locorder on postpone {: ;

: (local) ( addr len -- )
    dup if 
      buf-allot dup>r 0 swap c!
      s" {: " r@ +place
      r@ +place
      s"  :}" r@ +place
      r> count evaluate 
    else 2drop then ;

: local ( -<name>- -- )
    only-compiles> ]] {: w: :} [[ ;

previous previous
