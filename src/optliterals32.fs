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

\ ------------------------------------------------------------------------
\
\ constants & literals optimisation
\
\ ------------------------------------------------------------------------

cr .( Loading optimiser [literals]...)  sourcefilename type

[undefined] optimise [if]
  vocabulary optimise
[then]

also optimise definitions

\ This stack of literals should really be a queue, as we need fifo. Emulated
\ by reversing the contents as it's popped.

100 stack lits

\ *bug this is in-system, but the chain is in-application; turnkeys fail to run
: reset-lits ( -- )                       \ clear the stack
           lits -stack ;

' reset-lits reset-stack-chain chain+

\ The stack is addressed much in the same way PICK would; 0 refers to the top of
\ stack, and -1 to next of stack. 1 refers to the first new entry, and
\ so on for increasing positive numbers.
\ Keep in mind that the stack (addressed by ebp) grows downwards, so 3 +stk makes
\ the stack 3 cells bigger, and conversely -1 +stk makes it one smaller.

: +stk ( n ) ?dup if >r asm[ sub ebp r> cells a;] then ; \ adjust stack

\ not yet working

variable stk-delay 0 stk-delay !
: stk-adjust ( -- )                      \ generate delayed stack adjust
           stk-delay @ +stk
           0 stk-delay ! ;

: stack[]  ( -- )                 \ generate code to address stack entry n
           stk-delay @ ?dup if
             dup 0< if 1+ then
             negate cells >r asm[ { r@ ebp } ]asm r>drop
           else asm[ eax ]asm then ;

: (op-immed) ( op n -- ) >r execute asm[ stk-delay @ if dword then stack[] r> a;] ;

: op-immed1 ( op -- )             \ generate op <stack> # n
    create ,
    does> ( n -- )
      @ swap dup if
        (op-immed)
      else 2drop then ;

: op-select ( n -- xt )             \ select an opcode (-1=or, 0=and, other=mov)
    ?dup if
      1+ if
        asm[ ['] mov ]asm
      else
        asm[ ['] or ]asm
      then
    else
      asm[ ['] and ]asm
    then ;

asm[
' add  op-immed1 add-immed
' sub  op-immed1 sub-immed
' shl  op-immed1 shl-immed
' shr  op-immed1 shr-immed
' sar  op-immed1 sar-immed
' xor  op-immed1 xor-immed
' or   op-immed1  or-immed
' and  op-immed1 (and-immed)
' count-bits op-immed1 cnt-immed
]asm

: mov-immed ( n -- )                \ generate optimal move (-1=or, 0=and, other=mov)
     dup op-select
     swap (op-immed) ;

: and-immed ( n )
     dup if
       dup 1+ if
         (and-immed)         \ n<>-1
       else
         drop                \ n=-1
       then
     else
       mov-immed             \ n=0
     then ;

: litstack ( n xt -- ) drop lits spush ; \ stack literal
     
: lits=1?  ( -- n ) lits sdepth 1 = ;
: lits>0?  ( -- n ) lits sdepth ;
: lits>1?  ( -- n ) lits sdepth 1 > ;

variable in-sync in-sync off             \ to stop recursion in sync-code

\ addressing the data stack
\ eax refers to top of stack (eax in this case)
\ stk[-1] is next left (as in a stack diagram)
\ stk[1] is a new right stack entry
\ needs to account for stack delay code

: stk[]    ( off -- )                 \ generate code to address stack entry n
    ?dup if
      dup 0< if 1+ then
      negate cells >r asm[ { r> ebp } ]asm
    else asm[ eax ]asm then ;

: stk[1]      1 stk[] ;
: stk[-1]    -1 stk[] ;
: stk[-2]    -2 stk[] ;

: #->stk[]
    stk-delay dup @ >r !
    mov-immed
    r> stk-delay ! ;

\ generate code to get n in var. might be an and (0) or (-1) or mov 
\ and size is byte for C! and dword for !
: ->var(p1)  ( var n -- var n ) asm[ dup op-select execute ]asm ;
: ->var(p2)  ( var n -- ) asm[ { over } dup a;] 2drop ;
: #->var     ( var n -- ) ->var(p1) asm[ dword ]asm ->var(p2) ;
: #->cvar    ( var n -- ) ->var(p1) asm[ byte  ]asm ->var(p2) ;

: var->tos   ( var )   >r asm[ mov eax { r> }        a;] ;
: wvar->tos  ( var )   >r asm[ movzx eax word { r> } a;] ;
: cvar->tos  ( var )   >r asm[ movzx eax byte { r> } a;] ;
: tos->var   ( var )   >r asm[ mov   { r> } eax      a;] ;
: tos->stk[] ( off )   >r asm[ mov   r> stk[] eax    a;] ;
: tos->cvar  ( var )   >r asm[ mov   { r> } al       a;] ;
: add-v,tos  ( var )   >r asm[ add   { r> } eax      a;] ;
: add-v,#    ( var n -- ) dup if
                            asm[ add dword { over } dup a;]
                          then 2drop ;
: not-tos    ( -- )       asm[ not   eax             a;] ;
: loop-add   ( n -- )     asm[ >r add dword { esp }
                               r> jno 0              a;] ;
: setcc      ( -- )       asm[ cmp   eax 1
                               sbb   eax eax         a;] ;

: pop-tos  ( -- ) asm[ mov eax stk[-1] a;]  -1 +stk ; \ drop
: pop-2tos ( -- ) asm[ mov eax stk[-2] a;]  -2 +stk ; \ 2drop
: push-tos ( -- ) asm[ mov stk[1] eax  a;]   1 +stk ; \ dup

variable eax-lit

: litsync  ( -- )                        \ called when code is about to be generated
    in-sync @ 0= if               \ recursing?
      in-sync on                  \ no, so set
      asm[ a;]                    \ make sure assembler has finished
      stk-adjust
      lits>0? dup if              \ anything to do?
        macro[ mov stk[1] eax ]macro  \ save tos
        lits spop dup eax-lit !   \ get value off stack
        0 #->stk[]                 \ load tos
        lits sdepth 0 ?do         \ do for n-1 entries
          lits spop               \ get literal,
          over i -                \ calculate offset
          over eax-lit @ = if     \ compare
            nip tos->stk[]        \ same as eax, so save that instead
          else #->stk[] then       \ generate a literal onto stack
        loop
        +stk                     \ adjust stack
      else drop
      then
      in-sync off asm[ a;]    \ we're finished, so reset
    then
    ;

' litstack compiles-for literal
' litsync is sync-code

\ To help the optimiser, some code words that deal with constants
\ are broken up into their constituent parts for compile time
:noname drop postpone -cell postpone + ;          compiles-for cell-
:noname drop postpone cells postpone + ;          compiles-for cells+
:noname drop postpone cells postpone - ;          compiles-for cells-
:noname drop postpone swap postpone cells+ ;      compiles-for +cells
:noname drop postpone swap postpone cells- ;      compiles-for -cells
:noname drop 1 postpone literal postpone - ;      compiles-for 1-
:noname drop 2 postpone literal postpone - ;      compiles-for 2-
:noname drop 2 postpone literal postpone + ;      compiles-for 2+
:noname drop 1 postpone literal postpone + ; dup  compiles-for 1+
                                                  compiles-for char+

: litstart ( xt -- n )                    \ drop the xt, get constant
           drop lits spop sync-code ;

: opt@  ( xt -- ) lits>0? if litstart push-tos var->tos  else inline, then ;
: optw@ ( xt -- ) lits>0? if litstart push-tos wvar->tos else inline, then ;
: optc@ ( xt -- ) lits>0? if litstart push-tos cvar->tos else inline, then ;

: opt!     ( xt -- )
    lits=1? if
      litstart tos->var pop-tos
    else lits>1? if
      drop lits s2pop #->var
      else
        inline,
      then
    then
    ;

: optc!    ( xt -- )
    lits=1? if
      litstart tos->cvar pop-tos
    else lits>1? if
      drop lits s2pop #->cvar
      else
        inline,
      then
    then
    ;

: opt+!    ( xt -- )
    lits=1? if
      litstart add-v,tos pop-tos
    else lits>1? if
      drop lits s2pop add-v,#
      else
        inline,
      then
    then
    ;

' opt!     compiles-for !
' opt+!    compiles-for +!
' optc!    compiles-for c!
' opt@     compiles-for @
' optw@    compiles-for w@
' optc@    compiles-for c@

: opt/     ( xt -- ) 
    lits>1? if
      >r lits s2pop swap r> execute lits spush 
      else inline, then ;
' opt/ compiles-for /

: opt=     sub-immed setcc ;
: opt<>    sub-immed setcc not-tos ;
: opt<     sub-immed postpone 0<  ;
: opt<=    1+ opt< ;
: opt>=    opt< not-tos ;                
: opt>     opt<= not-tos ;

: optswap  1 #->stk[] 1 +stk ;

: imul-immed ( n )
    dup -1 = if postpone negate drop exit then
    dup 0= if mov-immed exit then
    dup 1 = if drop exit then
    dup 2 = if postpone 2* drop exit then
    dup count-bits 1 = if
      msbit shl-immed exit    \ shift for 4 8 16 32 etc
    then
    >r asm[ imul eax eax r> a;]
    ;

:noname  ( xt -- )
    lits>0? if
      litstart postpone dup dup if
        1+ negate >r asm[ mov eax r> stk[] a;]
      else drop then
    else
      inline,
    then ; compiles-for pick

:noname ( xt -- ) \ execute the xt
    lits>0? if
      litstart >r asm[ call r> a;]
    else
      inline,
    then ; compiles-for execute

:noname ( var -- ) \ perform the xt
    lits>0? if
      litstart >r asm[ call { r> } a;]
    else
      inline,
    then ; compiles-for perform

\ Only add entries with optimising code, or those with stack effects [...] n -- [...]
\ where n can be a literal. When there are emough literals on the stack for the xt,
\ it's executed at compile time and the results used instead. If there's only 1 literal,
\ then this table is used to generate the code. If nothing to do, then inline the code.

create opt-lit-table1
  ' +      , ' add-immed  ,
  ' -      , ' sub-immed  ,
  ' *      , ' imul-immed ,
  ' and    , ' and-immed  ,
  ' or     , ' or-immed   ,
  ' xor    , ' xor-immed  ,
  ' lshift , ' shl-immed  ,
  ' rshift , ' shr-immed  ,
  ' arshift , ' sar-immed ,
  ' =      , ' opt=       ,
  ' <>     , ' opt<>      ,
  ' <      , ' opt<       ,
  ' <=     , ' opt<=      ,
  ' >=     , ' opt>=      ,
  ' >      , ' opt>       ,
  ' 0=     , ' setcc      ,
  ' 0<>    , ' inline,    ,
  ' 0<     , ' inline,    ,
  ' 0>     , ' inline,    ,
  ' not    , ' setcc      ,
  ' invert , ' inline,    ,
  ' negate , ' inline,    ,
  ' cells  , ' inline,    ,
  ' dup    , ' push-tos   ,
  ' drop   , ' pop-tos    ,
  ' swap   , ' optswap    ,
  ' nip    , ' inline,    ,
  ' count-bits , ' cnt-immed  ,
    0      ,

: nseopt   ( xt -- )                     \ code gen for no-side-effect type words
    dup >name                     \ xt nfa
    (in/out@)                     \ get the in/out stack effects
    over lits sdepth <= if        \ enough input literals to do at compile time?
      >r swap >r                  \ save the output count & the xt
      dup>r 0 ?do lits spop loop  \ get literals onto the stack
      r> s-reverse                \ reverse the order
      r> execute                  \ execute the word
      r@ s-reverse                \ reverse the order
      r> 0 ?do lits spush loop    \ push outputs back on literal stack
      exit
    then 2drop                    \ drop the in/out

    lits>0? if                    \ might be a single literal operation
      >r opt-lit-table1           \ save xt on rstack, the table
      begin dup @ dup             \ fetch the entry
      while
        r@ = if                   \ if a match
          r> drop                 \ no longer need the xt
          lits spop sync-code     \ pop the literal, sync the code
          swap cell+ perform      \ get the xt from table, go do
          exit                    \ and finish
        then
        2 cells+                  \ next entry
      repeat drop                 \ otherwise just drop it
    then
    inline, ;                     \ else just inline it

code-here
\ all the code after this is temporary, and gets
\ thrown away. Don't use names

:noname    ( -- )                 \ set nseopt as the optimiser for specified xts
    ['] nseopt >r                 \ the optimisation code to run
    opt-lit-table1                \ the table
    begin dup @ dup               \ fetch the entry
    while
      r@ swap compiles
      2 cells+                    \ next entry
    repeat r>drop 2drop ; execute \ do it now
    
code-here - code-allot

only forth definitions


\s


: cmp-stk[-1],eax ( -- ) macro[ cmp    stk[-1] eax  ]macro ;
: cmp-immed  ( n )       macro[ ?dup if >r cmp eax r>
                             else test eax eax then ]macro ;
: jne-mark2     ( -- )      macro[ jne  long 0                  ]macro mark> 1 ;

: optof    ( xt -- ) \ optimise "case n of ... endof"
    drop 1+ >r
    lits>0? if 
      lits spop sync-code
      cmp-immed                  \ literal =
    else
      sync-code
      cmp-stk[-1],eax            \ over = if drop
      macro[ mov     eax { ebp }
             lea     ebp { 4 ebp } ]macro
      ( can't use postpone drop, as it mods flags )
    then
    jne-mark2
    postpone drop r> ;

' optof compiles-for of

((
create opt-lit-table2
\ Entries of the format ( n [ m ] -- x ) where m or n and m can be literals.
\ First entry is for 1 literal, 2nd for 2. For 0 literals, just inline.
\ Side effects not allowed

  ' @      , opt@-1   , opt@-2   ,
  ' c@     , optc@-1  , optc@-2  ,
  ' !      , opt!-1   , opt!-2   ,
  ' +!     , opt+!-1  , opt+!-2  ,
  ' c!     , optc!-1  , optc!-2  ,
    0      ,
))
