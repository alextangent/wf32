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

\ ----------------------- Local values support -------------------------------

\ Restrictions
\   DO NOT mix LOCALS| ... | and {: ... :} together
\   DO NOT declare locals inside control structures
\ Extensions
\   Recognises >r r> etc and when generating the code to fetch/set a local
\
\ Syntax is (where [ and ] represent meta-symbols for optional parameters)
\   {: [a b c] [ | [d e]] [-- [comments]] :}
\ The following are valid examples;
\   {: :}   {: a :}   {: a -- :}   {: a -- comment :}
\   {: a b | c :}     {: | c :}    {: a | c -- comment :}
\
\ Local variables can have any name except | -- or :}
\ Avoid local names that end in “:”, “[”, “^” or that are a 
\ single non-alphabetic character
\ NOTE: {: a -<name>- :} declares two locals; it doesn't indicate a parsing word
\       {: a -- -<name>- :} is the only correct method
\
\ localsgen, generates the locals header based on several pieces of info
\ collected during {: ... :} or LOCALS| ... | parsing. 
\ localadj is defined in the kernel; adjustment for word that use rstack
\ localtot        \ number of locals in this word
\ localdcl        \ number of locals in this {: :} section
\ localstk        \ number of locals initialised from stack in this {: :} section
\ localinc        \ 1 if counting localstk
\ localord        \ direction; 0 = {: :} , -1 = LOCALS| |

\ Generation of the code is split into several parts
\ : foo ... {: a | b :} ... {: c | d :} ... ;
\ 1         2  X 3 X 4      2  X 3 X 4      5
\ STEP 1 Initialise, done by kernel defined defer LOCALS-INIT
\     localtot <-- 0
\ STEP 2 {: start locals declaration
\     localdcl <-- 0
\     localstk <-- 0
\     localinc <-- 1
\ STEP X declare local based on localtot
\     localstk <-- localstk + localinc
\     localdcl <-- localdcl + 1
\     localtot <-- localtot + 1
\ STEP 3 | end of declared
\     localinc <-- 0
\ STEP 4 :} ends locals declaration
\     generate localstk pushes from stack to rstack
\ STEP 5 ; end of word
\     adjust rstack by localdcl
\
\ (Step 2)
\   For each initialised local , the value is taken from the stack and pushed
\   on the rstack. (There's a bit of code to handle top of stack because
\   it's in eax.)
\ (Step 3)
\   EBP and TOS are adjusted to account for any stack to rstack movement.
\
\ Nothing is generated for {: :} (null locals). Step 2 is omitted if there are
\ no initialised locals as in {: | a b :}. Step 1 is omitted if there are no
\ uninitialised locals as in {: a b :}.
\
\ The example {: a b c | d -- f :} would generate
\
\ ( step 2b)    push    eax             \ move across c
\               push    { ebp }         \ then b
\               push    { cell ebp }         \ then a
\               sub     esp 4           \ allow for d
\ ( step 3)     mov     eax { 2 cells ebp }    \ adjust tos
\               add     ebp 12       \ ebp accounts for d
\
\ On exit,
\               add     esp 16
\
\ For the LOCALS| c b a | syntax, the generation is
\
\ ( step 2a)    push    { cell ebp }         \ move across a
\               push    { ebp }         \ then b
\               push    eax             \ then c
\ ( step 3)     mov     eax { 2 cells ebp }    \ adjust tos
\               add     ebp 12       \ and ebp
\
\ On exit,
\               add     esp 12

only forth definitions

1 #vocabulary locals            \ locals vocab
' (common-header)               \ build head in locals area & voc
  ' locals >body voc.head !     \ set as locals vocabulary header create

also hidden definitions

create ldp  0 , 0 , 0 , dp-link link, ," *loc-d"
create mdp  0 , 0 , 0 , dp-link link, ," *loc-c"  

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

\ IN-LOC is used to switch HERE ALLOT , W, etc to point
\ to the locals data areas. Public because IMAGEMAN uses it
: in-loc         ( -- ) ldp mdp  set-section ;

:noname  2 +to localadj [ ' ?do   >comp @ ] literal execute ; compiles-for ?do
:noname  2 +to localadj [ ' do    >comp @ ] literal execute ; compiles-for do
:noname -2 +to localadj [ ' loop  >comp @ ] literal execute ; compiles-for loop
:noname -2 +to localadj [ ' +loop >comp @ ] literal execute ; compiles-for +loop
:noname -2 +to localadj [ ' -loop >comp @ ] literal execute ; compiles-for -loop
:noname  1 +to localadj [ ' [:    >comp @ ] literal execute ; compiles-for [:
:noname -1 +to localadj [ ' ;]    >comp @ ] literal execute ; compiles-for ;]
:noname ( n -- r: n )      1 +to localadj inline, ; dup compiles-for >r  compiles-for dup>r
:noname ( n -- r: n )     -1 +to localadj inline, ; dup compiles-for r>  compiles-for r>drop
:noname ( n m -- r: n m )  2 +to localadj inline, ;     compiles-for 2>r
:noname ( r: n m -- n m ) -2 +to localadj inline, ; dup compiles-for 2r> compiles-for 2rdrop

: eax>r   (  -- )       macro[ push    eax             ]macro       ;
: {n+ebp}>r ( n -- ) >r macro[ push    { r@ ebp }      ]macro rdrop ;
: @{n+ebp}  ( n -- ) >r macro[ mov     eax { r@ ebp }  ]macro rdrop ;
: localn& ( n -- )   >r macro[ lea     eax { r@ esp }  ]macro rdrop ;
: localn@ ( n -- )   >r macro[ mov     eax { r@ esp }  ]macro rdrop ;
: add-esp,n ( n -- ) >r macro[ add     esp r@          ]macro rdrop ;

create (localvars)
    0 , \ localtot number of declared locals in this word
    0 , \ localdcl number of declared locals in this {: :} section
    0 , \ localstk number of undeclared locals in this {: :} section
    0 , \ localinc increment
    0 , \ localord direction; 0 = {: :} , -1 = LOCALS| |
here (localvars) - equ (localvars-len)

\ localadj is defined in the kernel; adjustment for word that use rstack
0   value localtot        \ number of declared locals in this word
0   value localdcl        \ number of declared locals in this {: :} section
0   value localstk        \ number of undeclared locals in this {: :} section
0   value localinc        \ increment
0   value localord        \ direction; 0 = {: :} , -1 = LOCALS| |

:noname ( -- )                           \ STEP 1 in diagram above
    sync-code
    [ ldp data.origin ] literal @ ldp !  \ reset origin
    [ mdp data.origin ] literal @ mdp !  \ reset origin
    [ ' locals >body voc.#0 ] literal off \ clean thread in vocabulary
     0 to localtot                       \ clear locals stack counter
     0 to localstk ;                     \     localstk <-- 0
  is locals-init                         \ in kernel

:noname ( -- )                           \ STEP 5 in diagram above
    localstk ?dup if cells add-esp,n then ; \ adjust for locals
 is locals-exit                         \ in kernel

:noname ( addr len -- nfa | 0)        \ as in gforth, return name token
    state @ localtot and if           \ only search if any locals and compiling
      also locals
      (find-context)
      previous                        \ temp code; see above
    else (find-context) then ;
  is find-name                        \ in kernel

defer localn ' localn@ is localn \ fetch is the default

:noname ( -<value>- -- addr )                      \ compile time
    ' dup itc,
    dup >name tfa@ tloc = if
      ['] localn& is localn
      execute
      ['] localn@ is localn
    else (comp-vcon) then ;                     \ compile as value literal
  is (&of)                                      \ in kernel, supports TO <local>
            
: (local-gen) ( -- )                                \ generate locals code
   ( step 1)    localdcl ?dup if                    \ any declared?
                  1- cells dup                      \ convert into cells-1
                  localord if                       \ order; LOCALS| or {
    ( step 2a)      eax>r                           \ LOCAL| |last one; push TOS
                    0 ?do i {n+ebp}>r cell +loop    \ store the offset
                  else
    ( step 2b)      dup cell- swap 0max             \ {: :}
                    0 ?do dup i - {n+ebp}>r cell +loop drop \ store the offset
                    eax>r                           \ first one; push TOS
                  then
   ( step 3)      dup @{n+ebp}                      \ n-1 PICK
                  cell+ add-ebp,n                   \ n cells drop
                then ;

: (local)       ( addr len -- )           \ create name in locals vocab
    dup if                                \ STEP X declare local based on localtot
      localinc +to localstk               \     localstk <-- localstk + localinc
      1 +to localdcl                      \     localdcl <-- localdcl + 1       
      1 +to localtot                      \     localtot <-- localtot + 1
      2>r
      latestxt @ last @ last-link @ ofa @      \ save last (we wipe out)
      in-loc                             \ build in locals area
      [ ' locals >body ] literal swap-current       \ save current
      2r>                                \  name
      [:                                 \ when local name is compiled
        create localstk negate ,        \ declare time: negative offset
          does>
            postpone dup                \ dup
            @ localstk +                \ refer time: add in any new declares
            localadj +                  \ adjust for do loop etc
            cells localn                \ generate fetch or store
      ;] execute-parsing
      immediate tloc tfa!               \ mark as immediate & a local
      set-current
      in-app ofa ! last-link ! last ! latestxt ! \ restore pointers
    else 2drop \ :} sync-code                \ go on to create code
    then ;

only forth definitions also hidden

\ -------------------- Locals Parameter Compiler ---------------------------

: local-term? ( addr len -- flag )
     2dup s" |"  str= 4 and >r           \ as in locals| [...] | ...
     2dup s" --" str= 2 and >r           \ as in { [...] -- ...
          s" :}" str= 1 and 2r> or or    \ as in { [...] } ...
     ;

: dcl-local ( -- type )                  \ parse locals until | -- or :}
     begin definite-word                 \ get next word
       2dup local-term? dup 0=           \ as in { [...] } ...
     while drop 
     2dup
       (local) 
     cr localtot .
     localdcl .
     localstk .
     localadj .
     type
       repeat
     nip nip ;

: {: ( -- )                              \ STEP 2 {: start locals declaration
   only-compiles>
     0 to localdcl                       \     localdcl <-- 0
\     0 to localstk                       \     localstk <-- 0
     1 to localinc                       \     localinc <-- 1
     sync-code
     dcl-local                           \ parse defined locals
     dup 4 = if                          \ STEP 3 | end of declared
       0 to localinc                     \     localinc <-- 0
       drop
       dcl-local
     then
     2 = if                              \ -- comment
       s" :}" _))                        \ skip until terminator
     then
     0 0 (local)                         \ generate locals code
     ;                             

: locals| ( -- ) \ ans standard locals
    only-compiles>
     -1 to localord                      \ reversed stack order
     0 to localdcl                       \     localdcl <-- 0
     0 to localstk                       \     localstk <-- 0
     1 to localinc                       \     localinc <-- 1
     sync-code
     begin definite-word                 \ get next word
       2dup s" |"  str= not              \ as in locals| [...] | ...
     while (local) repeat 2drop
     0 0 (local)                         \ generate locals code
     ;

: local ( -<name>- -- ) only-compiles> definite-word (local) 0 0 (local) ;

only forth definitions

: foo 10 20 {: a b c | d e -- f g :} dup {: x y :} >r {: z :} r> ;
see foo
also locals words
: bar {: | f g -- hh :} ;
see bar



