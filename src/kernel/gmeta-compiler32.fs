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
\    Frankly, it needs rewritten. Arcane incantaions doesn't do it justice.
\
\ ------------------------------------------------------------------------

cr .( Loading META Compiler ) sourcefilename type

\ ======================================================================
\ Define the wordlists that are used in the metacompiler

\ Words defined in META support the meta compile; they're here to simplify
\ any collisions with words in the standard forth dictionary

\ Words defined in TARGET we need to refer to in the meta-compiler, but they
\ aren't yet compiled. They're not like FORWARD words, which are used in
\ the kernel before they're defined.

\ primitive words are immediates, like POSTPONE IS TO that are inside colon
\ definitions. They need to, for example, parse, and can't be compiled
\ as per usual.

\ FORWARD words are used in the kernel before they are defined.

vocabulary target       \ target words
vocabulary primitive    \ special compiling words (immediates)
vocabulary forward      \ forward references

: >wordlist  ( voc-cfa -- wordlist )  >body ;  ( win32forth )

' meta        >wordlist constant meta-wordlist          ( *sysdep* )
' target      >wordlist constant target-wordlist        ( *sysdep* )
' forward     >wordlist constant forward-wordlist       ( *sysdep* )
' primitive   >wordlist constant primitive-wordlist     ( *sysdep* )

\ We will use the following search orders:

: in-forth        only forth definitions  ;
: in-meta         only forth also vimage also meta definitions  ;
: in-primitive    only forward also target also primitive ;

: m:   ( -- "name" ) \ add word to meta wordlist
    also meta definitions previous : definitions ;
: p:   ( -- "name" ) \ add word to primitive wordlist (it's always immediate)
    also primitive definitions previous : immediate definitions ;

\ helper for debugging; invoke recover to get vocabs back to somthing reasonable
m: recover in-forth ; immediate

in-meta

\ ======================================================================
\ Memory Access Words

\ Where building words go: (for example)
\ COMPILE,       into  IMAGE-CODEPTR
\        ,       into  IMAGE-APPPTR
\    SYS-,       into  IMAGE-SYSPTR

\ -------------------- Deferred app space words ------------------------
\ Allocate a chunk of space for each of the sections we're building

$10000 dup malloc to image-codeptr image-codeptr swap erase \ where code is built
$10000 dup malloc to image-appptr  image-appptr  swap erase \ where target app is built
$10000 dup malloc to image-sysptr  image-sysptr  swap erase \ where target heads are built

image-origin image-csep + image-codeptr - constant tcode-base  \ target data base
image-origin image-asep + image-appptr  - constant tapp-base   \ target dictionary base
image-origin image-ssep + image-sysptr  - constant tsys-base   \ target header base

\ set the allocation word dp sections (see kernel for layout)
create tcode-dp image-codeptr , image-codeptr , image-codeptr $10000 + , 0 , ," tcode-dp" \ code
create tapp-dp  image-appptr  , image-appptr  , image-appptr  $10000 + , 0 , ," tapp-dp"  \ app
create tsys-dp  image-sysptr  , image-sysptr  , image-sysptr  $10000 + , 0 , ," tsys-dp"  \ system      

: porig ( n -- ) tab image-origin + h.8 ;
: psize ( n -- ) 13 u,.r ;
                 
: image-stats    ( -- )                                  \ image statistics
    tab-size 12 to tab-size
    cr
    cr ." Load point " IMAGE-ORIGIN $.
    cr ." Entry point +" IMAGE-ENTRY $.
    cr ." Section" tab ."  Address" tab 5 spaces  ." Used" tab  ."  Allocated"
    
    underline
    cr ." .code" image-csep porig image-cactual psize image-csize psize
    cr ." .app"  image-asep porig image-aactual psize image-asize psize
    cr ." .sys"  image-ssep porig image-sactual psize image-ssize psize
    underline
    
    cr ." Total" tab 8 spaces image-aactual image-sactual image-cactual + + psize
    cr to tab-size
    ;

: in-app-t       ( -- ) tapp-dp tcode-dp  set-section ;
: in-sys-t       ( -- ) tsys-dp tcode-dp  set-section ;
: in-code-t      ( -- ) tcode-dp tcode-dp  set-section ;
: in-sys-t? dp tsys-dp = ;

' in-app-t alias >tapp  \ select app dict, save prev dict
' in-sys-t alias >tsys  \ select sys dict, save prev dict
' in-code-t alias >tcode  \ select code dict, save prev dict

' in-prev alias tapp>
' in-prev alias tsys>
' in-prev alias tcode>

: tapp-there  ( taddr -- addr )    tapp-base -   ;
: tapp-@      ( taddr -- n )       tapp-there @  ;
: tapp-c@     ( taddr -- n )       tapp-there c@ ;
: tapp-c!     ( char taddr -- )    tapp-there c! ;
: tapp-w!     ( word taddr -- )    tapp-there w! ;
: tapp-!      ( n taddr -- )       tapp-there !  ;
: tapp-here   ( -- taddr )         >tapp here  tapp-base + tapp> ;
: tapp-allot  ( n -- )             >tapp allot tapp> ;
: tapp-c,     ( char -- )          >tapp c,    tapp> ;
: tapp-w,     ( w -- )             >tapp w,    tapp> ;
: tapp-,      ( n -- )             >tapp ,     tapp> ;
: tapp-align                       >tapp align tapp> ;
: tapp-s,     ( addr len -- )      0 ?do count tapp-c, loop drop ;

: tsys-there  ( taddr -- addr )    tsys-base -   ;
: tsys-@      ( taddr -- n )       tsys-there @  ;
: tsys-c@     ( taddr -- n )       tsys-there c@ ;
: tsys-c!     ( char taddr -- )    tsys-there c! ;
: tsys-w!     ( word taddr -- )    tsys-there w! ;
: tsys-!      ( n taddr -- )       tsys-there !  ;
: tsys-here   ( -- taddr )         >tsys here  tsys-base + tsys> ;
: tsys-allot  ( n -- )             >tsys allot tsys> ;
: tsys-c,     ( char -- )          >tsys c,    tsys> ;
: tsys-w,     ( w -- )             >tsys w,    tsys> ;
: tsys-,      ( n -- )             >tsys ,     tsys> ;
: tsys-align                       >tsys align tsys> ;
: tsys-s,     ( addr len -- )      0 ?do count tsys-c, loop drop ;

: tcode-there ( taddr -- addr )    tcode-base -   ;
: tcode-c@    ( taddr -- char )    tcode-there c@ ;
: tcode-w@    ( taddr -- word )    tcode-there w@ ;
: tcode-@     ( taddr -- n )       tcode-there @  ;
: tcode-c!    ( char taddr -- )    tcode-there c! ;
: tcode-w!    ( word taddr -- )    tcode-there w! ;
: tcode-!     ( n taddr -- )       tcode-there !  ;
: tcode-here  ( -- taddr )         >tcode here  tcode-base + tcode> ;
: tcode-allot ( n -- )             >tcode allot tcode> ;
: tcode-c,    ( char -- )          >tcode c,    tcode> ;
: tcode-w,    ( w -- )             >tcode w,    tcode> ;
: tcode-,     ( n -- )             >tcode ,     tcode> ;
: tcode-s,    ( addr len -- )      $90 ?do count tcode-c, loop drop ;
\ : tcode-align                      >tcode
\                                         cell allot                    \ align to 16byte boundary
\                                         here dup 16 naligned
\                                         swap - cell- allot tcode> ;
\ : tcode-align                      >tcode ( align ) tcode> ;
: tcode-align ; immediate

defer t-@
defer t-c@
defer t-!
defer t-here
defer t-allot
defer t-c,
defer t-c!
defer t-w,
defer t-,
defer t-s,
defer t-align

: in-app        ( -- )
    ['] tapp-@       is t-@
    ['] tapp-c@      is t-c@
    ['] tapp-!       is t-!
    ['] tapp-here    is t-here
    ['] tapp-allot   is t-allot
    ['] tapp-c,      is t-c,
    ['] tapp-c!      is t-c!
    ['] tapp-w,      is t-w,
    ['] tapp-,       is t-,
    ['] tapp-s,      is t-s,
    ['] tapp-align   is t-align
    [ also assembler asm-hidden ]
    ['] tcode-here   is asm-here
    ['] tcode-c,     is asm-c,
    ['] tcode-w,     is asm-w,
    ['] tcode-,      is asm-d,
    ['] tcode-c@     is asm-b@
    ['] tcode-c!     is asm-c!
    ['] tcode-w@     is asm-w@
    ['] tcode-w!     is asm-w!
    ['] tcode-@      is asm-d@
    ['] tcode-!      is asm-d!
    ['] tcode-align  is asm-align
    [ previous ]
    ;

: in-sys        ( -- )
    ['] tsys-@       is t-@
    ['] tsys-c@      is t-c@
    ['] tsys-!       is t-!
    ['] tsys-here    is t-here
    ['] tsys-allot   is t-allot
    ['] tsys-c,      is t-c,
    ['] tsys-c!      is t-c!
    ['] tsys-w,      is t-w,
    ['] tsys-,       is t-,
    ['] tsys-s,      is t-s,
    ['] tsys-align   is t-align
    ;


\ this is a meta-body
: t>body 1+ tcode-@ ;   

\ ======================================================================
\ Modify assembler to place code into target

variable in-code?
     in-code? off    \ we're building in target

20 stack acs

acs -stack

\ -------------------------------------------------------
\ Header structure, load into meta vocabulary
 
include src/kernel/gstructs.fs \ meta load the kernel structures

\ -----------------------------------------------------------------------------------
\ Meta Compiler Forward References

\ Forward references are important in a one pass compiler, so there's support here
\ to make it as automatic as possible.
\
\ The FWD structure can be used for other references that need to be resolved later,
\ by using the FWD.ACTION field. Currently supported are:
\   CALL type references with a relative address
\      generated by not-found and FORWARD: <name>
\   CALL type references with an absolute indirect address (for DEFER)
\     no support words yet
\   Absolute address references, generally for literals or constant addresses
\     no support words yet
\   Relative type references; like call types, but not associated with a call
\     no support words yet
\
\ At the end of compilation, we attempt auto-resolution. If the forward reference is not
\ resolved by using  ' <NAME1> RESOLVES <NAME2>  in the kernel code, then the metacompiler
\ looks for matching names in the TARGET dictionary. If they're the same, it automatically
\ resolves them. RESOLVES only needs to be used if the resolution name is not the same
\ as the target name (an unusual case).
\
\ Unresolved forward pointers are maintained for each FWD structure in a RES linked
\ list. Each points to a cell in the code that's been generated with a dummy address
\ that needs to be resolved before meta-compilation ends. (An implementation note: it
\ is possible to link through the generated code and avoid the RES block. This has
\ an assumption that each forward address is a 32bit structure, something that may
\ not be true in future versions.)
\
\ When an not-found word is found or if it is explicitly created with FORWARD:
\ we create a word the first time through with the same name, in the FORWARD vocab,
\ using the FWD structure. This is maintained on a linked list of all FWD structures.
\
\ Next time the word is executed (because it's no longer not-found) it generates a call
\ and a RES block linked in to the FWD structure if the address is still not known.
\ If the word is subsequently resolved by RESOLVES, all previous addresses are
\ fixed up with the correct address, and this invocation and subsequent generate correct
\ code that requires no fixup, and no RES block is generated.
\
\ Don't abuse this forward feature! Only words that can be called (as in call ' name )
\ are supported. As this is a one-pass compiler, we don't know how to generate any
\ other code.

variable last-h         \ target address of count byte
0 value  ofa-h          \ target address of ofa
0 value mtail-call

\ ======================================================================
\ Force compilation of target words. We need to reference the
\ special runtime target words like LIT and BRANCH before they are defined,
\ so we store the name of the word and look it up when we need it. Hopefully
\ they will have been defined by then. [TARGET] is for target primitives.

begin-structure tgt%  ( target words, defined in TARGET )
    field: tgt.xt
    field: tgt.compile
    field: tgt.head
end-structure

: find-wid  ( addr len wid -- xt ) \ find the next word in a single wordlist only
    3dup >r 2>r search-wordlist if
      r>drop r>drop r>drop
    else
      r> 2r> -rot
      cr ." META: failed search in "
         case
           target-wordlist      of ." TARGET"      endof
           primitive-wordlist   of ." PRIMITIVE"   endof
           forward-wordlist     of ." FORWARD"     endof
         endcase
         ."  wordlist for word: " type
      true abort" META: word not found"
    then ;

\ ======================================================================
\ Define primitive words, which behave like forth immediate words.

: [primitive] ( -- "name" )            \ compile a primitive word
    parse-name primitive-wordlist find-wid compile, ; immediate

: find-wid.xt  ( addr len -- xt )
    target-wordlist find-wid ;

: @tgt.xt ( xt -- tgt-xt ) ( fetch target xt given xt )
    >body tgt.xt @ ;

: 't ( <-name-> -- target-xt ) \ find target address from name
    parse-name find-wid.xt @tgt.xt ; \ need this because execute compiles the target word

: [target-xt] ( <-name-> -- xt )
    parse-name postpone sliteral
    postpone find-wid.xt postpone @tgt.xt ; immediate

: [target] ( <-name-> -- ) ( compiles the target xt )
    parse-name postpone sliteral
    postpone find-wid.xt postpone execute ; immediate
    
: target-call, ( tgt. -- )                       \ generate a call to resolved xt
    tgt.xt @ >r                                  \ get xt
    macro[ call r@ a; ]macro
    r>drop tcode-here to mtail-call ;            \ possible tail call

: target-define   ( xt -- )  \ add word to target wordlist
    also target definitions previous
    create                                  \ name in target wordlist
    definitions
      , ( xt )                               \ tgt.xt
      ['] target-call, ,                     \ tgt.compile the default for do-target
      last-h @ tsys-there head.nt ,          \ last header (true address)
    does> dup tgt.compile @ execute ;        \ what target words do

begin-structure fwd%
  field:  fwd.link         \ link to next fwd
  field:  fwd.target       \ target address
  field:  fwd.action       \ target action xt
  field:  fwd.res          \ points to res blocks
  cfield: fwd.name         \ name
end-structure

begin-structure res%       \ resolve blocks (points to addresses to be resolved)
  field:  res.link         \ next res for this fwd
  field:  res.modify       \ ptr to area to modify
end-structure

\ generate and actions for various type references
: fwd-call,    ( xt -- modify )           \ generate a call, return address to modify
        asm[ >r call r> a;]               \ relative address call
        tcode-here cell- ;

: res-coderel  ( target res -- )          \ store relative modify in target
        res.modify @                      \ fetch the address to modify
        tuck - cell- swap tcode-! ;       \ relative address for a call forward ref

\ resolving forward references, any type

: fwd-res   ( fwd -- )                    \ resolve all the res blocks on this fwd
        dup fwd.action @ >r               \ save action on rstack
        dup fwd.target @ swap             \ get the target
        fwd.res                           \ run thru fwd.res
        begin @ dup
        while
          2dup r@ execute                 \ execute the action to resolve
        repeat 2drop r>drop ;

: (resolves)  ( target fwd -- )           \ resolve fwd with target address
        dup fwd.target @ abort" META: forward ref already resolved" \ oops...
        tuck fwd.target ! fwd-res ;       \ otherwise resolve it

\ building forward references

: res-build ( fwd -- )
        dup fwd.target @ fwd-call,        \ generate a call to the target address
        swap fwd.res link, , ;            \ res.link link from fwd.res, res.modify

variable link-fwd 0 link-fwd !            \ linked list of forward words

: fwd-create ( "name" -- addr len )       \ build a forward structure
        forward-wordlist swap-current     \ definition in forward wordlist
        >in @ parse-name 2>r >in !        \ parse the name, and reset for create
        create                            \ create the forward ref
        set-current                       \ back to original vocab
        2r>                               \ the name
        cr ." fwd ref at line: "sourceline# . 2dup type 
        ;

: fwd-data   ( addr len -- fwd )          \ build a forward structure
        here >r                           \ start of fwd structure
        link-fwd link,                    \ link to previous
        0 ,                               \ target addr
        0 ,                               \ target action to resolve, caller must set
        0 ,                               \ res blocks pointer
        ", r>                             \ the name
        ;

: fwd-build    ( "name" -- fwd )          \ build a forward structure
        fwd-create
        fwd-data
        ;

\ defining words for call type references

0 value res
0 value unres

: fwd-fixup \ 0 0 {: res unres -- unres :}    \ resolve fwd references
        cr ." Fixup forward refs... "
        link-fwd
        begin @ dup                       \ run through the fwd structures
        while
          dup fwd.target @ 0= if          \ if the target address isn't known
            dup fwd.name count
            target-wordlist search-wordlist if \ search for the same name
              @tgt.xt over (resolves)     \ get the xt, and run the resolver
              1 +to res
            else
              dup cr ." META: -> " fwd.name count type                 
              ." <- forward reference not found"
              1 +to unres                 \ for which you need to use RESOLVES
            then
          then
        repeat drop
        unres if cr then
        res . ." resolved, " unres . ." unresolved"
        unres ;

: not-found ( "name" -- )                 \ not-found words create automatic forward reference
          fwd-build                       \ build the fwd
          ['] res-coderel over fwd.action ! \ set action for call type words
          res-build                       \ build a call and res
        does> ( -- forward )              \ second & subsequent ref
          res-build                       \ build a call and res
          ;

\ -------------------------------------------------------
\ resolution of cts
\ points to compile, etc
\ these are forward references where it's impossible to use the [target-xt] word before defined

variable ct-link ct-link off          \ list of cts
-2 constant ct-compile,
-3 constant ct-call,                  \ comp values, gets substituted
-4 constant ct-inline,
-5 constant ct-(comp-val)
-6 constant ct-(comp-def)

: ctlink>ptr ( link -- header )       \ point at the HEADER nfa
        cell+ @ ;

: ct-replace  ( link -- )                \ update dummy ct tokens
        cell+ @ dup tsys-@                        \ dummy xt
        case 
             ct-compile,    of  [target-xt] compile,    swap tsys-! endof
             ct-inline,     of  [target-xt] inline,     swap tsys-! endof
             ct-call,       of  [target-xt] xt-call,    swap tsys-! endof
             ct-(comp-val)  of  [target-xt] (comp-val)  swap tsys-! endof
             ct-(comp-def)  of  [target-xt] (comp-def)  swap tsys-! endof
          drop
        endcase
        ;                                  

: ct-fixup  ( -- )                    \ resolve cts
        cr ." Fixup compile tokens..."
        ct-link @ ['] ct-replace list-apply
        ;

variable val-link  val-link off \ list of jumps generated in value xts
variable def-link  def-link off \ list of jumps generated in defer xts
variable na-link   na-link  off \ list of jumps generated in :noname

: na-replace  ( ct -- )               \ update dummy na tokens
        cell+ @
        [target-xt] anonymous t>body
        over - cell-                     \ make relative to this address                
        swap tcode-! ;

: val-replace
        cell+ @ [target-xt] (exec-val)
        over - cell-                     \ make relative to this address
        swap tcode-! ;

: def-replace
        cell+ @ [target-xt] (exec-def)
        over - cell-                     \ make relative to this address
        swap tcode-! ;

: oth-fixup
        cr ." Fixup literal, :noname tokens, value/defer jumps..."
        val-link @ ['] val-replace list-apply
        def-link @ ['] def-replace list-apply
        na-link  @ ['] na-replace  list-apply
        ;

\ resolve all types, issue error messages if resolution fails

library winmm.dll
3 import: PlaySound

\ ----------------------- Include primitives -----------------------------

0 value error

: .fixup \ 0 {: error -- f :}
        ct-fixup
        oth-fixup
        fwd-fixup to error
        cr depth 0<> if
          1 +to error
          ." *** Stack was not clean on exit ***" cr .s cr
        else
          ." Stack clean on exit"
        then
        cr error . ." errors during meta-compile"
        error if 
          SND_ALIAS_ID SND_ASYNC + 0 SND_ALIAS_SYSTEMHAND PlaySound
        then
        error
        ;        

\ ======================================================================
\ Create Headers in Target IMAGE-APPPTR.

defer meta-hash also forth ' thread-hash previous is meta-hash

: tgt-vocs ( n "name" -- )
       create here swap dup cells allot cells erase ;

#threads   tgt-vocs forth-threads
#fthreads  tgt-vocs files-threads
#hthreads  tgt-vocs hidden-threads
#rthreads  tgt-vocs root-threads
#ithreads  tgt-vocs imports-threads   

: (thread)     ( addr len -- 'thread )      \ get forth vocab thread address
                #threads meta-hash forth-threads +  ;
: (fthread)     ( addr len -- 'thread )      \ get procs vocab thread address
                #fthreads meta-hash files-threads +  ;
: (hthread)     ( addr len -- 'thread )      \ get procs vocab thread address
                #hthreads meta-hash hidden-threads +  ;
: (rthread)     ( addr len -- 'thread )      \ get procs vocab thread address
                #rthreads meta-hash root-threads +  ;
: (ithread)     ( addr len -- 'thread )      \ get procs vocab thread address
                #ithreads meta-hash imports-threads +  ;

defer thread

false value in-hidden?

-1 value case-voc \ case of vocab

: voc-forth      ( -- ) -1 to case-voc ['] (thread)  is thread false to in-hidden? ;
: voc-imports    ( -- )  0 to case-voc ['] (ithread) is thread false to in-hidden? ;
: voc-files      ( -- ) -1 to case-voc ['] (fthread) is thread false to in-hidden? ;
: voc-root       ( -- ) -1 to case-voc ['] (rthread) is thread false to in-hidden? ;
: voc-hidden     ( -- ) -1 to case-voc ['] (hthread) is thread true  to in-hidden? ;
' voc-hidden alias internal
' voc-forth  alias external

external                        \ set as threads default

\ -------------------------------------------------------

: (header)
        2dup thread dup @ tsys-here  rot !  tsys-, \ lfa
        $e9         tcode-c,                      \ jmp
        tsys-here tcode-here - cell- tcode-,       \ offset to the ct pair
      ct-link link, tsys-here ,                  \ link & address
        ct-compile, tsys-,                         \ ct of compile,
        tcode-here  tsys-,                         \ xt-ptr
      ct-link link, tsys-here ,                  \ link & address
        ct-call,    tsys-,                         \ comp field
        0 tsys-,                                        \ recognizer
        sourceline# tsys-w,                        \ vfa
        0 tsys-w,                                  \ ofa
        -1 tsys-c, -1 tsys-c,                      \ ste
        0 tsys-c,                                  \ typeflag
        tsys-here last-h !                         \ remember nfa
        dup tsys-c, tsys-s, 0 tsys-c,              \ count byte nfa  name string
        tsys-align ;

: header   ( -- )
        >in @                                        \ so we can reparse
        tapp-align
        parse-name buf-allot dup>r place r>
        case-voc if lowercase then count
        (header)
        >in ! ;
        
: t-tfa! ( type -- )                 \ set the type
         last-h @ head.tfa tsys-c! ;

' t-tfa! alias tfa!                  \ to support localn

: immediate   ( -- )
        [target-xt] execute last-h @ head.ct tsys-! ;  \ set the ct token to xt

: in/out ( n m -- )
        2dup                                                        
        ste-o ! ste-i !
        last-h @ head.stk dup>r 1+ tsys-c! r> tsys-c! ;

\ ======================================================================
\ generating calls and jumps

: make-copy ( addr len -- )
\            macro[ a; ]macro
\            2dup dump
            tcode-here tcode-there swap dup tcode-allot move ;

: make-inline, ( xt -- )                      \ inline the xt
\            dup cr ." inlining " tgt.head @ head.nt name>string  type
            dup tgt.xt @
            tcode-there swap tgt.head @ head.ofa w@ make-copy ;

: make-tjmp,     ( xt -- )              \ jump to xt on the stack
    asm[ >r jmp r> a;] ;                \ jmp relative

: inline ( -- ) \ set the comp token to inline,
    ct-inline, last-h @ head.comp tsys-!
    ['] make-inline, last @ name>interpret >body tgt.compile ! ;

\ ======================================================================
\ Meta Compiler Create Target IMAGE-APPPTR

: target-alias   ( xt -- )             \ alias support
        header              \ create header in target
        dup target-define
        last-h @ head.xtptr tsys-! ;      \ point at xt

: target-create   ( -- here )
        header               \ create header in target
        in-code? @ if tcode-here else t-here then
        target-define
        ;

: recreate   ( -- )
        >in @ target-create >in ! ;

\ ------------------------------------------------------------------------

\ Define primitives

\ Meta compiler primitives for the STC 32bit version

\ Primitives; specify as
\ p: name <i> <o> in/out macro[
\             <code> ]macro ;

p: exit
    0 0 in/out
    asm[ ret a;] ;

p: literal  ( n -- )                          \ run-time skeleton for literal
    asm[        mov     { -$4 ebp } eax
                sub     ebp $4              a;]
    dup>r case
      0  of  asm[      xor     eax eax   a;]  endof
      -1 of  asm[      or      eax -1    a;]  endof
      ( otherwise )
             asm[      mov     eax r@    a;]
    endcase rdrop ;

p: 2literal ( n m -- )                       \ run-time skeleton for literal
            swap 2>r                        \ save n m
            asm[                            \ macro
                mov     { -4 ebp } eax         \ push eax
                mov     { -8 ebp } dword r>  \ n
                mov     eax r>             \ move m to eax
                sub     ebp $8              \ adjust stack
            a;] ;                \ end macro

\ ----------------------------- Branching ----------------------------------

: ?condition  ( f -- )      true - abort" META: conditionals not paired" ;

: tgt-mark> ( -- addr f ) tcode-here true ;

: tgt->resolve ( addr -- )
    [ also assembler ] a; [ previous ]
    tcode-here over - swap cell- tcode-! ;

: tgt-<resolve ( dest -- ) \ fixup relative jump to dest (backward jump)
    tcode-here - tcode-here cell- tcode-! ;

p: ahead   [target] bra     tgt-mark>       ;
p: if      [target] ?bra    tgt-mark>       ;
p: then    ?condition  tgt->resolve    ;
p: else    [primitive] ahead 2swap [primitive] then ;
p: begin   tgt-mark>       ;
p: again   ?condition   [target] bra   tgt-<resolve  ;
p: until   ?condition   [target] ?bra  tgt-<resolve   ;
p: while   [primitive] if 2swap    ;
p: repeat  [primitive] again [primitive] then        ;

\ ------------------------ Quotations ------------------------------------

p: [:  ( c: -- xt xt' cs1 cs2 ) ( start an anonymous quotation )
      [primitive] ahead
      over swap ;

p: ;]  ( c: xt xt' cs1 cs2 -- ) ( end an anonymous quotation )
      [primitive] exit
      [primitive] then
      [primitive] literal ;

\ --------------------  DO/I/J/LOOP  ---------------------------------

((
p: do ( start lim -- ) over $80000000 + - 2>r postpone begin ;
p: i  ( -- index )     2r@ + ;
p: loop ( -- )         r> 1+ dup >r overflow? until 2r> ; 
))

p: ?do   ( c: -- do-sys )
        ( -- r: n1 n2 )
        0 acs spush
        [target] do-part1
        [target] bra-?do
        tgt-mark> acs spush acs spush  \ for after loop
        [target] do-part2
        [primitive] begin              \ begin for loop
        ;

: (tgt-loop) ( c: do-sys -- )
        ?condition tgt-<resolve
        [target] unloop
        begin acs spop ?dup while
          acs spop [primitive] then repeat
        ;

p: loop  ( c: do-sys -- )
        ( r: n1 n2 -- )
        [target] _loop   \ for begin
        (tgt-loop) ;

p: +loop  ( c: do-sys -- )
        ( r: n1 n2 -- )
        [target] _+loop   \ for begin
        (tgt-loop) ;

p: compiles>  ( -- xt )             \ for alternative compilation
                [primitive] exit      \ exit current definition
                tcode-here             \ this xt
                last-h @ head.comp tsys-!        \ set xt2 token
                ;
                
p: only-compiles> ( -- )
                [target] (comp-only)
                [primitive] compiles>
                [target] drop ;
                                                     
p: break        $cc tcode-c, ;

p: does>
                [target] (;code)                         \ code section
                [primitive] exit                       \ ret: does> code 1 byte after this
                [target] dovar                           \ to fetch create value
                ;

p: ;code
                [target] (;code)                         \ code section
                [primitive] exit                       \ ret: does> code 1 byte after this
                also assembler
                ;

\ Comments while compiling
p: (   postpone (   ;
p: \   postpone \   ;

\ Resolving xts

p: [']       't [primitive] literal   ;

p: postpone  't dup
             ( >ct ) dup cell- tcode-@ +
             tsys-@ [target-xt] execute = if 
               >r macro[ call r@ a; ]macro r>drop
             else 
               [primitive] literal [target] compile,
             then ;

\ Strings
: t-string,     parse"  dup  t-c,  t-s,  0 t-c, ;
: t-sstring,    parse"  tuck t-s,  0 t-c, ;
p: c" t-here t-string,         [primitive] literal ;
p: s" t-here t-sstring,        [primitive] 2literal ;
p: z" t-here t-sstring, drop   [primitive] literal ;

\ Calling imports
p: call ( <-name-> -- ) ( call import ) macro[ call 't ]macro ;


\ ======================================================================
\ Create target assembler words

: ofa-meta  ( -- )
      tcode-here ofa-h -           \ length of code section
      last-h @ head.ofa tsys-w!       \ save it
      ;

' ofa-meta is ofa-calc

: ste-meta    ( -- )                  \ generate adjustment offset
                ste-i @ ste-o @
                2dup or 0< not -rot - cells and \ zero if either -ve
                ?dup if
                  dup 0< if
                    negate >r asm[ sub ebp r> a;]
                  else
                           >r asm[ add ebp r> a;]
                  then
                then ste-reset     \ reset
                ;
                
' ste-meta is ste-adjust  

also assembler definitions

also asm-hidden
: (end-code)
      ofa-meta    \ resolve the optimizer field address
      a; in-meta ?unres ?csp
      in-code? off
      ;
previous

' (end-code) is end-code

previous definitions

: ncode  ( -- )
        tcode-here to ofa-h        \ save code address in ofa
        tpri t-tfa!
        [ assembler asm-hidden ] init-asm
        [ meta ] assembler definitions  csp! 
        ;

: code  ( -- )
        in-code? on
        target-create
        ncode
        ;

: meta-constant ( n <-name-> -- )
            also meta definitions previous
            >in @ swap constant >in !
            definitions ;

: t-ecxaddr-#   ( n -- )                       \ generate a mov ecx, # n
    asm[ >r  mov ecx r> a;] ;

: t-ecxaddr-@   ( n -- )                          \ generate a mov ecx, n
    asm[ >r  mov ecx { r> } a;] ;

: t-ecxaddr-u   ( n -- )                       \ generate a lea ecx, { n ebx }
    ?dup if
       asm[ >r lea  ecx { r> ebx } a;]
    else
       asm[ mov ecx ebx a;]
    then ;

: t-dogen-#     ( xt type-of-name <-name-> -- )                   \ generate do code
            2>r code 2r>
            t-tfa!                              \ type
            t-align t-here t-ecxaddr-# make-tjmp, \ name -> mov ecx, # here | jmp xt
            asm[ end-code a;]
            ofa-meta ;

: t-dogen-#c    ( n xt type-of-name <-name-> -- )                   \ generate do code
            2>r >r code r> 2r>
            t-tfa!                              \ type
            swap t-ecxaddr-# make-tjmp, \ name -> mov ecx, # here | jmp xt
            asm[ end-code a;]
            ofa-meta ;

: t-dogen-u    ( n xt type-of-name <-name-> -- )                   \ generate do code
            2>r >r code r> 2r>
            t-tfa!                              \ type
            swap t-ecxaddr-u make-tjmp, \ name -> lea ecx, n [ebx] | jmp xt
            asm[ end-code a;]
            ofa-meta ;

\ ======================================================================
\ Define target vocabularies (uh, wordlists)

variable voc-link-t

: voc-threads ( "name" -- )
        't t>body voc.#0 ;        \ the ptr to the vocab threads

: #vocabulary  ( threads "name" -- )          \ lexicon vocabulary
        in-sys
        t-align
        t-here dup>r
        voc-link-T @ t-,   voc-link-T !
        0 t-,                        \ xt
        [target-xt] (std-header) t-,             \ aren't defined at this point
        [target-xt] (std-search) t-,             \ aren't defined at this point
        [target-xt] (std-iter) t-,               \ aren't defined at this point
        DUP t-,
        CELLS t-allot
        code
        tcode-here r@ voc.xt t-!          \ set the xt
        asm[
             mov     ecx r>
             jmp     [target-xt] dovoc     
             end-code
             ofa-meta
        a;]
        in-app
        0 0 in/out
        tvoc t-tfa!
        ;

: VOCABULARY   ( "name" -- )
        #threads #vocabulary ;
        
: case-asis
        in-sys
        [target-xt] (asis-header) voc-link-T @ voc.head t-!
        [target-xt] (asis-search) voc-link-T @ voc.srch t-!
        in-app
        ;

variable meta-state
variable meta->in 

\ ======================================================================
\ display the target symbol table
((

: .olly   ( -- )
        base @ >r 16 base !
        s" gkernel.txt" w/o create-file abort" META: unable to create file" >r
        target-link
        begin   @ dup
        while   dup cell- @ 8 0 <# SWAP 0 ?DO # LOOP #> r@ write-file drop
                9 sp@ 1 r@ write-file drop drop
                dup cell+ @ count r@ write-line drop
        repeat  drop r> close-file r> base ! drop ;
))  

\ also assembler definitions
\ : UP   ( offset -- ) ebx  ;     \ EbX is user pointer    ** fix
\ : TOS    ( -- )        eax    ;
\ : TOS,   ( -- )        eax,   ;
\ : [TOS]  ( -- )        [eax]  ;
\ : [TOS], ( -- )        [eax], ;
\ previous definitions

\ ======================================================================
\ Meta compiler primitive literals

\ Primitive EQU is like a constant except that if it is used in a definition
\ it will just compile a literal.

: (equ)  ( n -- )
        create ,  does> @ [primitive] literal ;

: equ   ( n -<name>- )
        primitive definitions
        >in @  over (equ)  >in !
        meta definitions
        constant ;

also meta definitions previous
 0 constant handler             \ kludge !!!! ****  user kludge!
 4 constant win-handler
 8 constant sp0
12 constant rp0
16 constant base 
20 constant buf-off     \ buffer offset
24 constant buf-base    \ buffer base
32 constant next-user \ unused right now...
44 constant hld \ kludge !!!! ****  user kludge!
definitions

forth variable libs-list-t meta

: import: ( n -- )
        t-align t-here meta-constant
        voc-imports
        [target-xt] imp-call      timp t-dogen-#  \ isn't defined at this point
        [target-xt] imp-resolve   t-,             \ isn't defined at this point
        0 t-,               \ library
        last-h @ head.nt t-,
\        0 t-,               \ type fields (err can't delete this...)
        1 in/out            \ parm count alwasy n -- 1
        voc-forth ;

: library  ( -<name>- )
        t-align t-here meta-constant
        [target-xt] dovar tlib t-dogen-#
        t-align t-here  libs-list-t @ t-,  libs-list-t !
        0 t-,
        last-h @ head.nt t-,
        ;

: constant  ( n -<name>- )                    \ create a constant
            dup meta-constant
            [target-xt] dovar tcon t-dogen-#c
            0 1 in/out
            ;

: create    ( -<name>- )                      \ create a ptr to here
            t-align t-here meta-constant
            [target-xt] dovar tcre t-dogen-#
            0 1 in/out
            ;

: variable  ( -<name>- )                      \ create a variable (changable)
            t-align t-here meta-constant
            [target-xt] dovar tvar t-dogen-#
            0 t-,
            0 1 in/out
            ;

: value    ( n -<name>- )                    \ create a self fetching changeable value
            >r code r>                       \ generate the header
            tval t-tfa!                      \ type
            t-align t-here 
            asm[ >r  mov ecx r>
                     jmp 0 a;]
            val-link link, tcode-here cell- ,      \ to resolve later to (exec-val)
            t-,                              \ value in data space
            ct-(comp-val) last-h @ head.comp tsys-!   \ set the comp token to (comp-val)
            asm[ end-code a;]
            ofa-meta
            0 1 in/out
            ;

: user  ( n -<name>- )                     \ create a constant
            dup meta-constant
            [target-xt] dovar tusr t-dogen-u
            0 1 in/out
            ;

: defer    ( -<name>- )                      \ create a defer
            code                             \ generate the header
            tdef t-tfa!                      \ type
            t-align t-here
            asm[ >r mov ecx r>
                    jmp 0 a;]
            def-link link, tcode-here cell- ,      \ to resolve later to (exec-val)
            0 t-,                            \ value in data space
            ct-(comp-def) last-h @ head.comp tsys-!   \ set the comp token to (comp-val)
            asm[ end-code a;]
            ofa-meta
            ;
            
: dt-token:
   create swap rot t-, t-, t-, ;

\ TO used inside a definition
p: to       ( n <-name-> -- )
            't t>body >r
            asm[
                mov     { r> } eax
                mov     eax { ebp }
                add     ebp 4
            a;]
            ;

p: +to      ( n <-name-> -- )
            't t>body >r
            asm[
                add     { r> } eax
                mov     eax { ebp }
                add     ebp 4
            a;]
            ;

: is        ( xt <-name-> -- )
            't t>body t-! ; \ address where jmp points

p: is       [primitive] to ;

: alias ( xt -- )                  \ *** may need to change (see kernel)
        target-alias
        ;


: include ( -- )
          tcode-here >in @ include >in !
          voc-files create
          tcode-here t-, t-, 
          voc-forth ;

: begin-structure ( -- addr 0 )
             0 constant tcode-here #9 - 0 ;

: end-structure ( addr n -- )
             swap tcode-! ;                  \ set size of constant, horrible...

: offset    ( n -<name>- -- )                 \ create an offset
            >r
            code                              \ this will need changed to a header create?
            asm[ r@ if   add  eax r@    then
                         next; a;]
            rdrop 
            inline toff t-tfa! 1 1 in/out
            ;

: +field     ( n1 n2 <-name-> -- n3 )  \ compiling n3=n1+n2 stored offset=n1
             ( addr1 -- addr2 )        \ runtime addr2=addr1+n1
             over offset + ;

: field:     ( n1 <"name"> -- n2 ) ( addr -- 'addr )  aligned cell +field ;
: cfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  1 chars +field ;
: bfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  1 +field ;
: wfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  2 +field ;
: lfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  4 +field ;
: xfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  8 +field ;

: >ct        dup cell- tcode-@ + ;


\ ======================================================================
\ Meta Compiler Compiling Loop.

p: [ ( -- ) \ turn off meta-compiler
    [ also assembler ] a;                          
    [ previous ] in-meta  
    meta-state off ;

p: ;   tcode-here mtail-call = if     \ possible call/ret sequence
          $e9 tcode-here 5 - tcode-c!  \ change to a jump; tail call optimise
       then
       [ also assembler ] next; [ previous ]
       in-meta  meta-state off
       ;

\ The meta-compiling loop. Moves to in-primitive wordlists
\   only forward also target also primitive
\ Words, when executed, compile themselves (see do-target and do-forward)

\ If the word is not found, then we we need to create a forward reference
\ that will generate a call to the word later. This restricts these words
\ to those that are callable.

\ If the word is found, then it is executed, which will normally generate
\ the call to the word. These words will normally be in TARGET. If they're
\ in FORWARD, then see (forward) for the actions taken.

\ Words found in the primitive wordlist are those defined by p:, and they
\ when executed generate the specified code. Many are parsing words like
\ VALUE, POSTPONE, TO, IS etc.

\ [primitive] <name> forces the compilation from the primitive wordlist.
\ [target] <name> forces compilation from target wordlist.
\ [target-xt] <name> gets the target xt from the target wordlist.

\ Identify numbers (single numbers only)  <<<- USE RECOGNIZERS

forth

$10 dup , here swap cells allot value meta-recognizer

meta

: meta-not-found ( 0 -- )
    drop meta->in @ >in !
    not-found ( create forward reference ) ;

: meta-prim-literal ( n -- ) \ make primitive literal
    [primitive] literal ;

meta-recognizer  3                   over !
                 cell+ ' rec:num     over !
                 cell+ ' rec:quot    over !
                 cell+ ' rec:wincon  swap !

: meta-number?  ( ^str -- d n )           \ an extensible version of NUMBER
                meta-recognizer recognize
                dup dt:null <> if @ execute -1 else drop 0 0 then ;

\ We need a special version of WORD that will span multiple lines.
\ This will also save >IN so we can rescan the input stream.

: token  ( -- addr )
        begin 
          >in @ meta->in !
          parse-name buf-allot dup>r place r>
          dup c@ 0=                       \ *** ugly
        while   drop refill  0= abort" META: end of file in definition"
        repeat  ;

: ]     ( -- ) \ start meta-compiler. see [ for stopping
        meta-state on   
        in-primitive
        begin   token find \ no locals or class words
                if      execute
                else    count meta-number?
                        if      [primitive] literal
                        else    meta-not-found
                        then
                then
                meta-state @ 0=
        until   ;

\ ======================================================================
\ Interpretive words for Meta, must be last

: :noname $e9 tcode-c,
          na-link link, tcode-here ,
          0 tcode-,
          0 to mtail-call
          tcode-here
          ncode ] ;

: >body         t>body             ;
: '             't                 ;
: ,             t-,                ;
: w,            t-w,               ;
: c,            t-c,               ;
: here          t-here             ;
: allot         t-allot            ;
: ,"            t-string,          ;
: !             t-!                ;
: align         t-align            ;
: link,     ( addr -- )                       \ build link from list head at addr
    t-align t-here over t-@
    t-, swap t-! ;

: :             code ]             ; \ standard colon def MUST BE LAST

in-app                             \ start in-application

\ .olly

((
*** NOTE From this point on, everything is meta compiled!
))

