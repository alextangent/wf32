\ $Id: words.f,v 1.6 2007/05/23 08:12:58 georgeahubert Exp $

cr .( Loading WORDS...)

only forth also definitions

\ needs rewritten; very clunky code borrowed from Win32Forth

\ display words from one or two patterns

\ WORD doesn't meet the ANS-Standard in wf32.
\ The standard reqires that a space, not included in the length, must follow
\ the string. In wf32 a NULL follows the string.

here maxstring allot value pocket                  \ ptr to pocket allocated in main

code word ( char <chars>ccc<char> -- c-addr )
    1 1 in/out
      mov     edi { (source) cell+ }    \ edi = input pointer
      add     edi { >in }                \ add >in
      mov     ecx { (source) }           \ ecx = input length
      sub     ecx { >in }                \ subtract >in
      ja      short @@9
      xor     ecx ecx                \ at end of input
      jmp     short @@8
@@9:  cmp     al 32
      jne     short @@5
      \ delimiter is a blank, treat all chars <= 32 as the delimiter
@@1:  cmp     { edi } al             \ leading delimiter?
      ja      short @@2
      add     edi 1                \ go to next character
      sub     ecx 1
      jnz     short @@1
      mov     esi edi                \ esi = start of word
      mov     ecx edi                \ ecx = end of word
      jmp     short @@7
@@2:  mov     esi  edi                \ esi = start of word
@@3:  cmp     { edi } al             \ end of word?
      jbe     short @@4
      add     edi 1
      sub     ecx 1
      jnz     short @@3
      mov     ecx edi                \ ecx = end of word
      jmp     short @@7
@@4:  mov     ecx edi                \ ecx = end of word
      add     edi 1                \ skip over ending delimiter
      jmp     short @@7
      \ delimiter is not a blank
@@5:  repz    scas byte
      jne     short @@6
      mov     esi edi                \ end of input
      mov     ecx edi
      jmp     short @@7
@@6:  sub     edi 1                \ backup
      add     ecx 1
      mov     esi edi                \ esi = start of word
      repnz   scas byte
      mov     ecx edi                \ ecx = end of word
      jne     short @@7
      sub     ecx 1                \ account for ending delimiter
      \ update >in pointer and get word length
@@7:  sub     edi { (source) cell+ }     \ offset from start
      mov     { >in } edi               \ update >in
      sub     ecx esi                \ length of word
      cmp     ecx maxcounted 1-    \ max at maxcounted
      jbe     short @@8
      mov     ecx maxcounted 1-    \ clip to maxcounted
      \ move string to pocket
@@8:  mov     edi { ' pocket >body } \ edi = pocket
      push    edi
      mov     { edi } cl             \ store count byte
      add     edi 1
      rep     movs byte                   \ move rest of word
      xor     eax eax                \ clear eax
      stos byte                           \ append a null to pocket
      pop     eax                    \ return pocket reloc
      next;

internal        \ internal definitions start here

variable vocsave

create words-pocket maxstring allot

: .voc-once     ( -- )
    vocsave @ ?dup
    if      cr 10 dashes space .voc-name space
      horiz-line
      vocsave off
    then    ;

: match?        ( addr len -- f )
    2dup >r >r words-pocket count search nip nip
    pocket c@ if
      r> r>    pocket count search nip nip and
    else    
      r> r> 2drop
    then words-pocket c@ 0= or ;

0 value with-address?
true value with-tabs?
0 value word-count
0 value w#threads

: (words) ( 0 0 ) {: voc :} \  words-pad$ w#threads -- :}
    buf-allot {: words-pad$ :}
    voc dup voc#threads to w#threads
    voc.#0 here 500 + w#threads cells move     \ copy vocabulary up
    voc vocsave !
    begin   
      here 500 + w#threads largest dup
    while   
      dup link>name count 2dup words-pad$ place
      words-pad$ uppercase count match? if
        .voc-once with-address? if
          2 pick link>name name>interpret dup here u> if
            ." +"
          else space
          then $. ."  " 28
        else 18
        then -rot type
        with-tabs? if
          dup #tab space ?cr
        else drop cr
        then 1 +to word-count
      else 2drop
      then @ swap !
    repeat
    2drop vocsave off ;

\ enum constants
\ int wcEnumWin32Constants(char* addr, int len, CALLBACKPROC* proc)
\ int callback(char* addr, int len, int value)
3 import: wcEnumWin32Constants

0 value constant-cnt
0 value constant-tot

3 callback: .WinConstantCount ( abs_adr len value -- f )
    3drop 1 +to constant-tot 1 ;

3 callback: .WinConstant ( abs_adr len value -- f )
    drop \ discard the constant's value
    2dup filter-match? if
      type 20 #tab space 20 ?cr
      1 +to constant-cnt
    else 
      2drop
    then true ;

: count-constants ( -- n1 ) \ count the constants available to system
    constant-tot 0= if   \ only count if not counted...
      ['] .WinConstantCount 0 here wcEnumWin32Constants drop
    then constant-tot ;

external

: .constants    ( -<optional_name>- )
    0 to constant-cnt
    cr 10 dashes ."  Windows Constants "
    horiz-line
    set-filter
    ['] .WinConstant 0 here wcEnumWin32Constants drop
    cr horiz-line
    count-constants constant-cnt s" Windows constants" (displayed) ;

: with-address ( -- ) true to with-address? ;
                
internal

: _words         ( -<optional_name>- ) \ words partial-string will focus the list
    0 to word-count
    words-pocket off
    bl word uppercase c@ if
      pocket count words-pocket place
      bl word uppercase drop
      voc-link @
      begin   
        dup  ( #threads cells - )
        ['] (words) catch if
          cr ." Interrupted! (1)"
          drop true      \ stop now
        else @ dup 0=
        then
      until drop
    else context @ ['] (words) catch if
      drop cr ." Interrupted! (2)"
      then
    then 0 to with-address?
    base @ >r decimal
    cr horiz-line
    count-words word-count s" words" (displayed)
    r> base ! ;

external

: words         ( -<optional_name>- ) \ words partial-string will focus the list
    true to with-tabs? _words ;

module          \ end of the module

