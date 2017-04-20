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

((
begin-structure evm%   ( mouse event )
  coord% +field evm.pos
         field: evm.button
         field: evm.ctlstate
         field: evm.evflags
end-structure

begin-structure evt%
     field: evt.type
  0 +field  evt.evt
end-structure

begin-structure evk%   ( key event )
  field:  evk.keydown
  wfield: evk.rep
  wfield: evk.vkey
  wfield: evk.virtscan
  wfield: evk.char
  field:  evk.ctlstate
end-structure

begin-structure coord%
  wfield: coord.x
  wfield: coord.y
end-structure

begin-structure srect%
  wfield: srect.left
  wfield: srect.top
  wfield: srect.right
  wfield: srect.bottom
end-structure

begin-structure csb%   ( console info )
  coord% +field  csb.size
  coord% +field  csb.cursorpos
         wfield: csb.attr
  srect% +field  csb.window
  coord% +field  csb.maxwinsize
end-structure
))

external

( console and input/output handles )
2 import: ShowWindow
0 value conhndl ( window handle for the original console handle )
0 value csbin   ( input buffer handle )
0 value csbout  ( output buffer handle )

internal

: vis-console  ( -- ) conhndl call ShowWindow drop ;
: t-hide-console ( -- ) SW_HIDE vis-console ;
: t-show-console ( -- ) SW_SHOWNORMAL vis-console ;

( console status words )
2 import: GetConsoleScreenBufferInfo
create csb 24 allot
: constate    ( -- )      csb csbout call GetConsoleScreenBufferInfo drop ;

( get and set rows and cols )
2 import: SetConsoleCursorPosition
: t-cols        ( -- n1 ) [ csb 0 + ] literal w@ ;   \ current screen columns
: t-rows        ( -- n1 ) [ csb 2 + ] literal w@ ;   \ current screen rows
: t-csrcol      ( -- n )  [ csb 4 + ] literal w@ ;
: t-csrrow      ( -- n )  [ csb 6 + ] literal w@ ;
: wbuffsz       ( -- n )  t-cols t-rows * ;

: t-getcolrow   ( -- c r ) cols rows ;
: t-get-xy      ( -- c r ) t-csrcol t-csrrow ;
: t-at-xy       ( c r -- ) 16 lshift or csbout call SetConsoleCursorPosition drop constate ;
: t-#col        ( n -- )   cols 1- min t-csrcol - spaces ;
: t-#tab        ( n -- )   t-csrcol over / 1+ * #col ;
: t-csr-inview  ( -- )     get-xy set-xy ;

( attributes )
2 import: SetConsoleTextAttribute
5 import: FillConsoleOutputAttribute
FOREGROUND_RED FOREGROUND_BLUE FOREGROUND_GREEN + + constant out-attr
FOREGROUND_RED FOREGROUND_BLUE FOREGROUND_GREEN + +
    FOREGROUND_INTENSITY +                          constant inp-attr
BACKGROUND_RED BACKGROUND_BLUE BACKGROUND_GREEN + +
    BACKGROUND_INTENSITY +                          constant csr-attr

: t-attr        ( n -- )  csbout call SetConsoleTextAttribute drop ;
: t-attrinp     ( -- )    inp-attr t-attr ;
: t-attrout     ( -- )    out-attr t-attr ;
: t-setattr     ( -- )    constate 0 sp@ 0 wbuffsz out-attr
                          csbout call FillConsoleOutputAttribute 2drop ;

( cursor support )
1 import: SetConsoleCursorInfo
0 value ins-flag
: t-insert ( f -- ) ( set or toggle insert )
    if ins-flag 0= else 0 then dup to ins-flag
    -80 and 100 + 1 swap sp@ csbout call SetConsoleCursorInfo 3drop ;

( codepage support )
0 import: GetConsoleCP
1 import: SetConsoleCP
1 import: SetConsoleOutputCP
0 value conCP
: t-setCP ( n -- )        dup call SetConsoleCP drop
                          call SetConsoleOutputCP drop ;
: t-setlatin ( -- )       conCP t-setCP ;
: t-setutf-8 ( -- )       65001 t-setCP ;

( output to console )
5 import: FillConsoleOutputCharacter
: t-cls ( -- )  t-setattr
                0 sp@ 0 wbuffsz bl
                csbout call FillConsoleOutputCharacter 2drop
                0 0 set-xy ;

: t-type        ( addr cnt -- ) \ type a UTF-8 string
                csbout write-file drop constate ;

: t-emit        ( xchar -- ) ( support utf-8, codepoint )
                sp@ over $100 < 2+ type drop ;

: t-beep        7 emit ;
: t-bs          8 emit ;
: t-tab         tab-size #tab ;
: t-cr          crlf$ count type ;
: t-?cr         ( n -- ) cols t-csrcol - > if cr then ;

( input from console )
4 import: ReadConsoleInputW
2 import: GetNumberOfConsoleInputEvents

external 

$80000000 constant k-keydown-mask  \ keydown mask
$40000000 constant k-mkey-mask    \ mouse mask
$20000000 constant k-char-mask   \ indicates a character
$10000000 constant k-fkey-mask      \ special key mask
$08000000 constant k-alt-mask      \ alt key mask
$04000000 constant k-ctrl-mask     \ control key mask
$02000000 constant k-shift-mask    \ shift key mask
$1c000000 constant k-mod-mask      \ fn, alt and control key mask

$01 constant k-lbutton  \ left mouse button
$02 constant k-rbutton  \ right mouse button
$03 constant k-cancel   \ control-break processing
$04 constant k-mbutton  \ middle mouse button (three-button mouse)

$10000010 constant k-shift    \ shift key
$10000011 constant k-control  \ control key
$10000012 constant k-alt      \ alt key

$08 constant k-bs       \ backspace
$09 constant k-tab      \ tab
$0a constant k-lf       \ linefeed
$0d constant k-cr       \ return
$1b constant k-esc      \ escape key

$21 constant k-prior    \ page up key
$22 constant k-next     \ page down key
$23 constant k-end      \ end key
$24 constant k-home     \ home key
$25 constant k-left     \ left arrow key
$26 constant k-up       \ up arrow key
$27 constant k-right    \ right arrow key
$28 constant k-down     \ down arrow key
$29 constant k-select   \ select key
$2a constant k-print    \ print key
$2b constant k-execute  \ execute key
$2c constant k-snapshot \ print screen key
$2d constant k-insert   \ ins key
$2e constant k-delete   \ del key
$2f constant k-help     \ help key

$70 constant k-F1       \ function keys
$71 constant k-F2
$72 constant k-F3
$73 constant k-F4
$74 constant k-F5
$75 constant k-F6
$76 constant k-F7
$77 constant k-F8
$78 constant k-F9
$79 constant k-F10
$7a constant k-F11
$7b constant k-F12

internal 

create inp-rec  10 cells allot    ( input-record for console input )
: inp-down ( -- u ) [ inp-rec  4 + ] literal @ ;
: inp-vkey ( -- u ) [ inp-rec 10 + ] literal w@ ;
: inp-char ( -- u ) [ inp-rec 14 + ] literal w@ ;
: inp-ctrl ( -- u ) [ inp-rec 16 + ] literal @ ;

external

: +k-shift      ( c1 -- c2 )  k-shift-mask or ; inline  \ add in shift bit
: +k-alt        ( c1 -- c2 )  k-alt-mask or ;   inline  \ add in alt bit
: +k-ctrl       ( c1 -- c2 )  k-ctrl-mask or ;  inline   \ add in ctrl bit

internal

: t-ctl>mask ( ctlstate -- mask )
    0
    over SHIFT_PRESSED
      and if +k-shift then
    over [ LEFT_ALT_PRESSED RIGHT_ALT_PRESSED or ]   literal
      and if +k-alt then
    swap [ LEFT_CTRL_PRESSED RIGHT_CTRL_PRESSED or ] literal
      and if +k-ctrl then ;

: dokey ( -- u ) ( process a keystroke )
    inp-char         ( unicode char )
    dup $20 >= over $0D = or if ( is it a char or CR? )
      k-char-mask or ( it's potential char )
    else
      drop inp-vkey  ( if no char, replace with virt key )
      k-fkey-mask or   ( mask in special key )
    then
    inp-ctrl t-ctl>mask          ( translate control key state )
    or dup k-mod-mask and if
      [ k-char-mask invert ] literal and ( turn of char, since ctl char )
    then
    inp-down 31 lshift or ; ( add in keydown state )

: ekeyprim ( -- u ) \ get keyboard & mouse primitives
    0 sp@ 1 inp-rec dup>r csbin call ReadConsoleInputW 2drop
    r> w@ case
      KEY_EVENT of
        dokey
      endof
      WINDOW_BUFFER_SIZE_EVENT of
        constate 0
      endof
\      MOUSE_EVENT of
\        domouse
\      endof
      0 swap
    endcase ;

: t-ekey ( -- u )
    begin ekeyprim ?dup if exit then again ;

: t-ekey> ( u mask -- u flag )
    2dup and = ;

: t-ekey>char   ( u -- u false | char true ) \ is char ?
    [ k-char-mask k-keydown-mask or ] literal t-ekey>
    if $ffff and true else false then ;

: t-ekey>fkey   ( u -- u false | key true ) \ is fn-key ?
    [ k-fkey-mask k-keydown-mask or ] literal t-ekey>
    if $0e00ffff and true else false then ;

variable doschar -1 doschar !   \ current char buffer

: t-key         ( -- char ) \ get key from keyboard
     doschar @ dup 1+ if -1 doschar ! exit then
     drop
     begin ekey ekey>char
       0= while
       drop
     repeat ;

: t-key?        ( -- flag ) \ is a char (ie key pressed) available ?
     doschar @ 0> if true exit then
     begin
       0 sp@ csbin call GetNumberOfConsoleInputEvents drop
     while                          \ loop while events present
       ekey ekey>char             \ exit if event is a valid char
       if doschar ! true exit then
       drop
     repeat false ;

((

Edit buffer pointer primitives invoked by various key and/or mouse sequences
CURSOR and ANCHOR are guaranteed to be inside the buffer. CURSOR is the
position of the cursor where edit operations take place, such as PASTE. CURSOR
is either leftmost or rightmost of ANCHOR, depending on the SELECT operation.

  SELECTION n m Leftmost and length of selection
  n POSITION    Move anchor and cursor to a specific char location in the buffer
  n RELATIVE    Move anchor and cursor left/right a character
  n SELECT      shift+arrow (left or right)
                move the cursor left (-ve) or right (+ve)
                The cursor moves and the anchor stays fixed.
                0 SELECT, n RELATIVE or n POSITION resets the selection.
  SELECT-ALL    ctrl+A
                Move anchor and cursor to beginning and end of buffer

))

0 value buffptr      ( the buffer )
variable bufflen     ( buffer length )
variable buffmax     ( buffer max length )
variable vcursor     ( the virtual cursor )
variable anchor      ( the select anchor point )
variable prevmin     ( min position for update )
variable prevmax     ( previous buffer length )

: buffer     ( -- a n )    buffptr bufflen @ ;
: limit      ( n -- m )    0max bufflen @ min ;
: anc-curs   ( -- a c )    anchor @ vcursor @ ;
: leftmost   ( -- n )      anc-curs min ;
: rightmost  ( -- n )      anc-curs max ;
: prevmost   ( -- )        bufflen @ prevmax @ max prevmax !
                           prevmin @ leftmost min prevmin ! ;
: expand     ( n -- )      bufflen @ + dup bufflen !
                           buffmax @ > abort" Buffer overrun"
                           prevmost ;

: position   ( n -- )      prevmost limit dup anchor ! vcursor ! ;
: relative   ( n -- )      vcursor @ + position ;
: selection  ( -- pos n )  leftmost rightmost over - ;
: select     ( n -- )      prevmost vcursor @ + limit vcursor ! ;
: end        ( -- )        bufflen @ position ;
: home       ( -- )        0 position ;
: select-all ( -- )        end 0 anchor ! ;

: buff-init   ( a l -- )
              buffmax ! to buffptr
              0 bufflen !
              0 prevmax !
              0 prevmin ! 0 position ;

: dbg-s
    buffer cr type
\    chg-min cr spaces ." |"
\    chg-max chg-min - 1- spaces ." |"
    cr selection swap spaces
    ?dup if
      0 ?do  ." _"  loop
    else
      ." ^"
    then ;


((

Edit buffer action primitives invoked by various key and/or mouse sequences )

  DELETE      DEL
              Delete the selected text
  DELETE-LEFT backspace
              If there's a selection: Delete the selected text
              Otherwise delete 1 character to the left of the cursor
  DELETE-RIGHT as DELETE-LEFT
  a l INSERT  Inserts a string after deleting the selection if any
  a l OVERWRITE Deletes L chars and inserts

))

create copybuff 256 allot   ( temp )
variable copylen
create undobuff 256 allot   ( temp )
variable undolen            ( length )
variable undopos            ( cursor position )

: saveundo    ( -- )       vcursor @ undopos !
                           buffer undobuff swap dup undolen ! cmove ;

: delete ( -- )      ( deletes selection )
    saveundo
    buffer selection >r /string r>
    over if
      dup negate expand
      2 pick >r /string r> swap move
    else
      3drop
    then
    leftmost position ; ( leftmost position )

: delete-lr ( n -- )  selection nip if drop else select then delete ;
: delete-right ( -- ) 1 delete-lr ;
: delete-left ( -- )  -1 delete-lr ;

variable insert-flag 

: insert     ( a n -- )
     delete                 ( must call delete )
     dup>r expand           ( make sure new string can fit )
     buffer rightmost /string over swap ( rightmost source is target for paste later )
     over r@ +              ( source len dest )
     swap move              ( shift buffer right )
     r@ cmove               ( move in new stuff )
     r> relative ;

: overwrite  ( a n -- )    dup select insert ;

((

Integrated into the Windows clipboard. The text is save in UTF-16 so that
other apps can copy and paste valid data to the console. Uses Windows
conversion routines to go between UTF-8 and UTF-16.

  COPY        ctrl+C
              Copy the selection to a copy buffer
  CUT         ctrl+X
              copy and delete the selected text
  PASTE       ctrl+V
              replace the selection with the text in the copy buffer
  UNDO        ctrl+Z
              undo the last action
  REDO        shift+ctrl+Z
              redo the last action

))

1 import: OpenClipboard
0 import: CloseClipboard
0 import: EmptyClipboard
2 import: SetClipboardData
1 import: IsClipboardFormatAvailable
1 import: GetClipboardData

6 import: MultiByteToWideChar
8 import: WideCharToMultiByte

variable copy-bptr ( copy buffer pointer )
variable paste-bptr ( paste buffer pointer )

: clip-alloc ( n p -- )
    dup>r @ swap cell+
    resize abort" failed to resize buffer"
    dup r> ! ;
: copy-alloc  ( n -- ) copy-bptr  clip-alloc ;
: paste-alloc ( n -- ) paste-bptr clip-alloc ;

: utf8>wc ( i*4 -- l ) 0 CP_UTF8 MultiByteToWideChar ;
\ : wc>utf8 ( i*6 -- l ) 0 CP_UTF8 WideCharToMultiByte ;

: copy>wc ( a n -- buff )
    swap 0 0 2over utf8>wc 2*
    dup copy-alloc 2dup 2>r
    2swap utf8>wc drop 2r> tuck + off ;

: str>clip ( a n -- ) ( copy text to clipboard )
    conhndl OpenClipboard drop
    EmptyClipboard drop
    copy>wc CF_UNICODETEXT SetClipboardData drop
    CloseClipboard drop ;

: clip>str ( -- a n ) ( get copy, trucnate to single line from clipboard )
    conhndl OpenClipboard drop
    CF_UNICODETEXT IsClipboardFormatAvailable if
        0 0
        maxcounted dup paste-alloc dup>r
        -1 CF_UNICODETEXT GetClipboardData
      0 CP_UTF8 WideCharToMultiByte drop
      r> zcount 2dup $0d scan nip -
    else s" "
    then CloseClipboard drop ;

: copy       ( -- )        buffer selection ?dup if
                             >r /string drop r> str>clip
                           else 2drop then ;
: cut        ( -- )        copy delete ;
: paste      ( -- )        clip>str insert ;
: undo        ( -- )       undobuff buffptr undolen @ ?dup if
                             dup bufflen ! cmove
                             undopos @ position
                           else 2drop then ;
: redo        ( -- )       ;

((

Various console operations have to be synchronised with the changes to the
input buffer. There are 2 operations:

1. Character changes in the buffer
   These need to be reflected on-screen. Two pointers are maintained; the
   leftmost affected character and the maximum righmost affected, which can
   exceed the current buffer length. The buffer is re-written, along with the
   output attributes from leftmost to the current length; the trailing
   characters exposed by a buffer shortening (if any) are blanked.

2. Cursor and selection movement
   The supplied console "hardware" cursor is used when the selection is 0
   characters. _ indicates the insertion point; a block cursor indicates the
   overtype point. For non-zero selections the hardware cursor is turned off
   and reverse video is used for the characters affected, and there is no
   indication of insert/overtype until the selection is processed or abandoned.

(1) is dealt with first, then (2). The buffer must not contain any control
characters that move the cursor when using WRITE-FILE to output, otherwise we
will lose control of the correct cursor & selection placement. (There's a
mixture of high-level console management, for instance WRITE-FILE, and some low-
level calls for such things as attributes and the cursor.)

Cursor position is specfied in row/column by Windows. The code maintains a
relative virtual cursor and translates this to the physical location.

))

( cursor support )
1 import: SetConsoleCursorInfo

: cursor-set ( on=1|off=0 -- )
    insert-flag @ if 20 else 100 then ( set the size )
    sp@ csbout call SetConsoleCursorInfo 3drop ;
: cursor-on  1 cursor-set ;
: cursor-off 0 cursor-set ;

( row and col support )
0 value acc-col
0 value acc-row

: cursor-position ( n -- ) ( place cursor on screen ) ( fail for selection )
    >r acc-col acc-row r> cols /mod d+
    at-xy ;

: r-output ( -- )
    buffer prevmin @ /string dup if
      prevmin @ cursor-position type
    else 2drop then
    prevmax @ bufflen @ - 0max 0 ?do ."  " loop
    vcursor @ cursor-position
    0 prevmin ! 0 prevmax !
    ;

: r-cr ( -- )
    r-output
    end
    vcursor @ cursor-position
    bufflen @ dup if space then
    t-attrout ;

: r-accept      ( c-addr nbmax -- nbread ) \ accept a string
    buff-init 
    get-xy 2dup set-xy to acc-row to acc-col
    cursor-on
    t-attrinp
    vcursor @ cursor-position
    begin
      ekey
      ekey>fkey if              ( control characters )
        case
          k-cr of
            r-cr exit
          endof
          k-end of
            end
          endof
          k-home of
            home
          endof
          k-left of
            -1 relative
          endof
          k-right of
            1 relative
          endof
          k-left +k-shift of
            -1 select
          endof
          k-right +k-shift of
            1 select
          endof
          k-delete of
            delete-right
          endof
          k-bs of
            delete-left
          endof
          k-esc of
            select-all delete
          endof
          k-insert of
            insert-flag @ 0= insert-flag !
            cursor-on
          endof
          'C' +k-ctrl of
            copy
          endof
          'V' +k-ctrl of
            paste
          endof
          'X' +k-ctrl of
            cut
          endof
          'A' +k-ctrl of
            select-all
          endof
          'Z' +k-ctrl of
            undo
          endof
          'Z' +k-ctrl +k-shift of
            redo
          endof
        endcase
        vcursor @ cursor-position
      else
        ekey>char if
          dup k-cr = if
            drop r-cr exit
          then
          dup sp@ 1
          insert-flag @ if overwrite else insert then
          drop
          drop
        else
          drop
        then
      then
      r-output
    again ;

( initialisation, error recovery etc )
\ 0 import: AllocConsole
2 import: GetConsoleMode
2 import: SetConsoleMode
0 import: GetConsoleWindow
1 import: GetStdHandle
\ 0 import: FreeConsole
1 import: SetConsoleTitle
\ 2 import: SetConsoleFont ( undocumented )
2 import: SetConsoleCursorInfo

: t-init-title ( -- ) (version) drop call SetConsoleTitle drop ;

1 import: FlushConsoleInputBuffer
: flush-console ( -- )
    csbin call FlushConsoleInputBuffer drop ( bin any outstanding "stuff") ;

: t-init-console ( -- ) \ init kernel's dos console
\    call AllocConsole drop  \ alloc the character mode cons.
\    d_init-console
    stdin to csbin
    stdout to csbout
    csb 24 erase
    0 sp@ csbin GetConsoleMode drop
      [ ENABLE_WINDOW_INPUT ] literal or
      [ ENABLE_PROCESSED_INPUT invert ] literal and
      csbin SetConsoleMode drop
    call GetConsoleWindow to conhndl
    call GetConsoleCP to conCP
    t-setutf-8
\    10 csbout call SetConsoleFont drop \ set Lucida
    t-setattr
    t-attrout
\    ['] flush-console reset-stack-chain chain+
    constate ;

' flush-console reset-stack-chain chain+

\ -------------------- Deferred I/O -----------------------------------------

' t-init-console       is init-console
' t-init-console       is init-screen
' t-show-console       is show-console
' t-hide-console       is hide-console
' t-init-title         is init-title

' t-cls                is cls
' t-at-xy              is set-xy
' t-get-xy             is get-xy
\ ' noop                 is console
' t-csr-inview         is cursor-inview
\ ' 2drop                is fgbg!
\ ' k_noop1              is fg@
\ ' k_noop1              is bg@
' t-getcolrow          is getcolrow
\ ' 2drop                is setcolrow
\ ' drop                 is set-cursor
\ ' k_noop1              is get-cursor
\ ' drop                 is setrowoff
\ ' k_noop1              is getrowoff
\ ' k_noop2              is getmaxcolrow
\ ' 2drop                is setmaxcolrow
' t-ekey               is ekey
' t-ekey>char          is ekey>char
' t-ekey>fkey          is ekey>fkey
' t-key                is key
' t-key?               is key?
' t-type               is type
' t-emit               is emit
' t-cr                 is cr
' t-?cr                is ?cr
' t-tab                is tab
' t-bs                 is bs
' t-beep               is beep
' r-accept             is accept
' t-#col               is #col
' t-#tab               is #tab
' t-cols               is cols
' t-rows               is rows

: r-init

    ['] t-init-console       is init-console
    ['] t-init-console       is init-screen
    ['] t-show-console       is show-console
    ['] t-hide-console       is hide-console
    ['] t-init-title         is init-title
    
    ['] t-cls                is cls
    ['] t-at-xy              is set-xy
    ['] t-get-xy             is get-xy
    \ ['] noop                 is console
    ['] t-csr-inview         is cursor-inview
    \ ['] 2drop                is fgbg!
    \ ['] k_noop1              is fg@
    \ ['] k_noop1              is bg@
    ['] t-getcolrow          is getcolrow
    \ ['] 2drop                is setcolrow
    \ ['] drop                 is set-cursor
    \ ['] k_noop1              is get-cursor
    \ ['] drop                 is setrowoff
    \ ['] k_noop1              is getrowoff
    \ ['] k_noop2              is getmaxcolrow
    \ ['] 2drop                is setmaxcolrow
    ['] t-ekey               is ekey
    ['] t-ekey>char          is ekey>char
    ['] t-ekey>fkey          is ekey>fkey
    ['] t-key                is key
    ['] t-key?               is key?
    ['] t-type               is type
    ['] t-emit               is emit
    ['] t-cr                 is cr
    ['] t-?cr                is ?cr
    ['] t-tab                is tab
    ['] t-bs                 is bs
    ['] t-beep               is beep
    ['] r-accept             is accept
    ['] t-#col               is #col
    ['] t-#tab               is #tab
    ['] t-cols               is cols
    ['] t-rows               is rows
    ;

init-console
init-title

\ s" dos-console.txt" ' included catch drop 2drop

module

