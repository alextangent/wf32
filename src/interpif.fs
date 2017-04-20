( interpif.f )
( interpretive if control structure )
( Placed in the public domain on 8aug96, by Jim Schneider )
\ August 9th, 1996 - 10:38 tjz slight modifications for Win32Forth
\ 19Apr2017 14:08:39 alexmcd support for wf32

cr .( Loading interpretive [IF] ... ) sourcefilename type

: match-[str] ( addr len addr len -- addr len fl )
    2over istr= ;

: match-[] ( possibly match an [if] [then] [else] )
           ( if-count addr len -- if-count' )
    s" [if]"   match-[str] if 2drop 1+
    else s" [else]" match-[str] if 2drop dup 1 <> and
    else s" [then]" match-[str] if 2drop 1-
    else 2drop then then then ;

: [else] ( -- )
    1 begin ?dup
    while definite-word match-[] repeat  ; immediate

: [if] ( flag -- )
    0= if postpone [else] then ; immediate

: [then] ( -- ) ; immediate

\ --------------------  [defined] and [undefined] --------------------

: [defined]     defined? 0<> ; immediate
: [undefined]   defined? 0= ; immediate

\ ------------------------------------------------------------------------
\ Conditional compiling
\ ------------------------------------------------------------------------

: \- ( "word" -- ) \ interpret line if "word" isn't defined
    postpone [defined] if postpone \ then ; immediate

: \+ ( "word" -- ) \ interpret line if "word" is defined.
    postpone [undefined] if postpone \ then ; immediate

