\ --------------------------- Change Block -------------------------------
\
\
\
\
\ ------------------------- End Change Block -----------------------------
\
\ Experimental: a fully optimising, STC based, ANS Forth compliant kernel
\
\ Copyright [c] 2011 by Alex McDonald (alex at rivadpm dot com)
\           [c] Stephen Pelc, <stephen@mpeforth.com>
\               MicroProcessor Engineering Ltd www.mpeforth.com
\           [c] Peter Knaggs <pjk@bcs.org.uk>
\               University of Exeter www.rigwit.co.uk
\
\ Taken from the VFX Forth source tree and modified to remove implementation
\ dependencies. This code assumes the system is case insensitive.
\ see http://www.forth200x.org/escaped-strings.html
\
\ This program is free software; you can redistribute it and/or modify it
\ under the terms of the GNU General Public License as published by the
\ Free Software Foundation; either version 2 of the License, or <at your
\ option> any later version.
\
\ This program is distributed in the hope that it will be useful, but
\ WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
\ General Public License for more details.
\
\ You should have received a copy of the GNU General Public License along
\ with this program; if not, write to the Free Software Foundation, Inc.,
\ 675 Mass Ave, Cambridge, MA 02139, USA.
\
\ ------------------------------------------------------------------------

base@ decimal

-400 constant throw_malestr

create esc-table ( table of translations for a..z )
        7 c,	\ \a bel (alert)
        8 c,	\ \b bs  (backspace)
      'c' c,    \ \c
      'd' c,    \ \d
       27 c,	\ \e esc (escape)
       12 c,	\ \f ff  (form feed)
      'g' c,    \ \g
      'h' c,    \ \h
      'i' c,    \ \i
      'j' c,    \ \j
      'k' c,    \ \k
       10 c,	\ \l lf  (line feed)
      'm' c,    \ \m
      'n' c,    \ \n ( CRLF, \r\n )
      'o' c,    \ \o
      'p' c,    \ \p
      '"' c,    \ \q "   (double quote)
       13 c,	\ \r cr  (carriage return)
      's' c,    \ \s
        9 c,	\ \t ht  (horizontal tab}
      'u' c,    \ \u     (becomes UTF-8)
       11 c,	\ \v vt  (vertical tab)
      'w' c,    \ \w
      'x' c,    \ \x     (becomes hex)
      'y' c,    \ \y
        0 c,	\ \z nul (no character)
        
\ Convert a number to a UTF-8 string and append to
\ a counted string

8 import: WideCharToMultiByte

: addUTF-8 \ {: w^ n dest -- :}
    dup>r sp@ cell+ >r >r
    0 0 4 r> count +
    -1 r> 0 cp_utf8
    WideCharToMultiByte
    dup 0= throw_malestr ?throw
    1- r> c+! drop ;

\ Extract an n-digit hex number in the given base from the
\ start of the string, returning the remaining string
\ and the converted number.

: extract-hex ( c-addr len n -- c-addr' len' u )
    base@ >r hex
    >r 0 0 2over r@ < throw_malestr ?throw ( input string too short )
    r@ >number throw_malestr ?throw        ( not hex characters )
    2drop r> swap >r /string r>
    r> base! ;

: addEscape	( c-addr len dest -- c-addr' len' )
    >r dup 0= throw_malestr ?throw         ( input string too short )
    over c@ case
      '0' of 0           r> c+place endof
      'm' of crlf$ count r> +place endof
      'n' of crlf$ count r> +place endof
      'u' of 1 /string 4 extract-hex r> addUTF-8 exit endof
      'x' of 1 /string 2 extract-hex r> c+place exit  endof
      dup dup 'a' 'z' between if
        'a' - esc-table + c@
      then r> c+place
    endcase 1 /string ;

: unescape-str ( c-addr len dest -- c-addr' len' )
    >r
    begin dup                           \ -- caddr len ; R: -- dest
      while over c@ dup '"' <>          \ check for terminator
      while dup '\' = if                \ deal with escapes
          drop 1 /string r@ addEscape
        else                            \ normal character
          r@ c+place 1 /string
        then
      repeat drop
    then 1 /string rdrop ;

: parse\" ( -- c-addr len )
    source-remain tuck
    buf-allot dup>r
    0 over c!
    unescape-str nip
    - >in +!
    r> count ;

: pslit postpone sliteral ;

: s\" ( -- )
    parse\"
    compiles> drop
      parse\"  pslit ;

: .\" ( -- )
    parse\" type
    compiles> drop
      postpone s\" postpone type ;

: ,\" ( -<string">-  ) parse\" ", ;

\ -------------------- "string type" ------------------------------

  ' noop
  ' pslit
  :noname pslit postpone pslit ;
dt-token: dt:string

: rec:string ( addr len -- addr len' dt:string | dt:null )
   over c@ '"' = if
     drop 1+ tib - >in !          \ step back in parse area
     parse\" dt:string            \ get up to trailing "
   else 2drop dt:null then ;

' rec:string get-recognizers 1+ set-recognizers

base!

