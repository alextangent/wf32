\ --------------------------- Change Block -------------------------------
\
\
\ ------------------------- End Change Block -----------------------------
\
\ Experimental: a fully optimising, STC based, ANS Forth compliant kernel
\
\    The licence is not clear on this code, but it is copyright of
\    Wil Baden 1981-07-04 1983-11-26
\
\ ------------------------------------------------------------------------


cr .( Loading sort & search... ) sourcefilename type

((

  *******************************************************************
  *                                                                 *
  *  Wil Baden 1981-07-04 1983-11-26                                *
  *                                                                 *
  *          Non-Recursive QUICKSORT                                *
  *                                                                 *
  *  This is the sort used in most Forth benchmarks.  It has        *
  *  an environmental dependency that `1 CELLS` is a power of 2.    *
  *                                                                 *
  *  It was originally written for figForth and Forth-83, before    *
  *  Standard Forth.                                                *
  *                                                                 *
  *******************************************************************

  Knuth, _Sorting and Searching_, 2nd Edition, p. 113

    The basic method of [Quicksort] is to take one record,
    say _R[1]_, and to move it to the final position that it
    should occupy in the sorted file, say position _s_.  While
    determining this final position, we will also rearrange
    the other records so there will be none with greater keys
    to the left of position _s_, and none with smaller keys
    to the right.  Thus the file will have been partitioned
    in such a way that the original sorting problem is
    reduced to two simpler problems, namely to sort _R[1]_
    ... _R[s-1]_, and (independently) _R[s+1]_ ... _R[N]_.

    We can apply the same technique to each of these
    subfiles, until the job is done.

  This implementation uses several improvements to Hoare's
  original formulation.

  SORT                ( a n -- )
     Order array _a_ _n_.

  (COMPARE)            ( pointer pointer -- flag )
     Deferred comparison routine for SORT.  This will usually be used to
     sort pointers. A sample set of CSTR-ASCEND and CSTR-DESCEND is 
     given to demonstrate string sorts.
     [ In this version, it is named PRECEDES ]

  THRESHOLD           ( -- <n cells> )
     The boundary where  `QUICKSORT` should not
     be used.  When sorting pointers to strings, for me 8 CELLS
     has tested a little better than the original 7 CELLS.

  EXCHANGE            ( addr1 addr2 -- )
     Exchange contents of two addresses.

  BOTH-ENDS           ( f l pivot -- f l )
     Put lower values on left, higher values on right.

  ORDER3              ( f l -- f l pivot )
     Order the first, last, and middle elements before
     doing a partition.  The pivot for the partition is the
     median of these three.

  PARTITION           ( f l -- f l' f' l )
     Split array into two partitions.

  SINK                ( f key where -- f )
     The innermost loop of insertion sort.

  INSERTION           ( f l -- )
     Insertion sort to be used with small sets.

  HOARIFY             ( f l -- ... )
     Partition until size is less than `THRESHOLD`.
     The smaller partition is taken next.

  QUICK               ( f l -- f l' f' l )
     Partition until done.
     
  BSEARCH             ( a n -- a' flag )
     Search ascending ordered array _a_ _n_. This version is designed for 
     maintaining a sorted array. If the item is not found, then the location 
     returned is the proper insertion point for the item. This could be used
     in an optimized insertion sort, for example.

  (COMPARE)            ( a1 a2 -- -ve | 0 | +ve )
     Deferred comparison routine for BSEARCH.  This will usually be used to
     sort pointers for which - (minus) can be used to set the flag. For strings
     use COMPARE (a sample CTR-COMPARE is given).
     [ In this version, it is named PRECEDES ]


  Exmaple tests;

  create test 7 , 3 , 0 , 99 , 9 , -1 , 6 , -8 , 4 , 5 ,
  
  : sortit1
    ['] > is precedes
    test 10 sort
    10 0 do test i cells + @ . loop cr ;
  sortit1 

  create a ," first"
  create b ," second"
  create c ," third"
  create d ," fourth"
  create e ," fifth"

  create stest a , b , c , d , e ,
  ' cstr-ascend is precedes

  : sortit2
    stest 5 sort
    5 0 do stest i cells + @ count type space loop cr ;
  sortit2

  ' cstr-compare is precedes
  : probe ( cstr -- )
        stest 5 bsearch . @ count type ;
  c" second" probe
  c" one" probe
  c" fourth" probe

  create test2 2 , 4 , 6 , 9 , 11 ,   99 ,
  ' - is precedes
  : probe ( n -- )
        test2 5 bsearch . @ . cr ;
  1 probe \ 0 2
  2 probe \ -1 2
  3 probe \ 0 4
  10 probe \ 0 11
  11 probe \ -1 11
  12 probe \ 0 99

))

external

defer precedes

   ' < is precedes  \ sort: signed ascends, u< for unsigned
\   ' - is precedes  \ bsearch: compare numbers
\   ' > is precedes  \ sort: signed descends, u> for unsigned

: cstr-compare         ( addr addr -- flag )
    >r count r> count compare ; \ bsearch: precedes for strings

: cstr-ascend          ( addr addr -- flag ) \ compare for ascending sort
    cstr-compare 0< ;  \ sort: compare for ascending

: cstr-descend         ( addr addr -- flag )
    cstr-compare 0> ;  \ sort: compare for descending

external

: bsearch ( search item count -- where found? )
    cells bounds ( item count -- item upper lower )
    rot >r
    begin  2dup >
    while  2dup
           tuck - 2/ -cell and +  ( u[*n] u l -- u[*n] mid )
           dup @ r@ precedes
           dup
    while  0<
           if   nip cell+   ( upper mid+1 )
           else rot drop swap ( mid lower )
           then
    repeat drop nip nip             true
    else   umax ( insertion-point ) false
    then
    r>drop ;

internal 

5 cells value threshold

\ in kernel
\ : exchange  ( a a -- )  2dup  @ swap @ rot !  swap ! ;

: both-ends         ( f l pivot -- f l )
    >r              ( f l)( r: pivot)
    begin
      over @ r@ precedes
    while
      cell 0 d+
    repeat
    begin
      r@ over @ precedes
    while
      cell-
    repeat r>drop ;

: order3            ( f l -- f l pivot )
    2dup over - 2/ -cell and + >r ( r: pivot)
    dup @ r@ @ precedes if
      dup r@ exchange
    then
    over @ r@ @ swap precedes if
      over r@ exchange
      dup @ r@ @ precedes if
        dup r@ exchange
      then
    then r> ;
    
: partition         ( f l -- f l' f' l )
    order3 @ >r
    2dup cell -cell d+  ( f l f' l')
      begin
      r@ both-ends 2dup 1+ u< if
        2dup exchange
        cell -cell d+
      then
      2dup swap u< 
    until swap rot r>drop ;            ( f l' f' l)
     
: sink              ( f key where -- f )
    rot >r                ( key where)( r: f)
    begin  
      cell- 2dup @ precedes
    while
      dup @ over cell+ !
      dup r@ = if
        ! r> exit  ( f)
      then  ( key where -- )
    repeat cell+ ! r> ;             ( f)

: insertion         ( f l -- )
    2dup u< if
      cell+ over cell+ 
      do ( f)
        i @ i sink
      cell +loop drop
    else 2drop
    then ;

: hoarify           ( f l -- ... )
    begin
      2dup threshold 0 d+ u<
    while
      partition ( ... f l' f' l)
      2dup - >r  2over - r> > if
        2swap
      then
    repeat ( ... f l) insertion ;  ( ...)

external

: quick             ( f l -- )
    depth >r
    begin
      hoarify
      depth r@ <
    until r>drop ;

: sort              ( a n -- )
    ?dup if
      1- over +cells ( f l) quick
    else drop then ;

module

