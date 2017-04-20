\ ENVIRONMENT? support    by Tom Zimmer

cr .( Loading ANS environment... ) 

\ Implementation suggested by Andrew McKewan

only forth also definitions
1 #vocabulary environment

: environment?  ( a1 n1 -- false | ?? true )
    ['] environment >body 
    search-wordlist if execute true else false then ;

environment definitions

               true  constant wf32

         maxcounted  constant /counted-string
          maxbuffer  constant /hold
          maxbuffer  constant /pad
                  8  constant address-unit-bits
               true  constant core
               true  constant core-ext
        -10 7 / -2 = constant floored

                $ff  constant max-char
               $100  constant return-stack-cells
               $100  constant stack-cells

          $7fffffff  constant max-n
          $ffffffff  constant max-u
 $7fffffffffffffff. 2constant max-d
 $ffffffffffffffff. 2constant max-ud
               true  constant double
               true  constant double-ext

loaded? err [if]
               true  constant exception
               true  constant exception-ext
[then]

               true  constant facility
               true  constant facility-ext
               true  constant memory-alloc
               true  constant memory-alloc-ext
               true  constant file
               true  constant file-ext
               true  constant tools
               true  constant tools-ext
               true  constant search-order
               true  constant search-order-ext
              #vocs  constant wordlists
               true  constant string
               true  constant string-ext

loaded? locals [if]
                 63  constant #locals
                            : locals true ;
               true  constant locals-ext
[then]


loaded? float [if]
               true  constant floating
               true  constant floating-ext
    fstack-elements  constant floating-stack
               fbig fconstant max-float
[then]

only forth also definitions


