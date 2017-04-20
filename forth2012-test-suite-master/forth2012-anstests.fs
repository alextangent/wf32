s" forth2012-test-suite-master/src" +path
ANS-MEMORY
warning off
cr ." This will NOT currently pass all the Forth2012 HAYES tests"
cr ."   1 LOCALS tests; the (LOCAL) word (and just that; all other locals"
cr ."     features work OK) is currently broken in this implementation"
cr ."   2 STRINGTEST.FTH is terminated early as there is no implementation"
cr ."     of SUBSTITUTE UNESCAPE etc"
cr key drop
cr
include runtests.fth
