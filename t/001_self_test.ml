#!/usr/bin/env ocaml

#use "topfind";;
#require "batteries";;
#directory "_build/src";;
#load "testsimple.cma";;

open TestSimple;;

plan 1;;

(* Just check the output of our 000_load test *)

(is 
    (TestBuilder.Tester.plan 11 (fun () ->
        diag "... testing O'Caml TestSimple v 0.01";
        ok true "... ok passed";
        okf true "... %s passed" "okf";
        is 2 2 "... is <int> <int> passed";    
        isf 2 2 "... %s <int> <int> passed" "isf";
        is 2. 2. "... is <float> <float> passed";        
        is "foo" "foo" "... is <string> <string> passed"; 
        is [] [] "... is <'a list> <'a list> passed";   
        is [1;2;3] [1;2;3] "... is <int list> <int list> passed"; 
        is ["foo";"bar"] ["foo";"bar"] "... is <string list> <string list> passed";
        is (1,"foo") (1,"foo") "... is <int * string> <int * string> passed";    
        is TAPDocument.Ok TAPDocument.Ok "... is <type> <type> passed";             
    ))
"1..11
# ... testing O'Caml TestSimple v 0.01
ok 1 - ... ok passed
ok 2 - ... okf passed
ok 3 - ... is <int> <int> passed
ok 4 - ... isf <int> <int> passed
ok 5 - ... is <float> <float> passed
ok 6 - ... is <string> <string> passed
ok 7 - ... is <'a list> <'a list> passed
ok 8 - ... is <int list> <int list> passed
ok 9 - ... is <string list> <string list> passed
ok 10 - ... is <int * string> <int * string> passed
ok 11 - ... is <type> <type> passed
" 
"... got the output we expected");;













