(*
 * TAPDocument - A simplified AST to represent a TAP document 
 * Copyright (C) 2007 Infinity Interactive, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type status = Ok | NotOk

type directive  = Todo of string
type diagnostic = Diag of string list

type node = TestCaseNode     of status * int * string * diagnostic option
          | TodoTestCaseNode of status * int * string * directive * diagnostic option
          | DiagnosticNode   of diagnostic
          | PlanNode         of int

type t = Document of node list

let rec count_test_nodes nodes count = 
   match nodes with
       | []                              
            -> count
       | TestCaseNode(_,_,_,_)::xs       
       | TodoTestCaseNode(_,_,_,_,_)::xs 
            -> count_test_nodes xs count + 1
       | DiagnosticNode(_)::xs
       | PlanNode(_)::xs                 
            -> count_test_nodes xs count

let count_tests = function Document(nodes) -> 
    count_test_nodes nodes 0
        
let count_test_failures = function Document(nodes) -> 
    let rec loop nodes count =
        match nodes with
            | []                                  
                -> count
            | TestCaseNode(Ok,_,_,_)::xs       
            | TodoTestCaseNode(Ok,_,_,_,_)::xs    
            | DiagnosticNode(_)::xs               
            | PlanNode(_)::xs                     
                -> loop xs count            
            | TestCaseNode(NotOk,_,_,_)::xs       
            | TodoTestCaseNode(NotOk,_,_,_,_)::xs 
                -> loop xs count + 1
    in loop nodes 0

(* These will get automatically tagged onto the document *)

(*
    TODO:
    Convert the next two functions to use 
    Format.sprintf for constructing the 
    strings
*)
let create_failure_footer test_count failure_count =
    DiagnosticNode(
        Diag(["Looks like you failed " 
            ^ (string_of_int failure_count) 
            ^ " tests of "
            ^ (string_of_int test_count)             
            ^ " run."
        ])
    )

let create_count_footer test_count plan_count =
    DiagnosticNode(
        Diag(["Looks like you planned " 
            ^ (string_of_int plan_count) 
            ^ " tests but "
            ^ (if test_count < plan_count then
                  ("only ran " ^ (string_of_int test_count)) 
              else 
                  ("ran " ^ (string_of_int (test_count - plan_count)) ^ " extra"))
            ^ "."
        ])
    )

(* 
    TODO:
    Using @ to append these lists cannot be 
    very efficient. At the very least make sure
    that we use the ExtLib version which is 
    tail-recusive
*)
let init_document d = 
    let count    = count_tests         d in
    let failures = count_test_failures d in   
    match d with 
        | Document(PlanNode(plan)::nodes) -> 
            Document(
                PlanNode(plan)::nodes 
                @
                (if count = plan then [] else [ (create_count_footer count plan) ])                
                @
                (if failures = 0 then [] else [ (create_failure_footer count failures) ])
            )
        | Document(nodes) -> 
            Document(
                nodes 
                @
                [ PlanNode(count) ]
                @              
                (if failures = 0 then [] else [ (create_failure_footer count failures) ])
            )    

(* conversions *)

let string_of_status s =
    match s with 
        | Ok    -> "ok"
        | NotOk -> "not ok"

(*
    TODO:
    There are many chances below here for 
    using Format.sprintf
*)
let string_of_diagnostic = function Diag(lines) -> 
    List.fold_left (^) "" (List.map (fun line -> "# " ^ line ^ "\n") lines)
    
let string_of_directive  = function Todo(s) -> 
    " # TODO " ^ s 

let string_of_node node =
    let emit_diagnostic diag =
        (match diag with
              | None    -> "\n"
              | Some(d) -> "\n" ^ (string_of_diagnostic d))    
    in    
    match node with 
        | TestCaseNode(status, num, desc, diag) ->
            (string_of_status status) 
            ^ " " 
            ^ (string_of_int num) 
            ^ " - " 
            ^ desc
            ^ (emit_diagnostic diag)
        | TodoTestCaseNode(status, num, desc, dir, diag) ->
            (string_of_status status) 
            ^ " " 
            ^ (string_of_int num) 
            ^ " - " 
            ^ desc
            ^ (string_of_directive dir)
            ^ (emit_diagnostic diag) 
        | DiagnosticNode(diag) ->
            (string_of_diagnostic diag)
        | PlanNode(count) ->
            "1.." ^ (string_of_int count) ^ "\n"

let string_of_document = function Document(nodes) ->
    (String.concat "" (List.map string_of_node nodes))
    
