(*
 * TestBuilder - A module for building test functions 
 * Copyright (C) 2007 Infinity Interactive, Inc.
 *
 * This module is heavily influenced by the Perl Test::Builder module.
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
 
(* storage for current running tests and plan *)

let current_plan  = ref (None : int option)
let running_tests = ref ([] : (int -> TAPDocument.node) list)

(* public functions *)

let create_test_plan ?count () =
    (* NOTE: 
       should probably test to see if there is 
       already a plan in progrees and croak if
       so. However, that use-case is both rare
       and totally incorrect, so it's not that 
       important really.
     *)
    current_plan := count    

let build_test_case ?todo ?diag test desc =
    running_tests := (
        fun number -> 
            match todo with 
              | None   -> TAPDocument.TestCaseNode(
                            (if test then TAPDocument.Ok else TAPDocument.NotOk), 
                            number, 
                            desc, 
                            diag
                          )
              | Some x -> TAPDocument.TodoTestCaseNode(
                            (if test then TAPDocument.Ok else TAPDocument.NotOk), 
                            number, 
                            desc, 
                            TAPDocument.Todo(x), 
                            diag
                          )
    ) :: !running_tests

let build_diagnostic line = 
    running_tests := (
        fun (_ : int) -> (TAPDocument.DiagnosticNode (TAPDocument.Diag([ line ])))
    ) :: !running_tests    

(* 
    TODO:
    convert the two functions below to use 
    Format.sprintf for the entire string and
    save the overhead of concatination
*)
let build_extended_diagnostic_message test (got : 'a) (expected : 'a) description =
    if test then 
        None 
    else 
        Some(TAPDocument.Diag([ "Failed test '" ^ description ^ "'";    
                                "in " ^ (Array.get Sys.argv 0);
                                (Format.sprintf "     got: %s" (ExtLib.dump got));
                                (Format.sprintf "expected: %s" (ExtLib.dump expected));      
                              ]))

let build_diagnostic_message test description =
    if test then 
        None 
    else 
        Some(TAPDocument.Diag([ "Failed test '" ^ description ^ "'";    
                                "in " ^ (Array.get Sys.argv 0); 
                              ]))

(* internal functions *)

let rec number_tests tests acc count =
    match tests with 
      | []   -> List.rev acc
      | h::t -> let node = (h count) in 
                    match node with 
                        | TAPDocument.TestCaseNode(_,_,_,_)   
                        | TAPDocument.TodoTestCaseNode(_,_,_,_,_)
                            -> number_tests t (node::acc) (count + 1) 
                        | TAPDocument.DiagnosticNode(_)           
                        | TAPDocument.PlanNode(_) 
                            -> number_tests t (node::acc) count    

let build_document ?count tests =
    let d = match count with 
                | None    -> TAPDocument.Document((number_tests tests [] 1)) 
                | Some(n) -> TAPDocument.Document(
                                 TAPDocument.PlanNode(n)::(number_tests tests [] 1)
                             ) 
    in
    TAPDocument.init_document d

let assemble_test_run () =
    let tests = (List.rev !running_tests) in 
    match !current_plan with 
        | None       -> build_document tests
        | Some(plan) -> build_document ~count:plan tests

module Tester = (* TestBuilder.Tester *)
struct  

    (* This will run a test suite which is
       isolated within a function. It will 
       make sure to save and restore any 
       existing global state so that tests
       can be run within tests. *)
    let run_isolated_test_suite plan test_suite =
        let backup_tests = !running_tests and
            backup_plan  = !current_plan  
        in
        running_tests := ([] : (int -> TAPDocument.node) list);
        current_plan  := plan;
        test_suite ();
        let document = assemble_test_run () in
        running_tests := backup_tests;
        current_plan  := backup_plan;
        document

    let plan count test_suite = 
        let document = run_isolated_test_suite (Some count) test_suite in
        TAPDocument.string_of_document(document)

    let no_plan test_suite = 
        let document = run_isolated_test_suite None test_suite in
        TAPDocument.string_of_document(document)
        
end;;


(* register our exit handler to 
   print our test output with *)
at_exit (fun () -> 
    print_string(TAPDocument.string_of_document(assemble_test_run ()))
);;


