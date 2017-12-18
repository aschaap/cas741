(*open Prelude*)

(* open Runcode *)
(* open Print_code *)

(*   module Lang = Code *)

  (*let string_of_code gen : string = (*mimicking Print_code.format_code*)
    let code = .< fun a -> .~(runM (gen .<a>.) []) >. in
    let (cde, _) = close_code_delay_check code in
    let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
    Pprintast.string_of_expression cde*)

  (*let printcode gen =
    let code = .< fun a -> .~(runM (gen .<a>.) []) >. in
    let ppf = Format.std_formatter in
      print_endline "before-test";
(*       Print_code.print_code Format.std_formatter code; *)
      let (cde, check) = close_code_delay_check code in
(*       print_closed_code ppf cde; *)
      Format.fprintf ppf ".<@,%a>.@ " format_code cde;
(*       end print_closed_code *)
      print_newline ();
      print_endline "after-test";
      try check ()
      with e -> Format.fprintf ppf "\n%s" (Printexc.to_string e)
(*       end print_code *)*)

(*  let instantiate gen =
    let code = .< fun a -> .~(runM (gen .<a>.) []) >. in
(*     printcode gen; *)
    Runcode.run code*)
    
open OUnit2 (*use OCAMLFIND_COMMANDS="ocamlc=metaocamlc" ocamlfind ocamlc -w Ax -g -o <outputname> -package oUnit -linkpkg -g <inputname(s)>*)

open Rkgen

let test_f0 x y = 
    Array.init (Array.length y) (fun i -> 0. )
    
let testevalrk45_0 _ =
  let x0 = Array.init 1 (fun _ -> 0.0) in
  let result = evalrk45 1.0 0.1 x0 test_f0 in
  assert_equal
    (Array.init 1 (fun _ -> 0.0))
    result

(* assembling tests into suites *)
let rksuite =
  "RK generator test suite">:::
  [
    "Simple constant function">:: testevalrk45_0
  ];;

(* running test suites *)
let () =
    run_test_tt_main rksuite;

