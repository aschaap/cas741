open Prelude

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

let pf = function
      Left x -> begin print_string x; print_endline "" end;
    | Right x -> begin print_float x; print_endline "" end;;

(*let List.iter (fun x -> pf (b 0 x))
        [-2.1; -2.0; -1.97; -0.97; 0.1; 1.82; 1.84; 1.93; 2.0; 2.01]*)
    
let testevalrk45_0 _ =
  let xk = Array.init 1 (fun _ -> 0.0) in
  let h = 0.1 in
  let tk = 1.0 in
  let result = evalrk45 tk h xk test_f0 in
  assert_equal
    (Array.init 1 (fun _ -> 0.0))
    result

let testrk45simpleode _ =
  let x0 = Array.init 1 (fun _ -> 1.0) in
  let f _ x = Array.init (Array.length x) (fun i -> 5.0 *. x.(i) -. 3.0 ) in
  let h = 0.1 in
  let t0 = 2.0
  and tn = 3.0 in
  let numknots = int_of_float ((tn -. t0) /. h) in
  let code = odesolve t0 tn numknots x0 f in
  List.iter (fun x -> pf ((Runcode.run code) 0 x))
        [-1024.0; 0.0; 2.0; 2.2; 2.5; 3.0; 23.0]

(* assembling tests into suites *)
let rksuite =
  "RK generator test suite">:::
  [
    "Simple constant function">:: testevalrk45_0
  ];;

(* running test suites *)
let () =
    run_test_tt_main rksuite;
    testrk45simpleode ()

