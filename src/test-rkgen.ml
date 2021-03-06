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

let test_f0 _ y = 
    Array.init (Array.length y) (fun _ -> 0. )

let pf = function
      Left x -> begin print_string x; print_endline "" end;
    | Right x -> begin print_float x; print_endline "" end;;

(*let List.iter (fun x -> pf (b 0 x))
        [-2.1; -2.0; -1.97; -0.97; 0.1; 1.82; 1.84; 1.93; 2.0; 2.01]*)
let numknots t0 tn h = int_of_float ((tn -. t0) /. h)

let testevalrk45_0 _ =
  let xk = Array.init 1 (fun _ -> 0.0) in
  let h = 0.1 in
  let tk = 1.0 in
  let result = evalrk45 tk h xk test_f0 in
  assert_equal
    (Array.init 1 (fun _ -> 0.0))
    result

let testrksimpleode order =
  let x0 = Array.init 1 (fun _ -> 1.0) in
  let f _ x = Array.init (Array.length x) (fun i -> 5.0 *. x.(i) -. 3.0 ) in
  let h = 0.1 in
  let t0 = 2.0
  and tn = 3.0 in
  let code = odesolve order t0 tn (numknots t0 tn h) x0 f in
  List.iter (fun x -> pf ((Runcode.run code) 0 x))
        [-1024.0; 0.0; 2.0; 2.2; 2.5; 3.0; 23.0]

let testrkhighorderode order =
  let x0 = Array.init 1 (fun _ -> 3.0) in
  let f t x = Array.init (Array.length x) (fun i -> 7.0 *. x.(i) *. x.(i) *. t *. t *. t ) in
  let h = 0.1 in
  let t0 = 2.0
  and tn = 4.0 in
  let code = odesolve order t0 tn (numknots t0 tn h) x0 f in
  List.iter (fun x -> pf ((Runcode.run code) 0 x))
        [-89.0; 0.0; 2.0; 2.5; 3.0; 8192.0]


let testrkstiffode order =
  let x0 = Array.init 1 (fun _ -> 1.0) in
  let f _ x = Array.init (Array.length x) (fun i -> -15.0 *. x.(i)) in
  let h = 0.1 in
  let t0 = 0.0
  and tn = 1.0 in
  let code = odesolve order t0 tn (numknots t0 tn h) x0 f in
  List.iter (fun x -> pf ((Runcode.run code) 0 x))
        [-1.0; 0.0; 0.4; 1.0; 1.5]

(* assembling tests into suites *)
let rksuite =
  "RK generator test suite">:::
  [
    "Simple constant function">:: testevalrk45_0
  ];;

(* running test suites *)
let () =
    run_test_tt_main rksuite;
    testrksimpleode 2;
    print_endline "---";
    testrksimpleode 4;
    print_endline "---";
    testrkhighorderode 2;
    print_endline "---";
    testrkhighorderode 4;
    print_endline "---";
    testrkstiffode 2;
    print_endline "---";
    testrkstiffode 4;

