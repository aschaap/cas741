open Prelude

(* runge kutta 4th order integration *)
let evalrk45 tk h xk f =
  let l = Array.length xk in
  let hh = h *. 0.5 in
  let tkh = tk +. hh in (*point between t_k and t_{k+1}*)
  let k1 = f tk xk in
  let xt = Array.init l (fun i -> xk.(i) +. hh *. k1.(i)) in
  let k2 = f tkh xt in
  let xt = Array.init l (fun i -> xk.(i) +. hh *. k2.(i)) in
  let k3 = f tkh xt in
  let xt = Array.init l (fun i -> xk.(i) +. h *. k3.(i)) in
  let k2k3 = Array.init l (fun i -> k3.(i) +. k2.(i)) in
  let k4 = f (tk+.h) xt in
  let h6 = h /. 6.0 in
  Array.init l (fun i ->
    xk.(i) +. h6 *. (k1.(i)+.k4.(i)+.2.0*.k2k3.(i)) )

(* runge kutta 2nd order integration *)
let evalrk23 tk h xk f =
  let l = Array.length xk in
  let k1 = f tk xk in
  let y1 = Array.init l (fun i -> xk.(i) +. h *. k1.(i)) in
  let k2 = f (tk+.h) y1 in
  let h2 = h /. 2.0 in
  Array.init l (fun i ->
    xk.(i) +. h2 *. (k1.(i)+.k2.(i)) )

let rk xknot yin f evalrk =
  let xknot_l = Array.length xknot 
  and yin_l = Array.length yin in
  (* Make the output array *)
  let yout = Array.make_matrix xknot_l yin_l 0.0 in
  (* yout.(0) = yin *)
  yout.(0) <- yin;
  (* For each knot, we perform r.k. 4th order integration *)
  for i = 0 to xknot_l - 2 do
    let dx = xknot.(i+1) -. xknot.(i) in
    yout.(i+1) <- evalrk (xknot.(i)) dx yout.(i) f;
  done;
  yout

let find_index num knots x =
  let n1 = num-1 and n2 = num-2 in
  let scale = knots.(n1) -. knots.(0) in
  let k0 = knots.(0) in
  let fn1 = float_of_int num in
  let mult = fn1 /. scale in
  .< min (int_of_float ((.~x -. k0) *. mult)) n2 >.


let constructs a b k y n =
  let bod i = .< fun x -> 
    if ((x<a) || (x>b)) then
      Left "Error: x not in range"
    else
      Right (y.(
        .~(find_index n k .<x>.)).(.~i) ) >.
    in
  .< fun i -> .~(bod .<i>.) >. ;;
       
let odesolve order a b num_knots yin f =
  (* Create num_knots equally spaced knots across a..b *)
  if num_knots < 2 then
  begin
      print_string "Error: There must be at least 2 knots";
      print_endline ""
  end;
  if order <> 2 && order <> 4 then
  begin
    print_string "Error: RK method must either be 2 for 2nd order or 4 for 4th order";
    print_endline ""
  end;
  let knots = Array.init num_knots
      (fun i -> (a +. ((b -. a) *. (float_of_int
      i /. float_of_int (num_knots - 1))))) in
  (* Compute the integrated values at each knot starting with yin *)
  let evalrk = if (order == 2) then evalrk23 else evalrk45 in
  let youts = rk knots yin f evalrk in
  (* Construct the ode solution as a function that takes input x in a..b*)
  constructs a b knots youts num_knots;;
