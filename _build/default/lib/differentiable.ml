
(** Signature of a Differentiable activation function *)
module type Sig = sig
  val apply : Matrix.t -> Matrix.t
  val derivative : Matrix.t -> Matrix.t
end

module Sigmoid : Sig = struct
  let apply m =
    Matrix.map
      (fun n ->
         (1.) /.
         (1. +. exp (-. n)))
      m

  let derivative m =
    let ones = Matrix.ones ~m:(Matrix.height m) ~n:(Matrix.width m) in
    let res = apply m in
    Matrix.return @@ Matrix.map2 ( *. )
      res
      (Matrix.return @@ Matrix.sub ones res)
end

module Softmax : Sig = struct
  let max m =
    Matrix.fold_left
      (fun max_val v -> if v > max_val then v else max_val)
      0.
      m

  let apply m =
    let max_v = max m in
    (* Matrix.print m; *)
    let m' = Matrix.map (fun x -> x -. max_v) m in
    (* Matrix.print m'; *)
    (* Printf.printf "max: %f\n" max_v; *)
    let exps = Matrix.map (fun v -> exp v) m' in
    (* Matrix.print exps; *)
    let sum = Matrix.sum exps in
    (* Printf.printf "sum: %f\n" sum; *)
    let a = Matrix.map
        (fun x -> x /. sum)
        exps
    in
    a

  let derivative m =
    (* let sum = Matrix.sum @@ Matrix.map exp m in *)
    Matrix.map2 ( *. )
      m
      (Matrix.map (fun x -> 1. -. x) (apply m))
    |> Matrix.return
end