open Ann

module PLayer = Perceptron.Layer(Differentiable.Sigmoid)
module PNet = Network.Make(PLayer)

let ( @.@ ) = PNet.(@.@)

let () = Random.self_init ()

let data =
  Printf.printf "Reading...%!";
  let m = Parse.matrix_of_file "examples/mnist/mnist.csv" in
  Printf.printf "DONE!\n";
  m

let net =
    PLayer.create_layer ~inputs:784 ~outputs:32
    @.@ PNet.of_layer @@ PLayer.create_layer ~inputs:32 ~outputs:10

let loss_func actual expected =
  (1. /. 2.) *. (Matrix.norm @@ Matrix.return @@ Matrix.sub expected actual)

let loss_func_deriv actual expected =
  Matrix.return @@ Matrix.sub expected actual

let _ =
  let net =
    PNet.train
      ~epsilon:(0.1)
      ~learning:(0.1)
      ~max_epoch:(Some 10)
      ~gradient_type:(MiniBatch 128)
      ~output_file:(Some "mnist_results.csv")
      (* ~dropout:(0.5) *)
      ~loss_func
      ~loss_func_deriv
      net data
  in
  (* PNet.save net "mnist"; *)
  Printf.printf "accuracy: %f\n" @@ PNet.eval_all net data
