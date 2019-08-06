open Core
open Bfl

let () =
  let filename = Sys.argv.(1) in
  let prog = In_channel.read_all filename |> Op.parse in
  Eval.eval prog