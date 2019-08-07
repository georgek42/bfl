open Core
open Bfl

let help () =
  printf "
Baffle 0.0.1
Commands:
  run <program>.bf -- Interpret a brainfuck program
  compile <program>.bf <target_file>.c -- Compile a brainfuck program to C
  repl -- Start the brainfuck repl
"

exception Not_implemented

let () =
  match Array.length Sys.argv with
  | 2 -> (
    match Sys.argv.(1) with
    | "repl" -> raise Not_implemented
    | _ -> help ()
  )
  | 3 -> (
    match Sys.argv.(1) with
    | "run" -> (
      let source_filename = Sys.argv.(2) in
      In_channel.read_all source_filename |> Op.parse |> Eval.eval
    )
    | _ -> help ()
  )
  | 4 -> (
    match Sys.argv.(1) with
    | "compile" -> (
      let source_filename = Sys.argv.(2) in
      let target_filename = Sys.argv.(3) in
      In_channel.read_all source_filename |> Op.parse |> Compile.compile ~filename:(Some target_filename)
    )
    | _ -> help ()
  )
  | _ -> help ()