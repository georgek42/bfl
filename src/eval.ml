open Core
open Op

let tape_size = 10_000_000

let tape = Array.create ~len:tape_size 0
let pos = ref 0
let pc = ref 0

exception Fault of string

let eval (op: op) (p: program) =
  match op with
  | MovR -> incr pos; incr pc
  | MovL -> decr pos; incr pc
  | Incr -> tape.(!pos) <- tape.(!pos) + 1; incr pc
  | Decr -> tape.(!pos) <- tape.(!pos) - 1; incr pc
  | Write -> printf "%c" (tape.(!pos) |> Char.of_int_exn); incr pc
  | Read -> (
    match In_channel.input_byte In_channel.stdin with
    | Some x -> tape.(!pos) <- x; incr pc
    | None -> tape.(!pos) <- 0; incr pc
  )
  | Jz -> (
    match tape.(!pos) with
    | 0 -> (
      let found = ref false in
      let c = ref 0 in
      let pc' = ref (!pc + 1) in
      let len = Array.length p in
      while not !found do
        if !pc' >= len then raise (Fault "matching ] not found") else begin
          match p.(!pc') with
          | Jz -> incr c; incr pc'
          | Jnz -> (
            match !c with
            | 0 -> found := true
            | _ -> decr c; incr pc'
          )
          | _ -> incr pc'
        end
      done;
      pc := !pc'
    )
    | _ -> incr pc
  )
  | Jnz -> (
    match tape.(!pos) with
    | 0 -> incr pc
    | _ -> (
      let found = ref false in
      let c = ref 0 in
      let pc' = ref (!pc - 1) in
      while not !found do
        if !pc' = 0 then raise (Fault "matching [ not found") else begin
          match p.(!pc') with
          | Jnz -> incr c; decr pc'
          | Jz -> (
            match !c with
            | 0 -> found := true
            | _ -> decr c; decr pc'
          )
          | _ -> decr pc'
        end
      done;
      pc := !pc'
    )
  )

let eval (p: program) =
  let len = Array.length p in
  while !pc < len do
    eval p.(!pc) p
  done