open Core
open Op
open Codegen

exception Compile_exception of string

let labels = ref 0
let fresh_label () = incr labels; !labels |> sprintf "label%d"

let offset_labels: (int, string) Hashtbl.t = Hashtbl.create (module Int)

let compile ?(filename=None) (p: program) =
  let c = new codegen in
  Array.iteri p ~f:(fun pc op -> (
    match op with
    | MovR -> c#movR
    | MovL -> c#movL
    | Incr -> c#incr
    | Decr -> c#decr
    | Write -> c#write
    | Read -> c#read
    | Jz -> (
      let current_label = fresh_label () in
      Hashtbl.set offset_labels ~key:pc ~data:current_label;
      c#label current_label;
      let found = ref false in
      let count = ref 0 in
      let pc' = ref (pc + 1) in
      let length = Array.length p in
      while not !found do
        if !pc' > length then raise (Compile_exception "matching ] not found") else begin
          match p.(!pc') with
          | Jz -> incr count; incr pc'
          | Jnz -> (
            match !count with
            | 0 -> found := true
            | _ -> decr count; incr pc'
          )
          | _ -> incr pc'
        end
      done;
      let target_label = fresh_label () in
      Hashtbl.set offset_labels ~key:!pc' ~data:target_label;
      c#jz target_label;
    )
    | Jnz -> (
      match Hashtbl.find offset_labels pc with
      | None -> raise (Compile_exception "dangling ] found")
      | Some label -> (
        c#label label;
        let found = ref false in
        let count = ref 0 in
        let pc' = ref (pc - 1) in
        while not !found do
          if !pc' < 0 then raise (Compile_exception "matching [ not found") else begin
            match p.(!pc') with
            | Jnz -> incr count; decr pc'
            | Jz -> (
              match !count with
              | 0 -> found := true
              | _ -> decr count; decr pc'
            )
            | _ -> decr pc'
          end
        done;
        match Hashtbl.find offset_labels !pc' with
        | None -> raise (Compile_exception "no label for matching [ found")
        | Some label -> c#jnz label
      )
    )
  ));
  let code = c#emit in
  match filename with
  | Some f -> Out_channel.write_all f ~data:code
  | None -> printf "%s" code