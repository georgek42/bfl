type op =
  | MovR
  | MovL
  | Incr
  | Decr
  | Write
  | Read
  | Jz
  | Jnz

let to_char (op: op): char =
  match op with
  | MovR -> '>'
  | MovL -> '<'
  | Incr -> '+'
  | Decr -> '-'
  | Write -> '.'
  | Read -> ','
  | Jz -> '['
  | Jnz -> ']'

let of_char (c: char): op option =
  match c with
  | '>' -> Some MovR
  | '<' -> Some MovL
  | '+' -> Some Incr
  | '-' -> Some Decr
  | '.' -> Some Write
  | ',' -> Some Read
  | '[' -> Some Jz
  | ']' -> Some Jnz
  | _ -> None

let%test "op_serialize_roundtrip" =
  let open Core in
  let ops = [
    MovR;
    MovL;
    Incr;
    Decr;
    Write;
    Read;
    Jz;
    Jnz
  ] in
  let ops_c = ops |> List.map ~f:to_char in
  let ops' = ops_c |> List.map ~f:of_char in
  match List.zip ops ops' with
  | None -> false
  | Some ops -> ops
    |> List.fold ~init:true ~f:(fun acc (op, op') -> match op' with | Some op' -> acc && (op = op') | None -> false)

type program = op array

let parse (src: string): program =
  let open Core in
  src
  |> String.to_array
  |> Array.map ~f:of_char
  |> Array.filter ~f:(fun a -> match a with | Some _ -> true | None -> false)
  |> Array.map ~f:(fun a -> match a with | Some op -> op | None -> Jnz)

let%test "prog_parse" =
  let open Core in
  let prog = "
  ++++++++               Set Cell #0 to 8
  [
      >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
      [                   as the cell will be cleared by the loop
          >++             Add 2 to Cell #2
          >+++            Add 3 to Cell #3
          >+++            Add 3 to Cell #4
          >+              Add 1 to Cell #5
          <<<<-           Decrement the loop counter in Cell #1
      ]                   Loop till Cell #1 is zero; number of iterations is 4
      >+                  Add 1 to Cell #2
      >+                  Add 1 to Cell #3
      >-                  Subtract 1 from Cell #4
      >>+                 Add 1 to Cell #6
      [<]                 Move back to the first zero cell you find; this will
                          be Cell #1 which was cleared by the previous loop
      <-                  Decrement the loop Counter in Cell #0
  ]                       Loop till Cell #0 is zero; number of iterations is 8
  
  The result of this is:
  Cell No :   0   1   2   3   4   5   6
  Contents:   0   0  72 104  88  32   8
  Pointer :   ^
  
  >>.                     Cell #2 has value 72 which is 'H'
  >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
  +++++++..+++.           Likewise for 'llo' from Cell #3
  >>.                     Cell #5 is 32 for the space
  <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
  <.                      Cell #3 was set to 'o' from the end of 'Hello'
  +++.------.--------.    Cell #3 for 'rl' and 'd'
  >>+.                    Add 1 to Cell #5 gives us an exclamation point
  >++.                    And finally a newline from Cell #6" in
  let parsed = parse prog in
  let compact = parsed |> Array.map ~f:to_char |> Array.to_list |> String.of_char_list in
  compact = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."