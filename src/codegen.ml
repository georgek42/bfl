open Core

let prelude = "
char tape[10000000];
char *pos = &tape;

int main() {
"

let movR = "pos++;"
let movL = "pos--;"
let doIncr = "(*pos)++;"
let doDecr = "(*pos)--;"
let write = "write(1, pos, 1);"
let read = "read(0, pos, 1);"
let jz dest = sprintf "if(!*pos) goto %s;" dest
let jnz dest = sprintf "if(*pos) goto %s;" dest
let label lbl = sprintf "%s:" lbl

class codegen =
  object
  val mutable code = ([prelude]: string list)
  method movR =
    code <- movR :: code
  method movL =
    code <- movL :: code
  method incr =
    code <- doIncr :: code
  method decr =
    code <- doDecr :: code
  method write =
    code <- write :: code
  method read =
    code <- read :: code
  method jz lbl =
    code <- (jz lbl) :: code
  method jnz lbl =
    code <- (jnz lbl) :: code
  method label lbl =
    code <- (label lbl) :: code
  method emit =
    "}" :: code |> List.rev |> String.concat ~sep:"\n"
end