type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT
type command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT | LESSTHAN | EQUAL | IF | BIND


let interpreter ((input : string), (output : string )) : unit =

  let ic = open_in input
  in

  let oc = open_out output
  in let file_write str_val = Printf.fprintf oc "%s" str_val
  in
  
  let rec loop_read acc =
    try
      let l = String.trim (input_line ic) in loop_read (l::acc)
    with
      | End_of_file -> List.rev acc
  in
  

  let strList = loop_read []


  in

  close_in ic;



let quo s =
  match String.split_on_char '"' s with
  | [""; accWord; ""]->accWord
  | _::_::_ -> s
  | _::[] -> s
  | [] -> s

in


let intStringOrName s =

  let firstLetter = String.sub s 0 1 in


  if (firstLetter >= "0" && firstLetter <= "9") || firstLetter = "-" then
    
    if String.contains s '.' then
      

      ERROR

    else

      INT (int_of_string s)

  else if firstLetter = "\"" then

    STRING (quo s)
  else
      NAME s
    



in   

let str2sv s =

  match s with
  | ":true:" -> BOOL true

  | ":false:" -> BOOL false


  | ":error:" -> ERROR
  | ":unit:" -> UNIT

  | _ -> intStringOrName (s)


          
in
  

let string2command s =
  match s with
  | "add" ->ADD
  | "sub" ->SUB
  | "mul" ->MUL
  | "div" ->DIV
  | "pop" ->POP
  | "rem" ->REM
  | "neg" ->NEG
  | "swap" ->SWAP
  | "toString" ->TOSTRING
  | "println" ->PRINTLN
  | "quit" ->QUIT
  | "cat" ->CAT
  | "and" ->AND
  | "or" ->OR
  | "not" ->NOT
  | "lessThan" ->LESSTHAN
  | "equal" ->EQUAL

  | "if" ->IF
  | "bind" ->BIND

  | _ ->
      match String.sub s 0 4 with
      | "push" -> PUSH(str2sv(String.sub s 5 (String.length s-5)))
  | _ ->QUIT

      




in


let sv2str v =
  match v with
  | BOOL true -> ":true:"
  | BOOL false-> ":false:"

  | INT integer -> string_of_int (integer)
  | ERROR -> ":error:"
  | STRING lue->lue 
  | NAME lue-> lue
  | UNIT ->":unit:"


in





let rec map f xs =
  match xs with
  | [] -> []
  | h :: tl -> (f h) :: (map f tl)



in

let comList =map string2command strList in



let empty = fun y ->None in

let lookup key env = env key in

let add key value env = fun y ->if y = key then Some value else env y in


let rec listLOokup nme mList =
  match mList with

  | [] -> None
  | m::rest->

      match lookup nme m with
      | None-> listLOokup nme rest
      | lue-> lue


in
  
let prevBoundName nme mList =
  
  match listLOokup nme mList with
  | Some lue-> lue

  | None -> NAME nme 




in
  
let getValfromName lue m =
  match lue with
  | NAME nme-> prevBoundName nme m

  | whatevValue ->whatevValue


in

let topEnvAdd nme lue mList =
  match mList with


  | []->[add nme lue empty]

  | m::rest->(add nme lue m)::rest


in

let rec processer (commandList,stack,m) =


  match (commandList,stack,m) with

  | (ADD::restOfCommands,INT(a)::INT(b)::restOfStack,m)-> processer (restOfCommands, (INT(b+a) ::restOfStack), m)
  | (ADD::restOfCommands,s,m) -> processer (restOfCommands,(ERROR::stack), m)

      
  | (SUB::restOfCommands, INT(a)::INT(b)::restOfStack, m) -> processer (restOfCommands, (INT(b-a)::restOfStack), m)
  | (SUB::restOfCommands, s, m) -> processer (restOfCommands, (ERROR::stack), m)
      

  | (MUL::restOfCommands, INT(a)::INT(b)::restOfStack, m) -> processer (restOfCommands, (INT(b*a) :: restOfStack), m)
  | (MUL::restOfCommands, s, m) -> processer (restOfCommands, (ERROR::stack), m)


  | (DIV::restOfCommands, INT(0)::INT(a)::restOfStack,m) -> processer (restOfCommands, (ERROR::stack), m)

  | (DIV::restOfCommands, INT(a)::INT(b)::restOfStack, m) -> processer (restOfCommands, (INT(b/a):: restOfStack), m)
  | (DIV::restOfCommands, s, m) -> processer (restOfCommands, (ERROR::stack),m)


  | (PUSH(whatevstr)::restOfCommands,stack,m) ->processer (restOfCommands,(getValfromName whatevstr m)::stack,m)

  | (POP::restOfCommands,a::restOfStack,m) ->processer (restOfCommands,restOfStack, m)
  | (POP::restOfCommands, s,m) -> processer (restOfCommands,(ERROR::stack), m)
  
  | (REM::restOfCommands, INT(0)::INT(a)::restOfStack, m) -> processer (restOfCommands, (ERROR::stack),m)
  
  | (REM::restOfCommands, INT(a)::INT(b)::restOfStack, m)-> processer (restOfCommands, (INT(b mod a)::restOfStack),m)
  | (REM::restOfCommands, s, m) -> processer (restOfCommands, (ERROR::stack), m)

      
  | (NEG::restOfCommands, INT(a)::restOfStack,m) -> processer (restOfCommands, (INT(-a)::restOfStack), m)
  | (NEG::restOfCommands,s, m) ->processer (restOfCommands,(ERROR::stack), m)
      

  | (SWAP::restOfCommands, a::b::restOfStack,m) -> processer (restOfCommands, (b::a::restOfStack), m)
  | (SWAP::restOfCommands,s,m) -> processer (restOfCommands, (ERROR::stack), m)
      
  
  
  | (TOSTRING::restOfCommands, a::restOfStack,m) ->processer (restOfCommands,(STRING(sv2str a)::restOfStack), m)
  | (TOSTRING::restOfCommands, s,m) -> processer (restOfCommands,(ERROR::stack), m)


  | (PRINTLN::restOfCommands,STRING(str)::restOfStack, m) -> file_write str; file_write "\n";processer (restOfCommands,restOfStack, m)
  | (PRINTLN::restOfCommands, s,m) -> processer (restOfCommands, (ERROR::stack),m)

      
  | (CAT::restOfCommands, STRING(a)::STRING(b)::restOfStack,m) -> processer (restOfCommands,(STRING (b^a)::restOfStack), m)
  | (CAT::restOfCommands,s, m) -> processer (restOfCommands,(ERROR::stack), m)
      

  | (AND::restOfCommands,BOOL(a)::BOOL(b)::restOfStack,m)-> processer (restOfCommands,(BOOL(b && a)::restOfStack),m)
  | (AND::restOfCommands,s,m)-> processer (restOfCommands, (ERROR::stack),m)

      
  | (OR::restOfCommands,BOOL(a)::BOOL(b)::restOfStack, m) ->processer (restOfCommands, (BOOL (b || a)::restOfStack),m)
  | (OR::restOfCommands,s, m) ->processer(restOfCommands, (ERROR::stack), m)

      
  | (NOT::restOfCommands, BOOL(a)::restOfStack, m) -> processer (restOfCommands,(BOOL (not a)::restOfStack), m)
  | (NOT::restOfCommands, s, m) -> processer (restOfCommands,(ERROR::stack),m)
      
  | (LESSTHAN::restOfCommands, INT(a)::INT(b)::restOfStack, m)-> processer (restOfCommands, (BOOL (b < a)::restOfStack), m)
  | (LESSTHAN::restOfCommands, s, m)-> processer(restOfCommands, (ERROR::stack), m)

  | (EQUAL::restOfCommands,INT(a)::INT(b)::restOfStack,m) -> processer (restOfCommands, (BOOL(b = a)::restOfStack), m)
  | (EQUAL::restOfCommands,s,m) -> processer (restOfCommands,(ERROR::stack), m)
      

  | (IF::restOfCommands,x::y::BOOL(true)::restOfStack, m) -> processer (restOfCommands, (x::restOfStack), m)
  | (IF::restOfCommands,x::y::BOOL(false)::restOfStack, m) -> processer (restOfCommands, (y::restOfStack), m)
  | (IF::restOfCommands,s,m) -> processer (restOfCommands, (ERROR::stack), m)
      

  | (BIND::restOfCommands,v::NAME name:: restOfStack, m)->if getValfromName v m=NAME name then processer (restOfCommands,ERROR::NAME name::v::restOfStack,m)
    else processer(restOfCommands,UNIT::restOfStack,topEnvAdd name (getValfromName v m)m)

  | (BIND::restOfCommands,s,m) ->processer (restOfCommands, (ERROR::stack),m)


  | (QUIT::_, stack, m) -> close_out oc; UNIT
  | (_, _, _) -> UNIT  
 


in

  let _ = processer (comList, [], [empty]) in ()  



(*
let () = interpreter ("input1.txt", "output1.txt")
let () = interpreter ("input2.txt", "output2.txt")
let () = interpreter ("input3.txt", "output3.txt")
let () = interpreter ("input4.txt", "output4.txt")
let () = interpreter ("input5.txt", "output5.txt")
let () = interpreter ("input6.txt", "output6.txt")
let () = interpreter ("input7.txt", "output7.txt")
let () = interpreter ("input8.txt", "output8.txt")
let () = interpreter ("input9.txt", "output9.txt")
let () = interpreter ("input10.txt", "output10.txt")
*)

