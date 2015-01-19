(* $Id: maindc.ml,v 1.5 2014-12-02 19:13:07-08 - - $ *)
(* Ryan Gliever -- rgliever@ucsc.edu *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

(* register array *)
let register = Array.make 256 Bigint.zero;;

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint
let strlen    = String.length
let strsub    = String.sub

(* print number *)
let print_number number = 
    let string = (string_of_bigint number)
    in (let rec print_string str =
        if strlen str > 69
        then (printf "%s\\\n%!" (strsub str 0 69);
             print_string (strsub str 69 (strlen str - 69) ))
        else printf "%s\n%!" str;
        in print_string string)

let print_stackempty () = printf "dc: stack empty\n%!"

(* quit program *)
let quit_program () = exit 0

(* execute reg: s = store, l = load from register *)
let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> (let num = register.(reg)
                 in push num thestack )
        | 's' -> (try let num = pop thestack
                      in register.(reg) <- num
                 with Stack.Empty -> print_stackempty() )
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

(* execute binop: executes oper on top two stack elements *)
let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

(* execute: calls appropriate functions for operators *)
let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 's'  -> failwith "operator s scanned with no register"
        | 'q'  -> quit_program()
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

(* scans inputchannel and calls functions accordingly *)
let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> quit_program();
    in  toploop ()

(* passes either a file to be read or stdin 
   to the top loop for operations *)
let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

