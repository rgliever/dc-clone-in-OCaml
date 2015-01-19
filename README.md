# dc-clone-in-OCaml

This OCaml program mimicks the 'dc' utility in UNIX.
http://en.wikipedia.org/wiki/Dc_%28computer_program%29

To use, run 'make' or 'gmake' in UNIX with the given source code. 
This will generate an executable, 'ocamldc', which can be run from the command line.
Once ran, you will be in the ocamldc prompt, where you can issue a number of commands.
The program pushes user input numbers onto a stack, and performs binary operations
on the two top-most elements.

Commands:
  any number: pushes the number onto the stack
  q: quits ocamldc
  p: prints the top stack element
  f: prints the entire stack
  c: clears the stack
  d: pushes the top-most element onto the stack (duplicate)
  s(char): pops the top-most stack element and stores it in a register specified by
    (char); example: 'sx' pops the top element and stores it in register x.
  l(char): loads the number from register (char) if one exists in that register and 
    pushes it onto the stack; example: 'lx' pushes the element in reg x onto the stack.
  +: pops the two top-most elements, adds them together, and pushes the sum on the stack.
  -: pops the two top-most elements, subtracts the top-most by the second top-most, 
    and pushes the difference onto the stack.
  /: pops the two top-most elements, divides the second top-most by the top-most, and 
    pushes the quotient onto the stack.
  *: pops the two top-most elements, multiplies the top-most by the second top-most, and
    pushes the product onto the stack.
  %: pops the two top-most elements, divides the second top-most by the top-most, and
    pushes the remainder onto the stack (mod).
  ^: pops the two top-most elements, raises the second top-most element to the power of
    the top-most element, and pushes the result onto the stack.
  

