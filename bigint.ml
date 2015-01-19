(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Ryan Gliever -- rgliever@ucsc.edu *)

open Printf

module Bigint = struct
    
    (* type definitions for cmp and deter_sign functions *)
    type cmp      = LT | EQ | GT 
                   (* less than | equal to | greater than *)
    type deter_signs = P | PN | NP | N 
                    (* Pos, Pos | Pos, Neg | Neg, Pos | Neg, Neg *)

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let push      = Stack.push
    let pop       = Stack.pop
    let numlen    = List.length
    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
    
    (* print list for debugging *)
    let rec print_list list = match list with
        | [] -> printf "\n%!"
        | list -> (printf "%d%!" (car list); print_list (cdr list))

    (* chops leading zeros off of numbers after subtraction 
       e.g. 002 -> 2 *)
    let chop_zeros list =
        let rec chop_zeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
         (let cdr' = chop_zeros' cdr
         in match car, cdr' with
            | 0, []     -> []
            | car, cdr' -> car::cdr' )
        in chop_zeros' list
    
    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    
    (* cmp: compare two lists of integers and return EQ, LT or GT *)
    let rec cmp list1 list2 = 
        if numlen list1 = numlen list2  
        then (match (reverse list1, reverse list2) with
        | [], []       -> EQ
        | [], list2    -> LT
        | list1, []    -> GT
        | car1::cdr1, car2::cdr2    -> 
            (if car1 > car2 then GT
            else (if car1 < car2 then LT
                 else cmp (reverse cdr1) (reverse cdr2))) )
        else (if numlen list1 > numlen list2 then GT
             else LT )
    
    (* deter_signs: determine the signs of two signs of bigints *)
    let deter_signs sign1 sign2 = match sign1, sign2 with
        | Pos, Pos   -> P
        | Pos, Neg   -> PN
        | Neg, Pos   -> NP
        | Neg, Neg   -> N
    
    (* recursive add function *)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            (let sum = car1 + car2 + carry
            in sum mod radix :: add' cdr1 cdr2 (sum / radix))
    
    (* recursive sub function *)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            (match (cmp list1 list2) with
            | EQ      -> [0]
            | LT | GT ->
            (if car1 < (car2 + carry)
            then (let diff = (car1 + radix) - (car2 + carry)
                 in diff mod radix :: sub' cdr1 cdr2 1)
            else (let diff = car1 - (car2 + carry)
                 in diff mod radix :: sub' cdr1 cdr2 0) ) )
    
    (* recursive mul function *)
    let rec mul' left right powerof2 = 
        match (cmp powerof2 left) with
        | EQ | LT  -> 
            (let remainder, product = 
            mul'
             left (add' right right 0) (add' powerof2 powerof2 0)
            in (match (cmp remainder powerof2) with
                | EQ | GT   -> 
                   (chop_zeros(sub' remainder powerof2 0)), 
                   (add' product right 0)
                | LT        -> remainder, product ) )
        | GT       -> left, [0]

    (* recursive div / rem function *)
    let rec divrem' left right powerof2 = match (cmp right left) with
        | EQ | LT ->
            (let quotient, remainder = 
             divrem' 
             left (add' right right 0) (add' powerof2 powerof2 0)
            in match (cmp remainder right) with
            | EQ | GT -> 
             (add' quotient powerof2 0), 
             (chop_zeros(sub' remainder right 0))
            | LT      -> quotient, remainder )
        | GT      -> [0], left
    
    (* returns true if even, false if odd *)
    let even list = 
        let _, remainder = divrem' list [2] [1]
        in match (cmp remainder [0]) with
        | EQ -> true
        | LT | GT -> false
    
    (* recursive pow function *)
    let rec pow' base expt result =
        match expt with
        | []    -> result
        | expt when even expt -> 
            (let newexpt, _ = divrem' expt [2] [1]
            in let _, base2 = mul' base base [1]
               in pow' base2 newexpt result )
        | expt    -> 
            (match (cmp base result) with
            | EQ | LT -> (let _, baseresult = mul' base result [1]
                in pow' base (chop_zeros (sub' expt [1] 0)) baseresult)
            | GT      -> (let _, baseresult = mul' result base [1]
                in pow' base (chop_zeros (sub' expt [1] 0)) baseresult)
            )
    
    (* add two Bigints. calls recursive add *)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (deter_signs neg1 neg2) with
        |P      -> (Bigint (Pos, add' value1 value2 0))
        |PN     -> (match (cmp value1 value2) with
                   | EQ -> zero
                   | LT -> 
                    (Bigint (Neg, chop_zeros (sub' value2 value1 0)))
                   | GT -> 
                    (Bigint (Pos, chop_zeros (sub' value1 value2 0))) )
        |NP     -> (match (cmp value1 value2) with
                   | EQ -> zero
                   | LT -> 
                    (Bigint (Pos, chop_zeros (sub' value2 value1 0)))
                   | GT -> 
                    (Bigint (Neg, chop_zeros (sub' value1 value2 0))) )
        |N      -> (Bigint (Neg, add' value1 value2 0))
     
    (* subtract two Bigints. calls recursive sub *)  
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (deter_signs neg1 neg2) with
        | P      -> (match (cmp value1 value2) with
                    | EQ -> zero
                    | LT -> 
                     (Bigint (Neg, chop_zeros(sub' value2 value1 0)))
                    | GT -> 
                     (Bigint (Pos, chop_zeros(sub' value1 value2 0))) )
        | PN     -> (Bigint (Pos, add' value1 value2 0))
        | NP     -> (Bigint (Neg, add' value1 value2 0))
        | N      -> (match (cmp value1 value2) with
                    | EQ -> zero
                    | LT -> 
                     (Bigint (Pos, chop_zeros(sub' value2 value1 0)))
                    | GT -> 
                     (Bigint (Neg, chop_zeros(sub' value1 value2 0))) )
    
    (* multiply two Bigints. calls recursive mul *)
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        match (deter_signs neg1 neg2) with
        | P | N    -> (match (cmp value1 value2) with
                      | EQ 
                      | LT -> (let _, product = mul' value1 value2 [1]
                              in Bigint (Pos, product) )
                      | GT -> (let _, product = mul' value2 value1 [1]
                              in Bigint (Pos, product) ) )
        | PN | NP  -> (match (cmp value1 value2) with
                      | EQ 
                      | LT -> (let _, product = mul' value1 value2 [1]
                              in Bigint (Neg, product) )
                      | GT -> (let _, product = mul' value2 value1 [1]
                              in Bigint (Neg, product) ) )

    (* divide two Bigints. calls recursive div /rem *)
    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (deter_signs neg1 neg2) with
        | P | N ->
            (let quotient, _ = divrem' value1 value2 [1]
            in Bigint (Pos, quotient) )
        | PN | NP ->
            (let quotient, _ = divrem' value1 value2 [1]
            in Bigint (Neg, quotient) )
    
    (* find remainder (mod) of two Bigints. calls recursive div/rem *)
    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (deter_signs neg1 neg2) with
        | P           ->
            (let _, remainder = divrem' value1 value2 [1]
            in Bigint (Pos, remainder) )
        | N | PN | NP ->
            (let _, remainder = divrem' value1 value2 [1]
            in Bigint (Neg, remainder) )
    
    (* exponentiate one Bigint by another. calls recursive pow *)   
    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        match (deter_signs neg1 neg2) with
        | P      -> (Bigint (Pos, pow' value1 value2 [1]))
        | NP     ->
            (if even value2 then (Bigint (Pos, pow' value1 value2 [1]))
            else (Bigint (Neg, pow' value1 value2 [1])) )
        | N | PN -> zero

end
