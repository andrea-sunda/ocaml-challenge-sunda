type bitstring = E | Z of bitstring | U of bitstring;;

(* converts a bitstring into a string *)
let rec string_of_bitstring = function
  | E -> ""
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^  string_of_bitstring s
;;

(* computes the length of a bitstring *)
let rec len = function
  | E -> 0
  | Z s -> 1 + len s
  | U s -> 1 + len s
;;

(* count the number of 0s *)
let rec countZ = function
  | E -> 0
  | Z s -> 1 + countZ s
  | U s -> 0 + countZ s
;;

(* count the number of 1s *)
let rec countU = function
  | E -> 0
  | Z s -> 0 + countU s
  | U s -> 1 + countU s
;;

(* concatenates two bitstrings *)
let rec concat s1 s2 =
  match s1 with
  | E -> s2
  | Z s -> Z (concat s s2)
  | U s -> U (concat s s2)
;;

(* checks if two bitstrings are equal *)
let rec equals str1 str2 =
  match (str1, str2) with
    | (E, E) -> true
    | (Z s1, Z s2) -> equals s1 s2
    | (U s1, U s2) -> equals s1 s2
    | _ -> false
;;

(* gets the tail of a bitstring *)
let tl = function
  | E -> E
  | Z s -> s
  | U s -> s
;;

(* checks if str1 is a prefix of str2 *)
let rec prefix str1 str2 =
  match (str1, str2) with
  | (E, _) -> true
  | (Z s1, Z s2) -> prefix s1 s2
  | (U s1, U s2) -> prefix s1 s2
  | _ -> false
;;

let rec substring s1 s2 =
  prefix s1 s2 || (match s2 with
                   | E -> false
                   | Z s | U s -> substring s1 s)
;;
