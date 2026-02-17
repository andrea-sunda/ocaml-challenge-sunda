type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let is_valid = function
  Card(v, _) -> v >= 1 && v <= 10
;;

let is_complete lst =
    let len = List.length(List.fold_left (fun acc -> fun x -> if (List.mem x acc || (not (is_valid x))) then acc else (x :: acc)) [] lst ) in
    len = 40
;;

let gen_deck () =
  let random_suit () =
    match Random.int 4 with
    | 0 -> Spades
    | 1 -> Hearts
    | 2 -> Diamonds
    | _ -> Clubs
  in
  let rec build acc =
    if List.length acc = 40 then 
      acc
    else
      let new_card = Card(Random.int 10 + 1, random_suit ()) in
      if List.mem new_card acc then
        build acc 
      else
        build (new_card :: acc)
  in
  build []
;;
