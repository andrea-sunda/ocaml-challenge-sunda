(* Solution with a tuple *)
type person = Person of (string * int * string option);;

let make_person name age email =
  if age < 0 || age > 150
    then failwith("Age not valid, it must be in the range of 0-150.")
  else
    Person(name, age, email)
;;

let get_email =
  function | Person(a,b,c) -> c
;;

let can_send_adult_email = function
  | Person(a,b,Some(c)) when b >= 18-> true
  | _ -> false
;;

(* Solution with a record *)
type person = {
  name : string;
  age : int;
  email : string option;
}

let make_person name age email =
  if age < 0 || age > 150
    then failwith("Age not valid, it must be in the range of 0-150.")
  else
    {name = name; age = age; email = email}
;;

let get_email p = p.email;;


(* Solution with pattern matching  *)
let can_send_adult_email p =
  match p with
  | {name = x; age = y; email = Some(z)} when y >= 18 -> true
  | _ -> false
;;

(* Solution with expression *)
let can_send_adult_email p = p.age >= 18 && p.email != None;;
