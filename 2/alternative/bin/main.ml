(* definition of operator *)
let ( <|> ) a b =
  match (a,b) with
  | (Some(c),_) -> Some(c)
  | (_,c) -> c
;;

assert (None <|> Some false = Some false);;
assert (Some true <|> None <|> Some false = Some true);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> None <|> Some "dog" <|> None = Some "dog");;
