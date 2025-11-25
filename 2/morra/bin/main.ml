Random.self_init ();;

type winner = Player | Computer | Tie ;;

let win (hp,gp) =
  let hand = Random.int 6 in
  let guess = Random.int 11 in
  let sum = hp + hand in
  match (gp = sum, guess = sum) with
  | (true, false) -> ((hand,guess), Player)
  | (false, true) -> ((hand,guess), Computer)
  | _ -> ((hand,guess), Tie)
;;
