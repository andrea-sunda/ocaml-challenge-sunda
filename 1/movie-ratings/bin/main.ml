(* First version with pattern matching *)
let movie_rating (g0 : int) (g1 : int) (g2 : int) : string =
  if g0 < 1 || g0 > 5 || g1 < 1 || g1 > 5 || g2 < 1 || g2 > 5
    then failwith("Ratings out of bounds")
  else
    let sum_ratings = g0 + g1 + g2 in
    match sum_ratings with
    | 15 -> "Masterpiece"
    | 14 -> "Higly Recommended"
    | x when ((g0 >= 4 && g1 >= 4 && g2 >= 3) || (g1 >= 4 && g2 >= 4 && g0 >= 3) || (g0 >= 4 && g2 >= 4 && g1 >= 3))-> "Recommended"
    | _ -> "Mixed Reviews"
;;

(* Second version with if-else and count_ge function that counts how many grades are greater than a constant *)
let movie_rating (g0 : int) (g1 : int) (g2 : int) : string =
  if g0 < 1 || g0 > 5 || g1 < 1 || g1 > 5 || g2 < 1 || g2 > 5
    then failwith("Ratings out of bounds")
  else
    let count_gt (g0 : int) (g1 : int) (g2 : int) (grade : int) : int =
      (if g0 >= grade then 1 else 0) +
      (if g1 >= grade then 1 else 0) +
      (if g2 >= grade then 1 else 0)
    in
    let count_gt5 = count_gt g0 g1 g2 5 in
    let count_gt4 = count_gt g0 g1 g2 4 in
    let count_gt3 = count_gt g0 g1 g2 3 in
    if count_gt5 = 3
      then "Masterpiece"
    else if count_gt5 = 2 && count_gt4 = 3
      then "Highly Recommended"
    else if count_gt4 >= 2 && count_gt3 >= 3
      then "Recommended"
    else
      "Mixed Reviews"
;;
