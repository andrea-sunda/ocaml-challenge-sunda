type grade = Val of int | CumLaude;;

let is_valid (g : grade) =
  match g with
  | CumLaude -> true
  | Val a -> (a >= 18 && a <= 30)
;;

let int_of_grade g =
  match g with
  | CumLaude -> 32
  | Val a when (a >= 18 && a <= 30) -> a
  | _ -> failwith "The grade is not valid!"
;;

let avg glst =
  let rec calc lst sum cnt =
    match lst with
    | [] -> if cnt = 0 then 0 else sum / cnt
    | h :: t when is_valid h -> calc t (sum + (int_of_grade h)) (cnt+1)
    | _ -> failwith "One element of the list was not a valid grade!"
  in
  Val(calc glst 0 0)
;;

let avg_norec grades =
  let valid_grades = 
  List.filter is_valid grades
  |> List.map int_of_grade
  in
  match valid_grades with
  | [] -> 0
  | _ -> List.fold_left (+) 0 valid_grades / List.length valid_grades
;;
