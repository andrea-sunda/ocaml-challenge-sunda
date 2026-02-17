type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
};;

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

let rec id_of_noshow = function
  | [] -> []
  | {id; vote; _} :: t -> 
    if vote = None then id :: id_of_noshow t 
    else id_of_noshow t
;;

let rec upgradable = function
  | [] -> []
  | {name; surname; vote; _} :: t -> 
    if vote >= (Some 15) && vote <= (Some 17) then (name ^ "/" ^ surname ) :: upgradable t 
    else upgradable t
;;

let rec upgrade = function
  | [] -> []
  | ({vote; _} as s) :: t ->
    if vote >= (Some 15) && vote <= (Some 17) then {s with vote=(Some 18)} :: upgrade t
    else s :: upgrade t
;;

let rec wrong_laude = function
  | [] -> []
  | {name; surname; vote; laude; _} :: t -> 
    if laude && (vote = None || vote < (Some 30)) then (name ^ "/" ^ surname ) :: wrong_laude t 
    else wrong_laude t
;;

let rec fix_laude = function
  | [] -> []
  | ({vote; laude; _} as s) :: t ->
    if laude && (vote = None || vote < (Some 30)) then {s with laude = false}:: fix_laude t
    else s :: fix_laude t
;;

let percent_passed lst =
  let rec passed = function
    | [] -> 0
    | {vote; _} :: t ->
      if vote >= (Some 18) then 1 + passed t
      else passed t
  in
  (passed lst) * 100 / (List.length lst)
;;

let rec avg_vote lst =
  let rec sum_and_count = function
    | [] -> (0, 0)
    | {vote; laude; _} :: t ->
        let (s, c) = sum_and_count t in
        if vote >= (Some 18) then
          let v = match vote with Some x -> x | None -> 0 in
          let score = if laude then v + 2 else v in
          (score + s, 1 + c)
        else (s, c)
  in
  let (total_sum, count) = sum_and_count lst in
  if count = 0 then 0.0
  else float_of_int total_sum /. float_of_int count
;;
