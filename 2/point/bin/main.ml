(* point record *)
type point = {
  x : float;
  y : float;
};;

(* translate function of a point *)
let translate p0 x1 y1 =
  {x = p0.x +. x1; y = p0.y +. y1}
;;

(* function to return the distance from the origin *)
let distance_from_origin p =
  sqrt ((p.x ** 2.) +. (p.y ** 2.))
;;

(* type color to implement a colored point *)
type color = RED | GREEN | BLUE;;

(* colored point type, I prefered to reuse the point type *)
type colored_point = {
  coordinates : point;
  color : color
};;

(* function to verify that two colored points has the same color *)
let same_color cp0 cp1 =
  cp0.color = cp1.color
;;

(* function to create a new colored point with different color but same coordinates *)
let recolor cp new_color =
  {coordinates = cp.coordinates; color = new_color}
;;

(* some asserts *)
assert(translate {x = 2.; y = 3.} 2. 3. = {x = 4.; y = 6.});;
assert(distance_from_origin {x = 3.; y = 4.} = 5.);;
assert(same_color {coordinates = {x = 2.; y = 3.}; color = BLUE} {coordinates = {x = 2.; y = 3.}; color = RED} = false);;
assert(same_color {coordinates = {x = 2.; y = 3.}; color = BLUE} {coordinates = {x = 2.; y = 3.}; color = BLUE} = true);;
assert(recolor {coordinates = {x = 2.; y = 3.}; color = BLUE} RED = {coordinates = {x = 2.; y = 3.}; color = RED});;
