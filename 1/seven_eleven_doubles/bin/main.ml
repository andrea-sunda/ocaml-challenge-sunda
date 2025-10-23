let seven_eleven_doubles () =
  let d1 = Random.int(6)+1 in
  let d2 = Random.int(6)+1 in
  let sum = d1 + d2 in
  let bool_value = (d1 = d2 || sum = 7 || sum = 11) in
  (bool_value,d1,d2)
;;
