let dice perc =
  if perc < 0 || perc > 100 then failwith "Error, percentage is not valid."
  else
    if Random.int 100 < perc then 6
    else (Random.int 5)+1
;;
