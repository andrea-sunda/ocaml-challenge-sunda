type vote = StrongReject | WeakReject | WeakAccept | StrongAccept;;

let decide_exam (a : vote) (b : vote) (c : vote) : bool =
  let is_accept (a : vote) : int =
   if  a = WeakAccept || a = StrongAccept then 1 else 0
  in
  let num_accepts =
    (is_accept a) + (is_accept b) + (is_accept c)
  in
  let there_is_strong_reject =
    (a = StrongReject || b = StrongReject || c = StrongReject)
  in
  num_accepts >= 2 && (not there_is_strong_reject)
;;
