let max_nat (a : int) (b : int) = if a < 0 || b < 0
then failwith("Use only natural numbers")
else if a < b
then b
else a;;
