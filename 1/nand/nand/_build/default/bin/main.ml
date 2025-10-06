let nand a b = (not(a && b));;

print_bool nand true true;;
nand true false;;
nand false true;;
nand false false;;

let nand a b = if a && b then false else true;;

nand true true;;
nand true false;;
nand false true;;
nand false false;;

let nand a b = match(a,b) with(true,true) -> false | _ -> true;;
nand true true;;
nand true false;;
nand false true;;
nand false false;;
