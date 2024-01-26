type expr = Num of int | Add of expr * expr | Mul of expr * expr

let is_reducible = function Num _ -> false | Add _ | Mul _ -> true

let rec reduce = function
  | Num n -> Num n
  | Add (left, right) -> (
      if is_reducible left then Add (reduce left, right)
      else if is_reducible right then Add (left, reduce right)
      else
        match (left, right) with
        | Num l, Num r -> Num (l + r)
        | _ -> failwith "Invalid case")
  | Mul (left, right) -> (
      if is_reducible left then Mul (reduce left, right)
      else if is_reducible right then Mul (left, reduce right)
      else
        match (left, right) with
        | Num l, Num r -> Num (l * r)
        | _ -> failwith "Invalid case")

let rec to_string = function
  | Num v -> string_of_int v
  | Add (l, r) -> "(" ^ to_string l ^ " + " ^ to_string r ^ ")"
  | Mul (l, r) -> "(" ^ to_string l ^ " * " ^ to_string r ^ ")"

let run expr print =
  let expresstion = ref expr in
  while is_reducible !expresstion do
    print (to_string !expresstion);
    expresstion := reduce !expresstion
  done;
  print (to_string !expresstion)
