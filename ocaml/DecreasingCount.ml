type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 

let int_add : int -> int -> int result=
  fun x -> fun y -> Return (x + y)

let int_sub : int -> int -> int result =
  fun x -> fun y -> Return (x - y)

let tick_fwd : unit -> unit result = 
  fun _ ->
    Raml.tick 1.0; 
    Return ()


let f_fwd : int -> int result =
  fun x -> Return 0 
let rec fpow_fwd : int -> int result =
  fun n ->
    if n > 0
    then
      match tick_fwd () with
      | Return x ->
          (match int_sub n 1 with
           | Return x1 ->
               (match fpow_fwd x1 with
                | Return x2 -> f_fwd x2
                | Fail e -> Fail e)
           | Fail e -> Fail e)
      | Fail e -> Fail e
    else Return 0