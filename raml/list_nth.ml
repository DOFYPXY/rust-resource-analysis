type 't a_list_t =
  | AListCons of 't * 't a_list_t 
  | AListNil 

type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 

let a_tick_fwd : unit -> unit result = 
  fun _ ->
    Raml.tick 1.0; 
    Return ()

let rec a_list_nth_fwd :
't a_list_t -> int -> 't result =
fun l ->
  fun i ->
    match l with
    | AListCons (x, tl) ->
        if i = 0
        then Return x
        else
          (match a_tick_fwd () with
            | Return x1 ->
              let x2 = i - 1 in a_list_nth_fwd tl x2      
            | Fail e -> Fail e )     
    | AListNil -> Fail Failure
