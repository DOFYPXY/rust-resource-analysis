type list_t =
  | ListCons of int * list_t 
  | ListNil 

type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 



let tick_fwd : unit -> unit result = 
    fun _ ->
      Raml.tick 1.0; 
      Return ()

let rec list_nth_mut_loop_loop_fwd :
  list_t -> int -> int result =
  fun ls ->
    fun i ->
      match ls with
      | ListCons (x, tl) ->
          if i = 0
          then Return x
          else
            (match tick_fwd () with
             | Return x1 ->
                 let x2 = i -1 in
                  list_nth_mut_loop_loop_fwd tl x2
             | Fail e -> Fail e)
      | ListNil -> Fail Failure
let list_nth_mut_loop_fwd :
  list_t -> int -> int result =
  fun ls -> fun i -> list_nth_mut_loop_loop_fwd ls i
let rec list_nth_mut_loop_loop_back :
  list_t -> int -> int -> list_t result =
  fun ls ->
    fun i ->
      fun ret ->
        match ls with
        | ListCons (x, tl) ->
            if i = 0
            then Return (ListCons (ret, tl))
            else
              (match tick_fwd () with
               | Return x1 ->
                    let x2 = i - 1 in
                        (match list_nth_mut_loop_loop_back tl x2 ret with
                         | Return x3 ->
                             Return (ListCons (x, x3))
                         | Fail e -> Fail e)
               | Fail e -> Fail e)
        | ListNil -> Fail Failure
let list_nth_mut_loop_back :
  list_t -> int -> int -> list_t result =
  fun ls -> fun i -> fun ret -> list_nth_mut_loop_loop_back ls i ret
