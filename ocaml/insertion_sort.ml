type 't utils_list_t =
  | UtilsListCons of 't * 't utils_list_t 
  | UtilsListNil 

type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 

let utils_tick_fwd : unit -> unit result = 
  fun _ ->
    Raml.tick 1.0; 
    Return ()


let insertion_sort_comp_fwd : 't -> 't -> bool result =
  fun x -> fun y -> Return false
let rec insertion_sort_insert_fwd :
    't ->
      't utils_list_t ->
        't utils_list_t result
  =
  fun v ->
    fun l ->
      match utils_tick_fwd () with
      | Return x ->
          (match l with
           | UtilsListCons (hd, tl) ->
               (match insertion_sort_comp_fwd v hd with
                | Return x1 ->
                    if x1
                    then
                      let l0 = UtilsListCons (hd, tl) in
                      Return (UtilsListCons (v, l0))
                    else
                      (match insertion_sort_insert_fwd v tl with
                       | Return x2 ->
                           let l1 = x2 in
                           Return
                             (UtilsListCons (hd, l1))
                       | Fail e -> Fail e)
                | Fail e -> Fail e)
           | UtilsListNil ->
               let l0 = UtilsListNil in
               Return (UtilsListCons (v, l0)))
      | Fail e -> Fail e
let rec insertion_sort_sort_fwd :
    't utils_list_t ->
      't utils_list_t result
  =
  fun l ->
    match utils_tick_fwd () with
    | Return x ->
        (match l with
         | UtilsListCons (hd, tl) ->
             (match insertion_sort_sort_fwd tl with
              | Return x1 -> insertion_sort_insert_fwd hd x1
              | Fail e -> Fail e)
         | UtilsListNil ->
             Return UtilsListNil)
    | Fail e -> Fail e