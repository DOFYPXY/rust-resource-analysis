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



let selection_sort_comp_fwd : 't -> 't -> bool result =
      fun x -> fun y -> Return false

let rec selection_sort_find_min_fwd :
    't utils_list_t ->
      't utils_list_t result
  =
  fun l ->
    match l with
    | UtilsListCons (x, xs) ->
        (match selection_sort_find_min_fwd xs with
         | Return x1 ->
             (match x1 with
              | UtilsListCons (y, ys) ->
                  (match utils_tick_fwd () with
                   | Return x2 ->
                       (match selection_sort_comp_fwd x y with
                        | Return x3 ->
                            if x3
                            then
                              let l1 = UtilsListCons (y, ys) in
                              Return
                                (UtilsListCons (x, l1))
                            else
                              (let l1 = UtilsListCons (x, ys) in
                               Return
                                 (UtilsListCons (y, l1)))
                        | Fail e -> Fail e)
                   | Fail e -> Fail e)
              | UtilsListNil ->
                  let l1 = UtilsListNil in
                  Return (UtilsListCons (x, l1)))
         | Fail e -> Fail e)
    | UtilsListNil -> Return UtilsListNil
let rec selection_sort_sort_fwd :
    't utils_list_t ->
      't utils_list_t result
  =
  fun l ->
    match selection_sort_find_min_fwd l with
    | Return x ->
        (match x with
         | UtilsListCons (hd, tl) ->
             (match utils_tick_fwd () with
              | Return x1 ->
                  (match selection_sort_sort_fwd tl with
                   | Return x2 ->
                       let l2 = x2 in
                       Return (UtilsListCons (hd, l2))
                   | Fail e -> Fail e)
              | Fail e -> Fail e)
         | UtilsListNil ->
             Return UtilsListNil)
    | Fail e -> Fail e