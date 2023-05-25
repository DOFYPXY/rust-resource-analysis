type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 

type 't list_t =
  | ListCons of 't * 't list_t 
  | ListNil 

let tick_fwd : unit -> unit result = 
  fun _ ->
    Raml.tick 1.0; 
    Return ()

let mem_replace_fwd : 'a . 'a -> 'a -> 'a = fun x -> fun y -> x
let mem_replace_back : 'a . 'a -> 'a -> 'a = fun x -> fun y -> y



let rec list_rev_aux_fwd :
  't . 't list_t -> 't list_t -> 't list_t result =
  fun li ->
    fun lo ->
      match tick_fwd () with
      | Return x ->
          (match li with
           | ListCons (hd, tl) -> list_rev_aux_fwd tl (ListCons (hd, lo))
           | ListNil -> Return lo)
      | Fail e -> Fail e
let list_rev_fwd_back : 't . 't list_t -> 't list_t result =
  fun l ->
    let li = mem_replace_fwd l ListNil in
    list_rev_aux_fwd li ListNil