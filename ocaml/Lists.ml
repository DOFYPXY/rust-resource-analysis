open Prims
type state = unit
type list_t =
  | ListCons of Primitives.u32 * list_t 
  | ListNil 
let (uu___is_ListCons : list_t -> Prims.bool) =
  fun projectee ->
    match projectee with | ListCons (_0, _1) -> true | uu___ -> false
let (__proj__ListCons__item___0 : list_t -> Primitives.u32) =
  fun projectee -> match projectee with | ListCons (_0, _1) -> _0
let (__proj__ListCons__item___1 : list_t -> list_t) =
  fun projectee -> match projectee with | ListCons (_0, _1) -> _1
let (uu___is_ListNil : list_t -> Prims.bool) =
  fun projectee -> match projectee with | ListNil -> true | uu___ -> false
let (tick_fwd : unit Primitives.result) = Primitives.Return ()
let rec (list_nth_mut_fwd :
  list_t -> Primitives.u32 -> Primitives.u32 Primitives.result) =
  fun l ->
    fun i ->
      match l with
      | ListCons (x, tl) ->
          if i = Prims.int_zero
          then Primitives.Return x
          else
            (match Primitives.u32_sub i Prims.int_one with
             | Primitives.Return x1 -> list_nth_mut_fwd tl x1
             | Primitives.Fail e -> Primitives.Fail e)
      | ListNil -> Primitives.Fail Primitives.Failure
let rec (list_nth_mut_back :
  list_t -> Primitives.u32 -> Primitives.u32 -> list_t Primitives.result) =
  fun l ->
    fun i ->
      fun ret ->
        match l with
        | ListCons (x, tl) ->
            if i = Prims.int_zero
            then Primitives.Return (ListCons (ret, tl))
            else
              (match Primitives.u32_sub i Prims.int_one with
               | Primitives.Return x1 ->
                   (match list_nth_mut_back tl x1 ret with
                    | Primitives.Return x2 ->
                        Primitives.Return (ListCons (x, x2))
                    | Primitives.Fail e -> Primitives.Fail e)
               | Primitives.Fail e -> Primitives.Fail e)
        | ListNil -> Primitives.Fail Primitives.Failure
let rec (list_nth_mut_loop_loop_fwd :
  list_t -> Primitives.u32 -> Primitives.u32 Primitives.result) =
  fun ls ->
    fun i ->
      match ls with
      | ListCons (x, tl) ->
          if i = Prims.int_zero
          then Primitives.Return x
          else
            (match tick_fwd with
             | Primitives.Return x1 ->
                 (match Primitives.u32_sub i Prims.int_one with
                  | Primitives.Return x2 -> list_nth_mut_loop_loop_fwd tl x2
                  | Primitives.Fail e -> Primitives.Fail e)
             | Primitives.Fail e -> Primitives.Fail e)
      | ListNil -> Primitives.Fail Primitives.Failure
let (list_nth_mut_loop_fwd :
  list_t -> Primitives.u32 -> Primitives.u32 Primitives.result) =
  fun ls -> fun i -> list_nth_mut_loop_loop_fwd ls i
let rec (list_nth_mut_loop_loop_back :
  list_t -> Primitives.u32 -> Primitives.u32 -> list_t Primitives.result) =
  fun ls ->
    fun i ->
      fun ret ->
        match ls with
        | ListCons (x, tl) ->
            if i = Prims.int_zero
            then Primitives.Return (ListCons (ret, tl))
            else
              (match tick_fwd with
               | Primitives.Return x1 ->
                   (match Primitives.u32_sub i Prims.int_one with
                    | Primitives.Return x2 ->
                        (match list_nth_mut_loop_loop_back tl x2 ret with
                         | Primitives.Return x3 ->
                             Primitives.Return (ListCons (x, x3))
                         | Primitives.Fail e -> Primitives.Fail e)
                    | Primitives.Fail e -> Primitives.Fail e)
               | Primitives.Fail e -> Primitives.Fail e)
        | ListNil -> Primitives.Fail Primitives.Failure
let (list_nth_mut_loop_back :
  list_t -> Primitives.u32 -> Primitives.u32 -> list_t Primitives.result) =
  fun ls -> fun i -> fun ret -> list_nth_mut_loop_loop_back ls i ret
let rec (list_plus_1_fwd : list_t -> Primitives.u32 Primitives.result) =
  fun l ->
    match l with
    | ListCons (x, tl) ->
        (match Primitives.u32_add x Prims.int_one with
         | Primitives.Return x1 -> list_plus_1_fwd tl
         | Primitives.Fail e -> Primitives.Fail e)
    | ListNil -> Primitives.Return (Prims.of_int (233))
let rec (list_plus_1_back : list_t -> list_t Primitives.result) =
  fun l ->
    match l with
    | ListCons (x, tl) ->
        (match Primitives.u32_add x Prims.int_one with
         | Primitives.Return x1 ->
             (match list_plus_1_back tl with
              | Primitives.Return x2 -> Primitives.Return (ListCons (x1, x2))
              | Primitives.Fail e -> Primitives.Fail e)
         | Primitives.Fail e -> Primitives.Fail e)
    | ListNil -> Primitives.Return ListNil
let rec (list_tricky_back : list_t -> list_t Primitives.result) =
  fun l ->
    match l with
    | ListCons (x, tl) ->
        (match Primitives.u32_add x Prims.int_one with
         | Primitives.Return x1 ->
             (match list_tricky_back tl with
              | Primitives.Return x2 ->
                  (match x2 with
                   | ListCons (i, l0) ->
                       Primitives.Return (ListCons (x1, (ListCons (i, l0))))
                   | ListNil -> Primitives.Return (ListCons (x1, ListNil)))
              | Primitives.Fail e -> Primitives.Fail e)
         | Primitives.Fail e -> Primitives.Fail e)
    | ListNil -> Primitives.Return ListNil
let rec (list_tricky_fwd : list_t -> Primitives.u32 Primitives.result) =
  fun l ->
    match l with
    | ListCons (x, tl) ->
        (match Primitives.u32_add x Prims.int_one with
         | Primitives.Return x1 ->
             (match list_tricky_fwd tl with
              | Primitives.Return x2 ->
                  (match list_tricky_back tl with
                   | Primitives.Return x3 ->
                       (match x3 with
                        | ListCons (i, l0) -> Primitives.Return x2
                        | ListNil -> Primitives.Return x2)
                   | Primitives.Fail e -> Primitives.Fail e)
              | Primitives.Fail e -> Primitives.Fail e)
         | Primitives.Fail e -> Primitives.Fail e)
    | ListNil -> Primitives.Return (Prims.of_int (233))