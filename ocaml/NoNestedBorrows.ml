type error =
  | Failure 
  | OutOfFuel

type 'a result =
  | Return of 'a 
  | Fail of error 

type state = unit
let (tick_fwd : unit result) = Return ()
type ('t1, 't2) pair_t = {
  pair_x: 't1 ;
  pair_y: 't2 }
let __proj__Mkpair_t__item__pair_x : 't1 't2 . ('t1, 't2) pair_t -> 't1 =
  fun projectee -> match projectee with | { pair_x; pair_y;_} -> pair_x
let __proj__Mkpair_t__item__pair_y : 't1 't2 . ('t1, 't2) pair_t -> 't2 =
  fun projectee -> match projectee with | { pair_x; pair_y;_} -> pair_y
type 't list_t =
  | ListCons of 't * 't list_t 
  | ListNil 
let uu___is_ListCons : 't . 't list_t -> bool =
  fun projectee ->
    match projectee with | ListCons (_0, _1) -> true | uu___ -> false
let __proj__ListCons__item___0 : 't . 't list_t -> 't =
  fun projectee -> match projectee with | ListCons (_0, _1) -> _0
let __proj__ListCons__item___1 : 't . 't list_t -> 't list_t =
  fun projectee -> match projectee with | ListCons (_0, _1) -> _1
let uu___is_ListNil : 't . 't list_t -> bool =
  fun projectee -> match projectee with | ListNil -> true | uu___ -> false
type 't1 one_t =
  | OneOne of 't1 
let uu___is_OneOne : 't1 . 't1 one_t -> bool = fun projectee -> true
let __proj__OneOne__item___0 : 't1 . 't1 one_t -> 't1 =
  fun projectee -> match projectee with | OneOne _0 -> _0
type empty_enum_t =
  | EmptyEnumEmpty 
let (uu___is_EmptyEnumEmpty : empty_enum_t -> bool) =
  fun projectee -> true
type enum_t =
  | EnumVariant1 
  | EnumVariant2 
let (uu___is_EnumVariant1 : enum_t -> bool) =
  fun projectee ->
    match projectee with | EnumVariant1 -> true | uu___ -> false
let (uu___is_EnumVariant2 : enum_t -> bool) =
  fun projectee ->
    match projectee with | EnumVariant2 -> true | uu___ -> false
type empty_struct_t = unit
type ('t1, 't2) sum_t =
  | SumLeft of 't1 
  | SumRight of 't2 
let uu___is_SumLeft : 't1 't2 . ('t1, 't2) sum_t -> bool =
  fun projectee -> match projectee with | SumLeft _0 -> true | uu___ -> false
let __proj__SumLeft__item___0 : 't1 't2 . ('t1, 't2) sum_t -> 't1 =
  fun projectee -> match projectee with | SumLeft _0 -> _0
let uu___is_SumRight : 't1 't2 . ('t1, 't2) sum_t -> bool =
  fun projectee ->
    match projectee with | SumRight _0 -> true | uu___ -> false
let __proj__SumRight__item___0 : 't1 't2 . ('t1, 't2) sum_t -> 't2 =
  fun projectee -> match projectee with | SumRight _0 -> _0
let (neg_test_fwd : i32 -> i32 result) =
  fun x -> i32_neg x
let (add_test_fwd :
  u32 -> u32 -> u32 result) =
  fun x -> fun y -> u32_add x y
let (subs_test_fwd :
  u32 -> u32 -> u32 result) =
  fun x -> fun y -> u32_sub x y
let (div_test_fwd :
  u32 -> u32 -> u32 result) =
  fun x -> fun y -> u32_div x y
let (div_test1_fwd : u32 -> u32 result) =
  fun x -> u32_div x (of_int (2))
let (rem_test_fwd :
  u32 -> u32 -> u32 result) =
  fun x -> fun y -> u32_rem x y
let (cast_test_fwd : u32 -> i32 result) =
  fun x -> scalar_cast U32 I32 x
let (test2_fwd : unit result) =
  match u32_add (of_int (23)) (of_int (44)) with
  | Return x -> Return ()
  | Fail e -> Fail e
let (get_max_fwd :
  u32 -> u32 -> u32 result) =
  fun x ->
    fun y -> if x >= y then Return x else Return y
let (test3_fwd : unit result) =
  match get_max_fwd (of_int (4)) (of_int (3)) with
  | Return x ->
      (match get_max_fwd (of_int (10)) (of_int (11)) with
       | Return x1 ->
           (match u32_add x x1 with
            | Return x2 ->
                if op_Negation (x2 = (of_int (15)))
                then Fail Failure
                else Return ()
            | Fail e -> Fail e)
       | Fail e -> Fail e)
  | Fail e -> Fail e
let (test_neg1_fwd : unit result) =
  match i32_neg (of_int (3)) with
  | Return x ->
      if op_Negation (x = (of_int (-3)))
      then Fail Failure
      else Return ()
  | Fail e -> Fail e
let (refs_test1_fwd : unit result) = Return ()
let (refs_test2_fwd : unit result) = Return ()
let (test_list1_fwd : unit result) = Return ()
let (test_box1_fwd : unit result) =
  let b = int_one in
  let x = b in
  if op_Negation (x = int_one)
  then Fail Failure
  else Return ()
let (copy_int_fwd : i32 -> i32 result) =
  fun x -> Return x
let (test_copy_int_fwd : unit result) =
  match copy_int_fwd int_zero with
  | Return x ->
      if op_Negation (int_zero = x)
      then Fail Failure
      else Return ()
  | Fail e -> Fail e
let is_cons_fwd : 't . 't list_t -> bool result =
  fun l ->
    match l with
    | ListCons (x, l0) -> Return true
    | ListNil -> Return false
let (test_is_cons_fwd : unit result) =
  let l = ListNil in
  match is_cons_fwd (ListCons (int_zero, l)) with
  | Return x ->
      if op_Negation x
      then Fail Failure
      else Return ()
  | Fail e -> Fail e
let split_list_fwd : 't . 't list_t -> ('t * 't list_t) result =
  fun l ->
    match l with
    | ListCons (hd, tl) -> Return (hd, tl)
    | ListNil -> Fail Failure
let (test_split_list_fwd : unit result) =
  let l = ListNil in
  match split_list_fwd (ListCons (int_zero, l)) with
  | Return x ->
      let uu___ = x in
      (match uu___ with
       | (hd, uu___1) ->
           if op_Negation (hd = int_zero)
           then Fail Failure
           else Return ())
  | Fail e -> Fail e
let choose_fwd : 't . bool -> 't -> 't -> 't result =
  fun b ->
    fun x -> fun y -> if b then Return x else Return y
let choose_back :
  't . bool -> 't -> 't -> 't -> ('t * 't) result =
  fun b ->
    fun x ->
      fun y ->
        fun ret ->
          if b
          then Return (ret, y)
          else Return (x, ret)
let (choose_test_fwd : unit result) =
  match choose_fwd true int_zero int_zero with
  | Return x ->
      (match i32_add x int_one with
       | Return x1 ->
           if op_Negation (x1 = int_one)
           then Fail Failure
           else
             (match choose_back true int_zero int_zero x1 with
              | Return x2 ->
                  (match x2 with
                   | (x3, y) ->
                       if op_Negation (x3 = int_one)
                       then Fail Failure
                       else
                         if op_Negation (y = int_zero)
                         then Fail Failure
                         else Return ())
              | Fail e -> Fail e)
       | Fail e -> Fail e)
  | Fail e -> Fail e
let (test_char_fwd : char result) =
  Return 97
type 't node_elem_t =
  | NodeElemCons of 't tree_t * 't node_elem_t 
  | NodeElemNil 
and 't tree_t =
  | TreeLeaf of 't 
  | TreeNode of 't * 't node_elem_t * 't tree_t 
let uu___is_NodeElemCons : 't . 't node_elem_t -> bool =
  fun projectee ->
    match projectee with | NodeElemCons (_0, _1) -> true | uu___ -> false
let __proj__NodeElemCons__item___0 : 't . 't node_elem_t -> 't tree_t =
  fun projectee -> match projectee with | NodeElemCons (_0, _1) -> _0
let __proj__NodeElemCons__item___1 : 't . 't node_elem_t -> 't node_elem_t =
  fun projectee -> match projectee with | NodeElemCons (_0, _1) -> _1
let uu___is_NodeElemNil : 't . 't node_elem_t -> bool =
  fun projectee ->
    match projectee with | NodeElemNil -> true | uu___ -> false
let uu___is_TreeLeaf : 't . 't tree_t -> bool =
  fun projectee ->
    match projectee with | TreeLeaf _0 -> true | uu___ -> false
let __proj__TreeLeaf__item___0 : 't . 't tree_t -> 't =
  fun projectee -> match projectee with | TreeLeaf _0 -> _0
let uu___is_TreeNode : 't . 't tree_t -> bool =
  fun projectee ->
    match projectee with | TreeNode (_0, _1, _2) -> true | uu___ -> false
let __proj__TreeNode__item___0 : 't . 't tree_t -> 't =
  fun projectee -> match projectee with | TreeNode (_0, _1, _2) -> _0
let __proj__TreeNode__item___1 : 't . 't tree_t -> 't node_elem_t =
  fun projectee -> match projectee with | TreeNode (_0, _1, _2) -> _1
let __proj__TreeNode__item___2 : 't . 't tree_t -> 't tree_t =
  fun projectee -> match projectee with | TreeNode (_0, _1, _2) -> _2
let rec list_length_fwd : 't . 't list_t -> u32 result
  =
  fun l ->
    match l with
    | ListCons (x, l1) ->
        (match list_length_fwd l1 with
         | Return x1 -> u32_add int_one x1
         | Fail e -> Fail e)
    | ListNil -> Return int_zero
let rec list_nth_shared_fwd :
  't . 't list_t -> u32 -> 't result =
  fun l ->
    fun i ->
      match l with
      | ListCons (x, tl) ->
          if i = int_zero
          then Return x
          else
            (match u32_sub i int_one with
             | Return x1 -> list_nth_shared_fwd tl x1
             | Fail e -> Fail e)
      | ListNil -> Fail Failure
let rec list_nth_mut_fwd :
  't . 't list_t -> u32 -> 't result =
  fun l ->
    fun i ->
      match l with
      | ListCons (x, tl) ->
          if i = int_zero
          then Return x
          else
            (match u32_sub i int_one with
             | Return x1 -> list_nth_mut_fwd tl x1
             | Fail e -> Fail e)
      | ListNil -> Fail Failure
let rec list_nth_mut_back :
  't . 't list_t -> u32 -> 't -> 't list_t result =
  fun l ->
    fun i ->
      fun ret ->
        match l with
        | ListCons (x, tl) ->
            if i = int_zero
            then Return (ListCons (ret, tl))
            else
              (match u32_sub i int_one with
               | Return x1 ->
                   (match list_nth_mut_back tl x1 ret with
                    | Return x2 ->
                        Return (ListCons (x, x2))
                    | Fail e -> Fail e)
               | Fail e -> Fail e)
        | ListNil -> Fail Failure
let rec list_rev_aux_fwd :
  't . 't list_t -> 't list_t -> 't list_t result =
  fun li ->
    fun lo ->
      match tick_fwd with
      | Return x ->
          (match li with
           | ListCons (hd, tl) -> list_rev_aux_fwd tl (ListCons (hd, lo))
           | ListNil -> Return lo)
      | Fail e -> Fail e
let list_rev_fwd_back : 't . 't list_t -> 't list_t result =
  fun l ->
    let li = mem_replace_fwd l ListNil in
    list_rev_aux_fwd li ListNil
let (test_list_functions_fwd : unit result) =
  let l = ListNil in
  let l0 = ListCons ((of_int (2)), l) in
  let l1 = ListCons (int_one, l0) in
  match list_length_fwd (ListCons (int_zero, l1)) with
  | Return x ->
      if op_Negation (x = (of_int (3)))
      then Fail Failure
      else
        (match list_nth_shared_fwd (ListCons (int_zero, l1))
                 int_zero
         with
         | Return x1 ->
             if op_Negation (x1 = int_zero)
             then Fail Failure
             else
               (match list_nth_shared_fwd (ListCons (int_zero, l1))
                        int_one
                with
                | Return x2 ->
                    if op_Negation (x2 = int_one)
                    then Fail Failure
                    else
                      (match list_nth_shared_fwd
                               (ListCons (int_zero, l1))
                               (of_int (2))
                       with
                       | Return x3 ->
                           if op_Negation (x3 = (of_int (2)))
                           then Fail Failure
                           else
                             (match list_nth_mut_back
                                      (ListCons (int_zero, l1))
                                      int_one (of_int (3))
                              with
                              | Return x4 ->
                                  (match list_nth_shared_fwd x4
                                           int_zero
                                   with
                                   | Return x5 ->
                                       if
                                         op_Negation
                                           (x5 = int_zero)
                                       then
                                         Fail Failure
                                       else
                                         (match list_nth_shared_fwd x4
                                                  int_one
                                          with
                                          | Return x6 ->
                                              if
                                                op_Negation
                                                  (x6 = (of_int (3)))
                                              then
                                                Fail
                                                  Failure
                                              else
                                                (match list_nth_shared_fwd x4
                                                         (of_int (2))
                                                 with
                                                 | Return x7 ->
                                                     if
                                                       op_Negation
                                                         (x7 =
                                                            (of_int (2)))
                                                     then
                                                       Fail
                                                         Failure
                                                     else
                                                       Return ()
                                                 | Fail e ->
                                                     Fail e)
                                          | Fail e ->
                                              Fail e)
                                   | Fail e -> Fail e)
                              | Fail e -> Fail e)
                       | Fail e -> Fail e)
                | Fail e -> Fail e)
         | Fail e -> Fail e)
  | Fail e -> Fail e
let id_mut_pair1_fwd : 't1 't2 . 't1 -> 't2 -> ('t1 * 't2) result
  = fun x -> fun y -> Return (x, y)
let id_mut_pair1_back :
  't1 't2 . 't1 -> 't2 -> ('t1 * 't2) -> ('t1 * 't2) result =
  fun x ->
    fun y ->
      fun ret ->
        let uu___ = ret in
        match uu___ with | (x0, x1) -> Return (x0, x1)
let id_mut_pair2_fwd : 't1 't2 . ('t1 * 't2) -> ('t1 * 't2) result
  =
  fun p ->
    let uu___ = p in match uu___ with | (x, x0) -> Return (x, x0)
let id_mut_pair2_back :
  't1 't2 . ('t1 * 't2) -> ('t1 * 't2) -> ('t1 * 't2) result =
  fun p ->
    fun ret ->
      let uu___ = ret in
      match uu___ with | (x, x0) -> Return (x, x0)
let id_mut_pair3_fwd : 't1 't2 . 't1 -> 't2 -> ('t1 * 't2) result
  = fun x -> fun y -> Return (x, y)
let id_mut_pair3_back'a :
  't1 't2 . 't1 -> 't2 -> 't1 -> 't1 result =
  fun x -> fun y -> fun ret -> Return ret
let id_mut_pair3_back'b :
  't1 't2 . 't1 -> 't2 -> 't2 -> 't2 result =
  fun x -> fun y -> fun ret -> Return ret
let id_mut_pair4_fwd : 't1 't2 . ('t1 * 't2) -> ('t1 * 't2) result
  =
  fun p ->
    let uu___ = p in match uu___ with | (x, x0) -> Return (x, x0)
let id_mut_pair4_back'a :
  't1 't2 . ('t1 * 't2) -> 't1 -> 't1 result =
  fun p -> fun ret -> Return ret
let id_mut_pair4_back'b :
  't1 't2 . ('t1 * 't2) -> 't2 -> 't2 result =
  fun p -> fun ret -> Return ret
type ('t1, 't2) struct_with_tuple_t = {
  struct_with_tuple_p: ('t1 * 't2) }
let __proj__Mkstruct_with_tuple_t__item__struct_with_tuple_p :
  't1 't2 . ('t1, 't2) struct_with_tuple_t -> ('t1 * 't2) =
  fun projectee ->
    match projectee with | { struct_with_tuple_p;_} -> struct_with_tuple_p
let (new_tuple1_fwd :
  (u32, u32) struct_with_tuple_t result) =
  Return
    { struct_with_tuple_p = (int_one, (of_int (2))) }
let (new_tuple2_fwd :
  (i16, i16) struct_with_tuple_t result) =
  Return
    { struct_with_tuple_p = (int_one, (of_int (2))) }
let (new_tuple3_fwd :
  (u64, i64) struct_with_tuple_t result) =
  Return
    { struct_with_tuple_p = (int_one, (of_int (2))) }
type ('t1, 't2) struct_with_pair_t = {
  struct_with_pair_p: ('t1, 't2) pair_t }
let __proj__Mkstruct_with_pair_t__item__struct_with_pair_p :
  't1 't2 . ('t1, 't2) struct_with_pair_t -> ('t1, 't2) pair_t =
  fun projectee ->
    match projectee with | { struct_with_pair_p;_} -> struct_with_pair_p
let (new_pair1_fwd :
  (u32, u32) struct_with_pair_t result) =
  Return
    {
      struct_with_pair_p =
        { pair_x = int_one; pair_y = (of_int (2)) }
    }
let (test_constants_fwd : unit result) =
  match new_tuple1_fwd with
  | Return x ->
      let uu___ = x.struct_with_tuple_p in
      (match uu___ with
       | (i, uu___1) ->
           if op_Negation (i = int_one)
           then Fail Failure
           else
             (match new_tuple2_fwd with
              | Return x1 ->
                  let uu___3 = x1.struct_with_tuple_p in
                  (match uu___3 with
                   | (i0, uu___4) ->
                       if op_Negation (i0 = int_one)
                       then Fail Failure
                       else
                         (match new_tuple3_fwd with
                          | Return x2 ->
                              let uu___6 = x2.struct_with_tuple_p in
                              (match uu___6 with
                               | (i1, uu___7) ->
                                   if op_Negation (i1 = int_one)
                                   then Fail Failure
                                   else
                                     (match new_pair1_fwd with
                                      | Return x3 ->
                                          if
                                            op_Negation
                                              ((x3.struct_with_pair_p).pair_x
                                                 = int_one)
                                          then
                                            Fail
                                              Failure
                                          else Return ()
                                      | Fail e ->
                                          Fail e))
                          | Fail e -> Fail e))
              | Fail e -> Fail e))
  | Fail e -> Fail e
let (test_weird_borrows1_fwd : unit result) = Return ()
let (test_mem_replace_fwd_back :
  u32 -> u32 result) =
  fun px ->
    let y = mem_replace_fwd px int_one in
    if op_Negation (y = int_zero)
    then Fail Failure
    else Return (of_int (2))
let (test_shared_borrow_bool1_fwd :
  bool -> u32 result) =
  fun b ->
    if b
    then Return int_zero
    else Return int_one
let (test_shared_borrow_bool2_fwd : u32 result) =
  Return int_zero
let (test_shared_borrow_enum1_fwd :
  u32 list_t -> u32 result) =
  fun l ->
    match l with
    | ListCons (i, l0) -> Return int_one
    | ListNil -> Return int_zero
let (test_shared_borrow_enum2_fwd : u32 result) =
  Return int_zero