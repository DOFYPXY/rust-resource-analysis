open Prims
type state = unit
type 't binary_search_tree_t =
  | BinarySearchTreeLeaf 
  | BinarySearchTreeTree of 't * 't binary_search_tree_t * 't
  binary_search_tree_t 
let uu___is_BinarySearchTreeLeaf : 't . 't binary_search_tree_t -> Prims.bool
  =
  fun projectee ->
    match projectee with | BinarySearchTreeLeaf -> true | uu___ -> false
let uu___is_BinarySearchTreeTree : 't . 't binary_search_tree_t -> Prims.bool
  =
  fun projectee ->
    match projectee with
    | BinarySearchTreeTree (_0, _1, _2) -> true
    | uu___ -> false
let __proj__BinarySearchTreeTree__item___0 :
  't . 't binary_search_tree_t -> 't =
  fun projectee ->
    match projectee with | BinarySearchTreeTree (_0, _1, _2) -> _0
let __proj__BinarySearchTreeTree__item___1 :
  't . 't binary_search_tree_t -> 't binary_search_tree_t =
  fun projectee ->
    match projectee with | BinarySearchTreeTree (_0, _1, _2) -> _1
let __proj__BinarySearchTreeTree__item___2 :
  't . 't binary_search_tree_t -> 't binary_search_tree_t =
  fun projectee ->
    match projectee with | BinarySearchTreeTree (_0, _1, _2) -> _2
type my_ordering_t =
  | MyOrderingEqual 
  | MyOrderingGreater 
  | MyOrderingLess 
let (uu___is_MyOrderingEqual : my_ordering_t -> Prims.bool) =
  fun projectee ->
    match projectee with | MyOrderingEqual -> true | uu___ -> false
let (uu___is_MyOrderingGreater : my_ordering_t -> Prims.bool) =
  fun projectee ->
    match projectee with | MyOrderingGreater -> true | uu___ -> false
let (uu___is_MyOrderingLess : my_ordering_t -> Prims.bool) =
  fun projectee ->
    match projectee with | MyOrderingLess -> true | uu___ -> false
let (tick_fwd : unit Primitives.result) = Primitives.Return ()
let comp_fwd : 't . 't -> 't -> my_ordering_t Primitives.result =
  fun x -> fun y -> Primitives.Return MyOrderingLess
let rec search_fwd :
  't . 't binary_search_tree_t -> 't -> Prims.bool Primitives.result =
  fun l ->
    fun value ->
      match tick_fwd with
      | Primitives.Return x ->
          (match l with
           | BinarySearchTreeLeaf -> Primitives.Return false
           | BinarySearchTreeTree (key, left, right) ->
               (match comp_fwd key value with
                | Primitives.Return x1 ->
                    (match x1 with
                     | MyOrderingEqual -> Primitives.Return true
                     | MyOrderingGreater -> search_fwd left value
                     | MyOrderingLess -> search_fwd right value)
                | Primitives.Fail e -> Primitives.Fail e))
      | Primitives.Fail e -> Primitives.Fail e
let rec insert_fwd_back :
  't .
    't binary_search_tree_t ->
      't -> 't binary_search_tree_t Primitives.result
  =
  fun l ->
    fun value ->
      match tick_fwd with
      | Primitives.Return x ->
          (match l with
           | BinarySearchTreeLeaf -> Primitives.Return BinarySearchTreeLeaf
           | BinarySearchTreeTree (key, left, right) ->
               (match comp_fwd value key with
                | Primitives.Return x1 ->
                    (match x1 with
                     | MyOrderingEqual ->
                         (match right with
                          | BinarySearchTreeLeaf ->
                              let bst = BinarySearchTreeLeaf in
                              let bst0 = BinarySearchTreeLeaf in
                              Primitives.Return
                                (BinarySearchTreeTree
                                   (key, left,
                                     (BinarySearchTreeTree (value, bst, bst0))))
                          | BinarySearchTreeTree (x2, bst, bst0) ->
                              (match insert_fwd_back
                                       (BinarySearchTreeTree (x2, bst, bst0))
                                       value
                               with
                               | Primitives.Return x3 ->
                                   Primitives.Return
                                     (BinarySearchTreeTree (key, left, x3))
                               | Primitives.Fail e -> Primitives.Fail e))
                     | MyOrderingGreater ->
                         (match right with
                          | BinarySearchTreeLeaf ->
                              let bst = BinarySearchTreeLeaf in
                              let bst0 = BinarySearchTreeLeaf in
                              Primitives.Return
                                (BinarySearchTreeTree
                                   (key, left,
                                     (BinarySearchTreeTree (value, bst, bst0))))
                          | BinarySearchTreeTree (x2, bst, bst0) ->
                              (match insert_fwd_back
                                       (BinarySearchTreeTree (x2, bst, bst0))
                                       value
                               with
                               | Primitives.Return x3 ->
                                   Primitives.Return
                                     (BinarySearchTreeTree (key, left, x3))
                               | Primitives.Fail e -> Primitives.Fail e))
                     | MyOrderingLess ->
                         (match left with
                          | BinarySearchTreeLeaf ->
                              let bst = BinarySearchTreeLeaf in
                              let bst0 = BinarySearchTreeLeaf in
                              Primitives.Return
                                (BinarySearchTreeTree
                                   (key,
                                     (BinarySearchTreeTree (value, bst, bst0)),
                                     right))
                          | BinarySearchTreeTree (x2, bst, bst0) ->
                              (match insert_fwd_back
                                       (BinarySearchTreeTree (x2, bst, bst0))
                                       value
                               with
                               | Primitives.Return x3 ->
                                   Primitives.Return
                                     (BinarySearchTreeTree (key, x3, right))
                               | Primitives.Fail e -> Primitives.Fail e)))
                | Primitives.Fail e -> Primitives.Fail e))
      | Primitives.Fail e -> Primitives.Fail e