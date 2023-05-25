type 't binary_search_tree_t =
  | BinarySearchTreeLeaf 
  | BinarySearchTreeTree of 't * 't binary_search_tree_t * 't
  binary_search_tree_t 

type my_ordering_t =
  | MyOrderingEqual 
  | MyOrderingGreater 
  | MyOrderingLess 

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

let comp_fwd : 't -> 't -> my_ordering_t result =
  fun x -> fun y -> Return MyOrderingLess
let rec search_fwd :
  't binary_search_tree_t -> 't -> bool result =
  fun l ->
    fun value ->
      match tick_fwd () with
      | Return x ->
          (match l with
           | BinarySearchTreeLeaf -> Return false
           | BinarySearchTreeTree (key, left, right) ->
               (match comp_fwd key value with
                | Return x1 ->
                    (match x1 with
                     | MyOrderingEqual -> Return true
                     | MyOrderingGreater -> search_fwd left value
                     | MyOrderingLess -> search_fwd right value)
                | Fail e -> Fail e))
      | Fail e -> Fail e


let rec insert_fwd_back :
  't binary_search_tree_t ->
    't -> 't binary_search_tree_t result
=
fun l ->
  fun value ->
    match tick_fwd () with
    | Return x ->
        (match l with
          | BinarySearchTreeLeaf -> Return BinarySearchTreeLeaf
          | BinarySearchTreeTree (key, left, right) ->
              (match comp_fwd value key with
              | Return x1 ->
                  (match x1 with
                    | MyOrderingEqual ->
                        (match right with
                        | BinarySearchTreeLeaf ->
                            let bst = BinarySearchTreeLeaf in
                            let bst0 = BinarySearchTreeLeaf in
                            Return
                              (BinarySearchTreeTree
                                  (key, left,
                                    (BinarySearchTreeTree (value, bst, bst0))))
                        | BinarySearchTreeTree (x2, bst, bst0) ->
                            (match insert_fwd_back
                                      (BinarySearchTreeTree (x2, bst, bst0))
                                      value
                              with
                              | Return x3 ->
                                  Return
                                    (BinarySearchTreeTree (key, left, x3))
                              | Fail e -> Fail e))
                    | MyOrderingGreater ->
                        (match right with
                        | BinarySearchTreeLeaf ->
                            let bst = BinarySearchTreeLeaf in
                            let bst0 = BinarySearchTreeLeaf in
                            Return
                              (BinarySearchTreeTree
                                  (key, left,
                                    (BinarySearchTreeTree (value, bst, bst0))))
                        | BinarySearchTreeTree (x2, bst, bst0) ->
                            (match insert_fwd_back
                                      (BinarySearchTreeTree (x2, bst, bst0))
                                      value
                              with
                              | Return x3 ->
                                  Return
                                    (BinarySearchTreeTree (key, left, x3))
                              | Fail e -> Fail e))
                    | MyOrderingLess ->
                        (match left with
                        | BinarySearchTreeLeaf ->
                            let bst = BinarySearchTreeLeaf in
                            let bst0 = BinarySearchTreeLeaf in
                            Return
                              (BinarySearchTreeTree
                                  (key,
                                    (BinarySearchTreeTree (value, bst, bst0)),
                                    right))
                        | BinarySearchTreeTree (x2, bst, bst0) ->
                            (match insert_fwd_back
                                      (BinarySearchTreeTree (x2, bst, bst0))
                                      value
                              with
                              | Return x3 ->
                                  Return
                                    (BinarySearchTreeTree (key, x3, right))
                              | Fail e -> Fail e)))
              | Fail e -> Fail e))
    | Fail e -> Fail e