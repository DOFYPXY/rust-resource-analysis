open Prims
let rec list_update : 'a . 'a Prims.list -> Prims.nat -> 'a -> 'a Prims.list
  =
  fun ls ->
    fun i ->
      fun x ->
        match ls with
        | x'::ls1 ->
            if i = Prims.int_zero
            then x :: ls1
            else x' :: (list_update ls1 (i - Prims.int_one) x)
type error =
  | Failure 
  | OutOfFuel 
let (uu___is_Failure : error -> Prims.bool) =
  fun projectee -> match projectee with | Failure -> true | uu___ -> false
let (uu___is_OutOfFuel : error -> Prims.bool) =
  fun projectee -> match projectee with | OutOfFuel -> true | uu___ -> false
type 'a result =
  | Return of 'a 
  | Fail of error 
let uu___is_Return : 'a . 'a result -> Prims.bool =
  fun projectee -> match projectee with | Return v -> true | uu___ -> false
let __proj__Return__item__v : 'a . 'a result -> 'a =
  fun projectee -> match projectee with | Return v -> v
let uu___is_Fail : 'a . 'a result -> Prims.bool =
  fun projectee -> match projectee with | Fail e -> true | uu___ -> false
let __proj__Fail__item__e : 'a . 'a result -> error =
  fun projectee -> match projectee with | Fail e -> e
let return : 'a . 'a -> 'a result = fun x -> Return x
let op_let_Star : 'a 'b . 'a result -> ('a -> 'b result) -> 'b result =
  fun m -> fun f -> match m with | Return x -> f x | Fail e -> Fail e
let (massert : Prims.bool -> unit result) =
  fun b -> if b then Return () else Fail Failure
let eval_global : 'a . 'a result -> 'a = fun x -> __proj__Return__item__v x
type char = FStar_Char.char
type string = Prims.string
let (is_zero : Prims.nat -> Prims.bool) = fun n -> n = Prims.int_zero
let (decrease : Prims.nat -> Prims.nat) = fun n -> n - Prims.int_one
let mem_replace_fwd : 'a . 'a -> 'a -> 'a = fun x -> fun y -> x
let mem_replace_back : 'a . 'a -> 'a -> 'a = fun x -> fun y -> y
let (isize_min : Prims.int) = (Prims.parse_int "-9223372036854775808")
let (isize_max : Prims.int) = (Prims.parse_int "9223372036854775807")
let (i8_min : Prims.int) = (Prims.of_int (-128))
let (i8_max : Prims.int) = (Prims.of_int (127))
let (i16_min : Prims.int) = (Prims.of_int (-32768))
let (i16_max : Prims.int) = (Prims.of_int (32767))
let (i32_min : Prims.int) = (Prims.parse_int "-2147483648")
let (i32_max : Prims.int) = (Prims.parse_int "2147483647")
let (i64_min : Prims.int) = (Prims.parse_int "-9223372036854775808")
let (i64_max : Prims.int) = (Prims.parse_int "9223372036854775807")
let (i128_min : Prims.int) =
  (Prims.parse_int "-170141183460469231731687303715884105728")
let (i128_max : Prims.int) =
  (Prims.parse_int "170141183460469231731687303715884105727")
let (usize_min : Prims.int) = Prims.int_zero
let (usize_max : Prims.int) = (Prims.parse_int "4294967295")
let (u8_min : Prims.int) = Prims.int_zero
let (u8_max : Prims.int) = (Prims.of_int (255))
let (u16_min : Prims.int) = Prims.int_zero
let (u16_max : Prims.int) = (Prims.parse_int "65535")
let (u32_min : Prims.int) = Prims.int_zero
let (u32_max : Prims.int) = (Prims.parse_int "4294967295")
let (u64_min : Prims.int) = Prims.int_zero
let (u64_max : Prims.int) = (Prims.parse_int "18446744073709551615")
let (u128_min : Prims.int) = Prims.int_zero
let (u128_max : Prims.int) =
  (Prims.parse_int "340282366920938463463374607431768211455")
type scalar_ty =
  | Isize 
  | I8 
  | I16 
  | I32 
  | I64 
  | I128 
  | Usize 
  | U8 
  | U16 
  | U32 
  | U64 
  | U128 
let (uu___is_Isize : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | Isize -> true | uu___ -> false
let (uu___is_I8 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | I8 -> true | uu___ -> false
let (uu___is_I16 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | I16 -> true | uu___ -> false
let (uu___is_I32 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | I32 -> true | uu___ -> false
let (uu___is_I64 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | I64 -> true | uu___ -> false
let (uu___is_I128 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | I128 -> true | uu___ -> false
let (uu___is_Usize : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | Usize -> true | uu___ -> false
let (uu___is_U8 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | U8 -> true | uu___ -> false
let (uu___is_U16 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | U16 -> true | uu___ -> false
let (uu___is_U32 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | U32 -> true | uu___ -> false
let (uu___is_U64 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | U64 -> true | uu___ -> false
let (uu___is_U128 : scalar_ty -> Prims.bool) =
  fun projectee -> match projectee with | U128 -> true | uu___ -> false
let (scalar_min : scalar_ty -> Prims.int) =
  fun ty ->
    match ty with
    | Isize -> isize_min
    | I8 -> i8_min
    | I16 -> i16_min
    | I32 -> i32_min
    | I64 -> i64_min
    | I128 -> i128_min
    | Usize -> usize_min
    | U8 -> u8_min
    | U16 -> u16_min
    | U32 -> u32_min
    | U64 -> u64_min
    | U128 -> u128_min
let (scalar_max : scalar_ty -> Prims.int) =
  fun ty ->
    match ty with
    | Isize -> isize_max
    | I8 -> i8_max
    | I16 -> i16_max
    | I32 -> i32_max
    | I64 -> i64_max
    | I128 -> i128_max
    | Usize -> usize_max
    | U8 -> u8_max
    | U16 -> u16_max
    | U32 -> u32_max
    | U64 -> u64_max
    | U128 -> u128_max
type 'ty scalar = Prims.int
let (mk_scalar : scalar_ty -> Prims.int -> unit scalar result) =
  fun ty ->
    fun x ->
      if ((scalar_min ty) <= x) && ((scalar_max ty) >= x)
      then Return x
      else Fail Failure
let (scalar_neg : scalar_ty -> unit scalar -> unit scalar result) =
  fun ty -> fun x -> mk_scalar ty (- x)
let (scalar_div :
  scalar_ty -> unit scalar -> unit scalar -> unit scalar result) =
  fun ty ->
    fun x ->
      fun y ->
        if y <> Prims.int_zero then mk_scalar ty (x / y) else Fail Failure
let (int_rem : Prims.int -> Prims.int -> Prims.int) =
  fun x -> fun y -> if x >= Prims.int_zero then x mod y else - (x mod y)
let (scalar_rem :
  scalar_ty -> unit scalar -> unit scalar -> unit scalar result) =
  fun ty ->
    fun x ->
      fun y ->
        if y <> Prims.int_zero
        then mk_scalar ty (int_rem x y)
        else Fail Failure
let (scalar_add :
  scalar_ty -> unit scalar -> unit scalar -> unit scalar result) =
  fun ty -> fun x -> fun y -> mk_scalar ty (x + y)
let (scalar_sub :
  scalar_ty -> unit scalar -> unit scalar -> unit scalar result) =
  fun ty -> fun x -> fun y -> mk_scalar ty (x - y)
let (scalar_mul :
  scalar_ty -> unit scalar -> unit scalar -> unit scalar result) =
  fun ty -> fun x -> fun y -> mk_scalar ty (x * y)
let (scalar_cast :
  scalar_ty -> scalar_ty -> unit scalar -> unit scalar result) =
  fun src_ty -> fun tgt_ty -> fun x -> mk_scalar tgt_ty x
type isize = unit scalar
type i8 = unit scalar
type i16 = unit scalar
type i32 = unit scalar
type i64 = unit scalar
type i128 = unit scalar
type usize = unit scalar
type u8 = unit scalar
type u16 = unit scalar
type u32 = unit scalar
type u64 = unit scalar
type u128 = unit scalar
let (isize_neg : unit scalar -> unit scalar result) = scalar_neg Isize
let (i8_neg : unit scalar -> unit scalar result) = scalar_neg I8
let (i16_neg : unit scalar -> unit scalar result) = scalar_neg I16
let (i32_neg : unit scalar -> unit scalar result) = scalar_neg I32
let (i64_neg : unit scalar -> unit scalar result) = scalar_neg I64
let (i128_neg : unit scalar -> unit scalar result) = scalar_neg I128
let (isize_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div Isize
let (i8_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div I8
let (i16_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div I16
let (i32_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div I32
let (i64_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div I64
let (i128_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div I128
let (usize_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div Usize
let (u8_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div U8
let (u16_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div U16
let (u32_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div U32
let (u64_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div U64
let (u128_div : unit scalar -> unit scalar -> unit scalar result) =
  scalar_div U128
let (isize_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem Isize
let (i8_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem I8
let (i16_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem I16
let (i32_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem I32
let (i64_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem I64
let (i128_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem I128
let (usize_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem Usize
let (u8_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem U8
let (u16_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem U16
let (u32_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem U32
let (u64_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem U64
let (u128_rem : unit scalar -> unit scalar -> unit scalar result) =
  scalar_rem U128
let (isize_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add Isize
let (i8_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add I8
let (i16_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add I16
let (i32_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add I32
let (i64_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add I64
let (i128_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add I128
let (usize_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add Usize
let (u8_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add U8
let (u16_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add U16
let (u32_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add U32
let (u64_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add U64
let (u128_add : unit scalar -> unit scalar -> unit scalar result) =
  scalar_add U128
let (isize_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub Isize
let (i8_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub I8
let (i16_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub I16
let (i32_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub I32
let (i64_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub I64
let (i128_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub I128
let (usize_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub Usize
let (u8_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub U8
let (u16_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub U16
let (u32_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub U32
let (u64_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub U64
let (u128_sub : unit scalar -> unit scalar -> unit scalar result) =
  scalar_sub U128
let (isize_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul Isize
let (i8_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul I8
let (i16_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul I16
let (i32_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul I32
let (i64_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul I64
let (i128_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul I128
let (usize_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul Usize
let (u8_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul U8
let (u16_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul U16
let (u32_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul U32
let (u64_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul U64
let (u128_mul : unit scalar -> unit scalar -> unit scalar result) =
  scalar_mul U128
type 'a vec = 'a Prims.list
let vec_new : 'a . unit -> 'a vec = fun uu___ -> []
let vec_len : 'a . 'a vec -> usize = fun v -> FStar_List_Tot_Base.length v
let vec_push_back : 'a . 'a vec -> 'a -> 'a vec result =
  fun v ->
    fun x ->
      if (FStar_List_Tot_Base.length v) < usize_max
      then Return (FStar_List_Tot_Base.append v [x])
      else Fail Failure
let vec_insert_fwd : 'a . 'a vec -> usize -> 'a -> unit result =
  fun v ->
    fun i ->
      fun x ->
        if i < (FStar_List_Tot_Base.length v)
        then Return ()
        else Fail Failure
let vec_insert_back : 'a . 'a vec -> usize -> 'a -> 'a vec result =
  fun v ->
    fun i ->
      fun x ->
        if i < (FStar_List_Tot_Base.length v)
        then Return (list_update v i x)
        else Fail Failure
let vec_index_fwd : 'a . 'a vec -> usize -> 'a result =
  fun v ->
    fun i ->
      if i < (FStar_List_Tot_Base.length v)
      then Return (FStar_List_Tot_Base.index v i)
      else Fail Failure
let vec_index_back : 'a . 'a vec -> usize -> 'a -> unit result =
  fun v ->
    fun i ->
      fun x ->
        if i < (FStar_List_Tot_Base.length v)
        then Return ()
        else Fail Failure
let vec_index_mut_fwd : 'a . 'a vec -> usize -> 'a result =
  fun v ->
    fun i ->
      if i < (FStar_List_Tot_Base.length v)
      then Return (FStar_List_Tot_Base.index v i)
      else Fail Failure
let vec_index_mut_back : 'a . 'a vec -> usize -> 'a -> 'a vec result =
  fun v ->
    fun i ->
      fun nx ->
        if i < (FStar_List_Tot_Base.length v)
        then Return (list_update v i nx)
        else Fail Failure