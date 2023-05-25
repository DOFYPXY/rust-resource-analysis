enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}
// use List::Cons;
// use List::Nil;

fn tick() {}

fn comp<T>(x: &T, y: &T) -> bool {
    // TODO
    return false;
}

// fn myappend<T>(l1: &mut List<T>, l2: List<T>) {
//     tick();
//     match l1 {
//         List::Cons(x, ref mut xs) => match &mut **xs {
//             List::Nil => *xs = Box::new(l2),
//             List::Cons(y, ys) => myappend(&mut *ys, l2),
//         },
//         List::Nil => *l1 = l2,
//     }
// }

fn append<T>(l1: List<T>, l2: List<T>) -> List<T> {
    tick();
    match l1 {
        List::Nil => l2,
        List::Cons(x, xs) => List::Cons(x, Box::new(append(*xs, l2))),
    }
}

fn partition<T>(l: List<T>, x: &T) -> (List<T>, List<T>) {
    tick();
    match l {
        List::Nil => return (List::Nil, List::Nil),
        List::Cons(hd, tl) => {
            let (cs, bs) = partition(*tl, x);
            if comp(x, &hd) {
                return (cs, List::Cons(hd, Box::new(bs)));
            } else {
                return (List::Cons(hd, Box::new(cs)), bs);
            }
        }
    }
}

fn sort<T>(l: List<T>) -> List<T> {
    tick();
    match l {
        List::Nil => List::Nil,
        List::Cons(x, xs) => {
            let (ys, zs) = partition(*xs, &x);
            append(sort(ys), List::Cons(x, Box::new(sort(zs))))
        }
    }
}
