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

fn insert<T>(v: T, l: List<T>) -> List<T> {
    tick();
    match l {
        List::Nil => {
            return List::Cons(v, Box::new(List::Nil));
        }
        List::Cons(hd, tl) => {
            if comp(&v, &hd) {
                return List::Cons(v, Box::new(List::Cons(hd, tl)));
            } else {
                return List::Cons(hd, Box::new(insert(v, *tl)));
            }
        }
    }
}

fn sort<T>(l: List<T>) -> List<T> {
    tick();
    match l {
        List::Nil => {
            return List::Nil;
        }
        List::Cons(hd, tl) => return insert(hd, sort(*tl)),
    }
}
