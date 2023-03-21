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

fn find_min<T>(l: List<T>) -> List<T> {
    match l {
        List::Nil => {
            return List::Nil;
        }
        List::Cons(x, xs) => match find_min(*xs) {
            List::Nil => {
                return List::Cons(x, Box::new(List::Nil));
            }
            List::Cons(y, ys) => {
                tick();
                if comp(&x, &y) {
                    return List::Cons(x, Box::new(List::Cons(y, ys)));
                } else {
                    return List::Cons(y, Box::new(List::Cons(x, ys)));
                }
            }
        },
    }
}

fn sort<T>(l: List<T>) -> List<T> {
    match find_min(l) {
        List::Nil => return List::Nil,
        List::Cons(hd, tl) => {
            tick();
            return List::Cons(hd, Box::new(sort(*tl)));
        }
    }
}
