//! The examples from the ICFP submission, all in one place.
#![allow(dead_code)]

fn tick() {}

enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}
use List::Cons;
use List::Nil;

// fn append<T>(x: &mut List<T>, y: &mut List<T>) {

// }

fn list_nth_mut<'a, T>(l: &'a mut List<T>, i: u32) -> &'a mut T {
    match l {
        Nil => {
            panic!()
        }
        Cons(x, tl) => {
            if i == 0 {
                return x;
            } else {
                return list_nth_mut(tl, i - 1);
            }
        }
    }
}
