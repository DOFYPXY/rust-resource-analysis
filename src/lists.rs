enum List {
    Cons(u32, Box<List>),
    Nil,
}
use List::Cons;
use List::Nil;

fn tick() {}

fn list_nth_mut<'a>(l: &'a mut List, i: u32) -> &'a mut u32 {
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

/// Same as [list_nth_mut] but with a loop
pub fn list_nth_mut_loop(mut ls: &mut List, mut i: u32) -> &mut u32 {
    while let List::Cons(x, tl) = ls {
        if i == 0 {
            return x;
        } else {
            tick();
            ls = tl;
            i -= 1;
        }
    }
    panic!()
}

fn list_plus_1<'a>(l: &'a mut List) -> u32 {
    match l {
        Nil => {
            return 233;
        }
        Cons(x, tl) => {
            *x = *x + 1;
            let ret = list_plus_1(tl);
            return ret;
        }
    }
}

fn list_tricky<'a>(l: &'a mut List) -> u32 {
    match l {
        Nil => {
            return 233;
        }
        Cons(x, tl) => {
            *x = *x + 1;
            let ret = list_tricky(tl);
            match **tl {
                Nil => {}
                Cons(_, _) => {}
            }
            return ret;
        }
    }
}

// fn list_plus_1_fail<'a>(l: &'a mut List) -> u32 {
//     // let mut ret = 0;
//     match l {
//         Nil => {}
//         Cons(x, tl) => {
//             *x = *x + 1;
//             let ret = list_plus_1_fail(tl);
//         }
//     }
//     // return 0;
//     match l {
//         Nil => {
//             return 233;
//         }
//         Cons(_, _) => {
//             return 233;
//         }
//     }
// }

// fn sum(l: &List<i32>) -> i32 {
//     match l {
//         Nil => {
//             return 0;
//         }
//         Cons(x, tl) => {
//             return *x + sum(tl);
//         }
//     }
// }
