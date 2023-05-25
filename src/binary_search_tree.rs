// pub struct BinarySearchTree<T> {
//     value: Option<T>,
//     left: Option<Box<BinarySearchTree<T>>>,
//     right: Option<Box<BinarySearchTree<T>>>,
// }

enum BinarySearchTree<T> {
    Leaf,
    Tree(T, Box<BinarySearchTree<T>>, Box<BinarySearchTree<T>>),
}

use BinarySearchTree::{Leaf, Tree};

enum MyOrdering {
    Equal,
    Greater,
    Less,
}

fn tick() {}

fn comp<T>(x: &T, y: &T) -> MyOrdering {
    // TODO
    return MyOrdering::Less;
}

// impl<T> BinarySearchTree<T> {
//     /// Create a new, empty BST
//     pub fn new() -> BinarySearchTree<T> {
//         BinarySearchTree {
//             value: None,
//             left: None,
//             right: None,
//         }
//     }

/// Find a value in this tree. Returns True if value is in this
/// tree, and false otherwise
// pub fn search(&self, value: &T) -> bool {
//     match &self.value {
//         Some(key) => {
//             tick();
//             match comp(key, value) {
//                 MyOrdering::Equal => {
//                     // key == value
//                     true
//                 }
//                 MyOrdering::Greater => {
//                     // key > value
//                     match &self.left {
//                         Some(node) => node.search(value),
//                         None => false,
//                     }
//                 }
//                 MyOrdering::Less => {
//                     // key < value
//                     match &self.right {
//                         Some(node) => node.search(value),
//                         None => false,
//                     }
//                 }
//             }
//         }
//         None => false,
//     }
// }

fn search<T>(l: &BinarySearchTree<T>, value: &T) -> bool {
    tick();
    match l {
        Tree(key, left, right) => match comp(key, value) {
            MyOrdering::Equal => true,
            MyOrdering::Greater => search(left, value),
            MyOrdering::Less => search(right, value),
        },
        Leaf => false,
    }
}

// /// Returns a new iterator which iterates over this tree in order
// pub fn iter(&self) -> impl Iterator<Item = &T> {
//     BinarySearchTreeIter::new(self)
// }

// /// Insert a value into the appropriate location in this tree.
// pub fn insert(&mut self, value: T) {
//     if self.value.is_none() {
//         self.value = Some(value);
//     } else {
//         match &self.value {
//             None => (),
//             Some(key) => {
//                 let target_node = if let MyOrdering::Less = comp(&value, key) {
//                     &mut self.left
//                 } else {
//                     &mut self.right
//                 };
//                 match target_node {
//                     Some(ref mut node) => {
//                         node.insert(value);
//                     }
//                     None => {
//                         let mut node = BinarySearchTree::new();
//                         node.insert(value);
//                         *target_node = Some(Box::new(node));
//                     }
//                 }
//             }
//         }
//     }
// }

fn insert<T>(l: &mut BinarySearchTree<T>, value: T) {
    tick();
    if let Tree(key, left, right) = l {
        let target_node = if let MyOrdering::Less = comp(&value, key) {
            &mut **left
        } else {
            &mut **right
        };
        match target_node {
            Tree(_, _, _) => insert(target_node, value),
            Leaf => {
                let node = Tree(value, Box::new(Leaf), Box::new(Leaf));
                *target_node = node;
            }
        }
    }
}

// /// Returns the smallest value in this tree
// pub fn minimum(&self) -> Option<&T> {
//     match &self.left {
//         Some(node) => node.minimum(),
//         None => self.value.as_ref(),
//     }
// }

// /// Returns the largest value in this tree
// pub fn maximum(&self) -> Option<&T> {
//     match &self.right {
//         Some(node) => node.maximum(),
//         None => self.value.as_ref(),
//     }
// }

// /// Returns the largest value in this tree smaller than value
// pub fn floor(&self, value: &T) -> Option<&T> {
//     match &self.value {
//         Some(key) => {
//             match comp(key, value) {
//                 MyOrdering::Greater => {
//                     // key > value
//                     match &self.left {
//                         Some(node) => node.floor(value),
//                         None => None,
//                     }
//                 }
//                 MyOrdering::Less => {
//                     // key < value
//                     match &self.right {
//                         Some(node) => {
//                             let val = node.floor(value);
//                             match val {
//                                 Some(_) => val,
//                                 None => Some(key),
//                             }
//                         }
//                         None => Some(key),
//                     }
//                 }
//                 MyOrdering::Equal => Some(key),
//             }
//         }
//         None => None,
//     }
// }

// /// Returns the smallest value in this tree larger than value
// pub fn ceil(&self, value: &T) -> Option<&T> {
//     match &self.value {
//         Some(key) => {
//             match comp(key, value) {
//                 MyOrdering::Less => {
//                     // key < value
//                     match &self.right {
//                         Some(node) => node.ceil(value),
//                         None => None,
//                     }
//                 }
//                 MyOrdering::Greater => {
//                     // key > value
//                     match &self.left {
//                         Some(node) => {
//                             let val = node.ceil(value);
//                             match val {
//                                 Some(_) => val,
//                                 None => Some(key),
//                             }
//                         }
//                         None => Some(key),
//                     }
//                 }
//                 MyOrdering::Equal => {
//                     // key == value
//                     Some(key)
//                 }
//             }
//         }
//         None => None,
//     }
// }
// }
