use std::fmt;
use crate::parse::ast_nodes::LitNode;

#[derive(Debug)]
pub struct Pair {
    pub car: LitNode,
    pub cdr: LitNode,
}

// impl fmt::Display for Pair {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         fn recursive_fmt(pair: &Pair, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//             write!(f, "({}", pair.car.string_value())?;
//         }
//     }
    //         match &pair.cdr {
    //             LitNode::Pair(next_pair) => {
    //                 write!(f, ", ")?;
    //                 recursive_fmt(next_pair, f)?;
    //             }
    //             LitNode::Nil => write!(f, ", Nil")?,
    //             _ => write!(f, ", {}", pair.cdr.string_value())?,
    //         }
    //         write!(f, ")")
    //     }
    //     recursive_fmt(self, f)
    // }
//}

// impl fmt::Display for Pair {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         if !matches!(self.cdr, LitNode::Pair(_)) {
//             return write!(f, "({}, {})", self.car.string_value(), self.cdr.string_value());
//         }
//         let mut string = String::new();
//         string.push('(');
//         let mut curr_pair = self;
//         loop {
//             string.push_str(&curr_pair.car.string_value());
//             match &curr_pair.cdr {
//                 LitNode::Pair(next_pair) => {
//                     string.push_str(", ");
//                     curr_pair = &next_pair;
//                 }
//                 LitNode::Nil => break,
//                 _ => {
//                     string.push_str(" ,");
//                     string.push_str(&curr_pair.cdr.string_value());
//                     break;
//                 }
//             }
//         }
//         string.push(')');
//         write!(f, "{}", string)
//     }
// }