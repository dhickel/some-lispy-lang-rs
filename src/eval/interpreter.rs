// use std::cell::RefCell;
// use std::mem;
// use std::rc::Rc;
// use std::time::{SystemTime, UNIX_EPOCH};
// use AstNode::LiteralNode;
// use crate::eval::environment::{Binding, Environment};
// use crate::eval::operation_eval;
// use crate::parse;
// use crate::parse::ast_nodes::{AssignData, AstNode, CondData, ConsData, DefLambdaData, DefNode, ExprNode, FuncArg, FuncCallData, IfData, LambdaValue, ListAccData, LitNode, OpNode, Param, WhileData};
// use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};
// 
// 
// macro_rules! nano_time {
//     () => {
//         SystemTime::now()
//             .duration_since(UNIX_EPOCH)
//             .expect("Time went backwards")
//             .as_nanos() // Returns the number of nanoseconds since the Unix epoch
//     };
// }
// 
// 
// pub struct Interp {
//     pub env: Rc<RefCell<Environment>>,
// }
// 
// impl Interp {
//     pub fn repl_eval(&mut self, input: String) -> String {
//         let start = nano_time!();
//         let proc_time;
//         let eval_time;
//         let total_time;
//         let tokens = parse::lexer::process(input);
//         println!("{:?}", tokens);
//         let ast = match tokens {
//             Ok(tokens) => parse::parser::process(tokens),
//             Err(msg) => return msg
//         };
//         proc_time = nano_time!() - start;
//         let eval_start = nano_time!();
//         let result = match ast {
//             Ok(ast) => self.eval(ast).join("\n"),
//             Err(error) => return error
//         };
//         eval_time = nano_time!() - eval_start;
//         total_time = nano_time!() - start;
//         println!("Processing: {}", proc_time);
//         println!("Eval: {}", eval_time);
//         println!("Total: {}", total_time);
//         result
//     }
// 
// 
//     pub fn eval(&mut self, input: Vec<AstNode>) -> Vec<String> {
//         let mut evaled = Vec::<String>::new();
//         for node in input {
//             match &self.eval_node(node) {
//                 Ok(rtn) => if let LiteralNode(any) = rtn {
//                     evaled.push(any.value().as_string())
//                 } else { evaled.push("Fatal: Invalid Eval Return".to_string()) }
//                 Err(msg) => evaled.push(msg.clone())
//             }
//         }
//         evaled
//     }
// 
// 
//     fn eval_node(&mut self, node: AstNode) -> Result<AstNode, String> {
//         match node {
//             ExpressionNode(expr) => self.eval_expression(*expr),
//             LiteralNode(_) => Ok(node),
//             OperationNode(op) => self.eval_operation(op),
//             DefinitionNode(def) => self.eval_definition(*def),
//             ProgramNode(_) => Err("Fatal: Found Nested Program Node".to_string())
//         }
//     }
// 
//     fn eval_operation(&mut self, node: OpNode) -> Result<AstNode, String> {
//         match node {
//             OpNode::Addition(op_nodes) => Ok(operation_eval::add(self.eval_operands(op_nodes)?)),
//             OpNode::Subtraction(op_nodes) => Ok(operation_eval::subtract(self.eval_operands(op_nodes)?)),
//             OpNode::Multiplication(op_nodes) => Ok(operation_eval::multiply(self.eval_operands(op_nodes)?)),
//             OpNode::Division(op_nodes) => Ok(operation_eval::divide(self.eval_operands(op_nodes)?)),
//             OpNode::Modulo(op_nodes) => Ok(operation_eval::modulo(self.eval_operands(op_nodes)?)),
//             OpNode::Exponentiate(op_nodes) => Ok(operation_eval::exponentiate(self.eval_operands(op_nodes)?)),
//             OpNode::Increment(op_nodes) => Ok(operation_eval::increment(self.eval_operands(op_nodes)?)),
//             OpNode::Decrement(op_nodes) => Ok(operation_eval::decrement(self.eval_operands(op_nodes)?)),
//             OpNode::Or(op_nodes) => Ok(operation_eval::or(self.eval_operands(op_nodes)?)),
//             OpNode::And(op_nodes) => Ok(operation_eval::and(self.eval_operands(op_nodes)?)),
//             OpNode::Xor(op_nodes) => Ok(operation_eval::xor(self.eval_operands(op_nodes)?)),
//             OpNode::Nor(op_nodes) => Ok(operation_eval::nor(self.eval_operands(op_nodes)?)),
//             OpNode::Xnor(op_nodes) => Ok(operation_eval::xnor(self.eval_operands(op_nodes)?)),
//             OpNode::Nand(op_nodes) => Ok(operation_eval::nand(self.eval_operands(op_nodes)?)),
//             OpNode::Negate(op_nodes) => Ok(operation_eval::negate(self.eval_operands(op_nodes)?)),
//             OpNode::GreaterThan(op_nodes) => Ok(operation_eval::greater_than(self.eval_operands(op_nodes)?)),
//             OpNode::GreaterThanEqual(op_nodes) => Ok(operation_eval::greater_than_eq(self.eval_operands(op_nodes)?)),
//             OpNode::LessThan(op_nodes) => Ok(operation_eval::less_than(self.eval_operands(op_nodes)?)),
//             OpNode::LessThanEqual(op_nodes) => Ok(operation_eval::less_than_eq(self.eval_operands(op_nodes)?)),
//             OpNode::Equality(op_nodes) => Ok(operation_eval::ref_eqaulity(self.eval_operands(op_nodes)?)),
//             OpNode::RefEquality(op_nodes) => Ok(operation_eval::value_equality(self.eval_operands(op_nodes)?)),
//             OpNode::RefNonEquality(op_nodes) => Ok(operation_eval::value_non_equality(self.eval_operands(op_nodes)?))
//         }
//     }
//     fn eval_operands(&mut self, op_nodes: Vec<AstNode>) -> Result<(bool, Vec<LitNode>), String> {
//         let mut operands = Vec::<LitNode>::with_capacity(op_nodes.len());
//         let mut is_float = false;
//         for node in op_nodes {
//             match self.eval_node(node)? {
//                 LiteralNode(val) => {
//                     if matches!(val, LitNode::Float(_)) {
//                         is_float = true;
//                     }
//                     operands.push(val);
//                 }
//                 _ => return Err("Operand did not evaluate to literal".to_string()),
//             }
//         }
//         Ok((is_float, operands))
//     }
// 
//     fn eval_definition(&mut self, node: DefNode) -> Result<AstNode, String> {
//         match node {
//             DefNode::Variable(var) => {
//                 if let Ok(evaled_var) = self.eval_node(*var.value) {
//                     let binding = Binding::new_binding(evaled_var, var.modifiers);
//                     let bind = self.env.borrow_mut().create_binding(var.name, binding?);
//                     if let Err(s) = bind {
//                         Err(s.clone())
//                     } else { Ok(AstNode::new_bool_lit(true)) }
//                 } else { Err("Binding did not eval to literal".to_string()) }
//             }
//             DefNode::Lambda(lam) => {
//                 Ok(AstNode::new_lambda_lit(lam, Rc::clone(&self.env)))
//             }
//             DefNode::Function(fun) => {
//                 let mods = fun.lambda.modifiers.clone();
//                 let lambda_lit = AstNode::new_lambda_lit(fun.lambda, Rc::clone(&self.env));
//                 let binding = Binding::new_binding(lambda_lit, mods);
//                 let bind = self.env.borrow_mut().create_binding(fun.name, binding?);
//                 if let Err(s) = bind {
//                     Err(s.clone())
//                 } else { Ok(AstNode::new_bool_lit(true)) }
//             }
//         }
//     }
// 
// 
//     fn eval_expression(&mut self, node: ExprNode) -> Result<AstNode, String> {
//         match node {
//             ExprNode::Assignment(data) => self.eval_assignment(data),
//             ExprNode::MultiExpr(data) => self.eval_multi_expr(data),
//             ExprNode::PrintExpr(data) => self.eval_print_expr(data),
//             ExprNode::IfExpr(data) => self.eval_if_expr(data),
//             ExprNode::CondExpr(data) => self.eval_cond_expr(data),
//             ExprNode::WhileLoop(data) => self.eval_while_expr(data),
//             ExprNode::ConsExpr(data) => self.eval_cons_expr(data),
//             ExprNode::PairList(data) => self.eval_pair_list_expr(data),
//             ExprNode::ListAccess(data) => self.eval_list_acc_expr(data),
//             ExprNode::FuncCall(data) => self.eval_func_call_expr(data),
//             ExprNode::LiteralCall(data) => self.eval_lit_call_expr(data),
//         }
//     }
// 
//     fn eval_assignment(&self, expr: AssignData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_multi_expr(&self, expr: Vec<AstNode>) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_print_expr(&self, expr: AstNode) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_if_expr(&self, expr: IfData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_cond_expr(&self, expr: CondData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_while_expr(&self, expr: WhileData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_cons_expr(&self, expr: ConsData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_pair_list_expr(&self, expr: Vec<AstNode>) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_list_acc_expr(&self, expr: ListAccData) -> Result<AstNode, String> {
//         panic!("Not Implemented")
//     }
// 
//     fn eval_func_call_expr(&mut self, call: FuncCallData) -> Result<AstNode, String> {
//         if let Some((LitNode::Lambda(lambda))) = self.env.clone().borrow_mut().get_literal(&call.name) {           
//             let env: Rc<RefCell<Environment>>;
//             if lambda.def.parameters.is_some() && call.arguments.is_some() {
//                 env = self.map_param_to_env(
//                     lambda.def.parameters.unwrap(),
//                     call.arguments.unwrap(),
//                     Environment::of(Rc::clone(&lambda.env)))?;
//             } else if lambda.def.parameters.is_none() {
//                 env = Rc::clone(&lambda.env);
//             } else {
//                 return Err("Parameter-argument mismatch".to_string());
//             }
//             let prev_env = std::mem::replace(&mut self.env, env); 
//             let result = self.eval_node(lambda.def.body);
//             self.env = prev_env; // restore the old active one 
//             result
//         } else {
//             Err("Binding is not a lambda".to_string())
//         }
//     }
// 
// 
//     fn map_param_to_env(&mut self, mut params: Vec<Param>, mut args: Vec<FuncArg>, mut env: Rc<RefCell<Environment>>) -> Result<Rc<RefCell<Environment>>, String> {
//         println!("At map params");
//         println!("params len:{}", params.len());
//         if params.len() == args.len() {
//             for i in 0..args.len() {
//                 let arg = args.remove(i);
//                 let ast = self.eval_node(arg.value)?;
//                 let binding = Binding::new_binding(ast, None)?;
//                 println!("Created binding for: {}", &params[i].name);
//                 env.borrow_mut().create_binding(params[i].name.clone(), binding)?;
//             }
//             Ok(env)
//         } else {
//             panic!();
//             Err("Arg len mismatch".to_string())
//         }
//     }
// 
//     fn eval_lit_call_expr(&self, name: String) -> Result<AstNode, String> {
//         if let Some(lit) = self.env.borrow_mut().get_literal(&name) {
//             Ok(LiteralNode(lit.clone()))
//         } else { Err(format!("Failed to find binding for: {}", name)) }
//     }
// }