## About

Rough draft of a "toy" programming language for experimentation and learning PL concepts, ported and extended 
from an original java implementation to learn rust. Will be slowly developed over-time and is a semi-serious 
personal endeavour to create a class-based, lisp inspired language for personal use.

Current implementation is pretty rough and is a simple tree-walk interpreter, with no gc and some messy code from
iterating and changing things often.



## Progress
- [x] Tree Walk Interpreter
    - [x] First-Class Functions
    - [X] Closures
    - [x] Structs
    - [x] Classes
    - [x] Methods
    - [x] Fields
    - [ ] Interfaces
    - [ ] Abstract Classes
    - [ ] Namespaces
    - [ ] Types
- [ ] Bytecode VM
- [ ] Garbage Collection



## General

Implementation aims to be a class based lisp style language inspired some by kawa scheme  with static typing, classes,
and include  some annotation driven code generation/meta programming.

The class structure aims to mirror a java style implementation with more restrictions. Every non-primitive complex
data type will extend a base object class. Inheritance will be limited to only a class extending multiple structs,
a single abstract class, and interface implementations.

To deal with nested access to class fields/methods, "accessors" patterns are used, these look more like traditional
property/member access in other languages as to avoid nested expression based method and field calls.


For example in traditional c-style languages:
```java
MyClassInst.method(arg).field
```

With Accessors:
```
(MyClassInst::method[arg]:.field)
```

Which is arguably easier to grok then nested expression calls:
```lisp
(field (method MyClassInst arg)))
```

Which can get messy when you need to go deeper than 2 levels (though this could be considered an anti-pattern itself).


Typing is accomplished with post-fix type specification of ```::Type``` some opt in dynamic typing will be allowed,
as well as simple type inference.

Ex:
```lisp
(defunc my-func ::float [arg1 ::int arg2 ::int] (/ arg1 arg2))
(lambda ::int (arg ::int) (* arg arg))
(define div ::lambda[num num]:float  (lambda ::float (arg1 ::num arg2 ::num) (/ arg1 arg2))) 
```

Typing is currently not implemented and syntax is not concrete yet.


Modifier are implemented to allow for simple meta programming, these allow for specifying things like access-modifiers
but will also be expanded over time to allow for simple annotation drive meta programming for this like classes.



## Syntax

Most syntax is lisp/scheme inspired, with the main variance being build-in functions and how class members
are accessed.



### Functions

- #### Definitions

     Definitions have an implicit "begin" and allow for multiple expressions to follow arguments

    ```scheme
  (defunc my-func [param1 param2] (expr1) (expr2))
    ```
    
    Functions can also be defined in traditional scheme style syntax
  ```scheme
   (define my-func (lamba (param1 param2) (expr1) (expr2)))
  ```
    

- #### Calls
    ```
    (my-func arg1 arg2)
    ```

<br>

### Lambdas

As outline above lambda use tradition scheme/lisp syntax, but also allow include a sugared implementation:

```scheme
(=> (arg1 args) (expr))
```


#### Structs


