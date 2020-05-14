# lambda-hs

## Things I explored in this project

One of my major goals with this project was to explore various ways that
I could write decently clean compiler code. With that goal in mind, I explored 
a few different areas to help with code reuse and cleanliness which I've detailed
below.

### Generic Tree Traversal

About half way through the project, I was realizing that a lot of the tree
transformations and traversals that I was going to need to do would get quite 
repeditive. I decided to investigate various ways of overcoming this issue. 

#### A simple example

For example, say I had a tree defined like so:

```haskell
data Tree = Leaf Int
          | Branch Tree Tree
``` 

If I wanted to traverse this tree but I only cared about the 
Leaf nodes and not the branches, I still need to define the traversal
function for the Branch nodes. In this simple exmple, this isn't a big
deal but for my large AST I thought it would be neat to have a generic traversal
method for all the nodes and be able to define specific operations to perform on 
a specific node type. After a lot of digging, I found the library (alloy)[https://hackage.haskell.org/package/alloy] which helps with this a great deal. 

A good example of how I used this library can be found in ClosureConvert.hs
The whole process uses it but I think one of the more elegent functions is the
`makeEnvRefCalls` function. 

```haskell
makeEnvRefCalls :: Env -> VarSet -> Expr -> Expr
makeEnvRefCalls env fv = makeRecurse ops
  where 
    -- Define the operations for this traversal
    -- First: Check to see if we need to stop the traversal
    -- Then: Check to see if we need to replace the current node
    -- Finally: Perform the generic traversal if we've reached this point
    ops :: Expr :- Var :- BaseOp
    ops = stop :- replace :- baseOp

    -- This function continues the traversal until it hits another abstraction
    -- node in the AST. Essentially, it allows us to apply the 'replace' operation
    -- on only the current function body, not the desendants'.
    stop :: Expr -> Expr 
    stop e = case e of 
      EAbs _ -> e                   -- Stop descent if we hit another lambda
      _      -> makeDescend ops e   -- Otherwise continue descent
    
    -- This is the actual transformation to perform on the variable nodes
    -- If the variable is in the set of free variables, we need to replace
    -- it with an environment reference. Otherwise continue the traversal 
    -- like normal.
    replace :: Var -> Var
    replace e = case e of 
      Var n | Set.member n fv -> EnvRef env n
      _ -> e
```

I used this traversal library to perform almost all the AST transformations. I found
it to be a very interesting topic to explore. 

### Generic Programming

Similar to the generic tree traversals, I was finding that a lot of my data types 
had the structure:

```haskell
data Expr
  = ELit Literal 
  | EAbs Abstraction
  | EApp App 
  | EVar Var
  | EAssign Assign
```

Notice each type constructor only has one value associated with it. During the codegen
process (and various other places), I was needing to recursively apply a function 
to the entire AST. I found myself repeatedly defining a function that looked something
like

```haskell
traverse :: Expr -> a
traverse (ELit a) = traverse a
traverse (EAbs a) = traverse a
traverse (EApp a) = traverse a
traverse (EVar a) = traverse a
traverse (EAssign a) = traverse a
```

where each implementation is identical. Through quite a bit of research I found
that I could eliminate this through Haskell's generic programming library.
This allowed be to define a way to traverse all type constructors with one value.
A big example of my usage of this can be found in src/AST.hs with my freeVars and 
allVars implementations.

One of the nicest things that came out of this was that I was able to add more
types to my top level Expr type without having to change any other portion of the
code that traversed it. Because it was generic, the freeVar and allVar implemenations
still worked. This was definitely a whole new area for me but I found it very interesting.

