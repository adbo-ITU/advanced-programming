// Like "Nothing", but it does _not_ hold that forall t . EmptyType <: t.
// That is reserved only for "Nothing".

enum EmptyType:
  case E(e: EmptyType)

enum Op:
  case Add
  case Sub
  case Mul
  case Div

// An ADT modeling an abstract syntax tree. Note the seemingly useless
// contra-variant type parameter A
enum AST[-A]:
  case ConstInt(c : Int) extends AST[Nothing]
  case BinOp[A](a : A, op : Op, lhs : AST[A], rhs : AST[A]) extends AST[A]

  def toInt() = this match
    case ConstInt(x) => x
    case _ => throw Exception("Not a constant")

// This type is inhabited by both, ConstInt and BinOp:
// - ConstInt only needs an integer argument, it does not care about the type of A.
// - BinOp needs an instance of A - in this case the only value inhabiting Unit, ().
type Expr = AST[Unit]

// This type is not inhabited by BinOp, because that would require to
// call the BinOp constructor with a value of type EmptyType, which we
// cannot construct.
// Note: no sub-typing relation between ValueA and Expr
type ValueA = AST[EmptyType]

// This type is not inhabited by BinOp, because that would require to
// call the BinOp constructor with a value of type Nothing, which we
// cannot construct.
// Note: Expr <: ValueB because Nothing <: Unit
type ValueB = AST[Nothing]

// The following two functions are now guaranteed to never fail, but
// unfortunately, the Scala compiler does not see that.

def toIntA(c : ValueA) : Int = c match
  case AST.ConstInt(x) => x // Warns with "pattern matching may not be exhaustive"

def toIntB(c : ValueB) : Int = c match
  case AST.ConstInt(x) => x // Warns with "pattern matching may not be exhaustive"

// If you are curious, try to implement an evaluation function for
// each variant. There is a subtle difference between evalA and evalB
// owned to contra-variance.
def evalA(e : Expr) : ValueA = ???

def evalB(e : Expr) : ValueB = ???
