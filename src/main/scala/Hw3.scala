package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {

}

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  def doInterpret(env: Env, mem: Mem, expr: Expr): Val = expr match {
    
  }

  def eval(group: Tuple4[Env, Mem, Expr, Val]) = {
    val env = group._1;
    val mem = group._2;
    val expr = group._3;
    val value = group._4;
    expr match {
      case Const(n) => (env, mem, expr, IntVal(n));
      case (vrbl: Var) => {
        if (env.contains(vrbl)) {
          env(vrbl) match {
            case (contents: LocVal) => (env, mem, expr, mem.m(contents.l));
            case _ => (env, mem, expr, env(vrbl));
          }
        }
        else throw new UndefinedSemantics(s"undefined variable: ${vrbl}");
      }
      case Add(l, r) => {
        val left = eval(env, mem, l, value);
        val right = eval(env, left._2, r, value);
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, IntVal(sinister.n + dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} + ${r}");
        }
      }
      case Sub(l, r) => (doInterpret(env, mem, l), doInterpret(env, mem, r)) match {
        case (l: IntVal, r: IntVal) => IntVal(l.n - r.n);
        case _ => throw new UndefinedSemantics(s"No semantics for ${l} - ${r}");
      }
      case Mul(l, r) => (doInterpret(env, mem, l), doInterpret(env, mem, r)) match {
        case (l: IntVal, r: IntVal) => IntVal(l.n * r.n);
        case _ => throw new UndefinedSemantics(s"No semantics for ${l} * ${r}");
      }
      case Div(l, r) => (doInterpret(env, mem, l), doInterpret(env, mem, r)) match {
        case (l: IntVal, r: IntVal) => IntVal(l.n / r.n);
        case _ => throw new UndefinedSemantics(s"No semantics for ${l} / ${r}");
      }
      case GTExpr(l, r) => (doInterpret(env, mem, l), doInterpret(env, mem, r)) match {
        case (l: IntVal, r: IntVal) => BoolVal(l.n > r.n);
        case _ => throw new UndefinedSemantics(s"No semantics for ${l} > ${r}");
      }
      case GEQExpr(l, r) => (doInterpret(env, mem, l), doInterpret(env, mem, r)) match {
        case (l: IntVal, r: IntVal) => BoolVal(l.n >= r.n);
        case _ => throw new UndefinedSemantics(s"No semantics for ${l} >= ${r}");
      }
      case Iszero(c) => doInterpret(env, mem, c) match {
        case (c: IntVal) => BoolVal(c.n == 0);
        case _ => throw new UndefinedSemantics(s"Type error: ${c}");
      }
      case Ite(c, t, f) => doInterpret(env, mem, c) match {
        case (c: BoolVal) => if (c.b) doInterpret(env, mem, t); else doInterpret(env, mem, f);
        case _ => throw new UndefinedSemantics(s"Type error: ${c}");
      }
      case ValExpr(name, value, body) => {
        val new_env = env + (name -> doInterpret(env, mem, value));
        doInterpret(new_env, mem, body);
      }
      case VarExpr(name, value, body) => {
        val new_env = env + (name -> LocVal(mem.top + 1));
        if (mem.m.contains(mem.top + 1)) throw new UndefinedSemantics(s"No semantics for VarExpr");
        val new_mem = Mem(mem.m + (mem.top + 1 -> doInterpret(env, mem, value)), mem.top + 1);
        doInterpret(new_env, new_mem, body);
      }
      case Proc(v, expr) => {
        ProcVal(v, expr, env);
      }
      case DefExpr(fname, aname, fbody, ibody) => {
        val new_env = env + (fname -> RecProcVal(fname, aname, fbody, env));
        doInterpret(new_env, mem, ibody);
      }
      case Asn(v, e) => {
        val new_mem = env(v) match {
          case LocVal(l) => {
            Mem(mem.m + (l -> doInterpret(env, mem, e)), mem.top + 1);
          }
          case _ => throw new UndefinedSemantics(s"No semantics for ${Asn(v, e)}");
        }
        doInterpret(env, new_mem, v);
      }
      case Paren(expr) => doInterpret(env, mem, expr);
      case Block(f, s) => {
        doInterpret(env, mem, f);
        doInterpret(env, mem, s);
      }
      case PCall(ftn, arg) => doInterpret(env, mem, ftn) match {
        case (ftn: ProcVal) => {
          val new_env = env + (ftn.v -> doInterpret(env, mem, arg));
          doInterpret(new_env, mem, ftn.expr);
        }
        case (ftn: RecProcVal) => {
          val new_env = env + (ftn.av -> doInterpret(env, mem, arg));
          doInterpret(new_env, mem, ftn.body);
        }
        case _ => throw new UndefinedSemantics(s"Type error: ${ftn}");
      }
    }
  }
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    doInterpret(new Env(), Mem(new HashMap[Loc,Val],0), parsed)
  }

}


object Hw3App extends App {
  
  println("Hello from Hw3!")

}