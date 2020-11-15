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
  
  def doInterpret(env: Env, mem: Mem, expr: Expr): Val = eval(env, mem, expr, BoolVal(true))._4;

  def eval(group: Tuple4[Env, Mem, Expr, Val]): Tuple4[Env, Mem, Expr, Val] = {
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
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match { // sinister and dexter are left and right in Latin, respectively
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, IntVal(sinister.n + dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} + ${r}");
        }
      }
      case Sub(l, r) => {
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, IntVal(sinister.n - dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} - ${r}");
        }
      }
      case Mul(l, r) => {
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, IntVal(sinister.n * dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} * ${r}");
        }
      }
      case Div(l, r) => {
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, IntVal(sinister.n / dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} / ${r}");
        }
      }
      case GTExpr(l, r) => {
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, BoolVal(sinister.n > dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} > ${r}");
        }
      }
      case GEQExpr(l, r) => {
        val left = eval((env, mem, l, value));
        val right = eval((env, left._2, r, value));
        (left._4, right._4) match {
          case (sinister: IntVal, dexter: IntVal) => (env, right._2, expr, BoolVal(sinister.n >= dexter.n));
          case _ => throw new UndefinedSemantics(s"No semantics for ${l} >= ${r}");
        }
      }
      case Iszero(c) => {
        val valorem = eval((env, mem, c, value)); // valeorem is value in Latin
        valorem._4 match {
          case (resulten: IntVal) => (env, valorem._2, expr, BoolVal(resulten.n == 0));
          case _ => throw new UndefinedSemantics(s"Type error: ${c}");
        }
      }
      case Ite(c, t, f) => {
        val conditione = eval((env, mem, c, value)); // conditione is condition in Latin
        val verum = eval((env, conditione._2, t, value)); // verum is true in Latin
        val falsus = eval((env, verum._2, f, value)); // falsus is false in Latin
        conditione._4 match {
          case (condition: BoolVal) => if (condition.b) verum; else falsus;
          case _ => throw new UndefinedSemantics(s"Type error: ${c}");
        }
      }
      case ValExpr(name, value_, body) => 
        val valorem = eval((env, mem, value_, value));
        val new_env = env + (name -> valorem._4);
        eval((new_env, valorem._2, body, value));
      case VarExpr(name, value_, body) => {
        val valorem = eval(env, mem, value_, value);
        if (mem.m.contains(mem.top + 1)) throw new UndefinedSemantics(s"memory already occupied at ${LocVal(mem.top + 1)}");
        val new_env = env + (name -> LocVal(mem.top + 1));
        val new_mem = Mem(mem.m + (mem.top + 1 -> valorem._4), mem.top + 1);
        eval(new_env, new_mem, body, value);
      }
      case Proc(v, expr_) => {
        (env, mem, expr, ProcVal(v, expr, env));
      }
      case DefExpr(fname, aname, fbody, ibody) => {
        val new_env = env + (fname -> RecProcVal(fname, aname, fbody, env));
        eval(new_env, mem, ibody, value);
      }
      // case Asn(v, e) => {
      //   val new_mem = env(v) match {
      //     case LocVal(l) => {
      //       Mem(mem.m + (l -> doInterpret(env, mem, e)), mem.top + 1);
      //     }
      //     case _ => throw new UndefinedSemantics(s"No semantics for ${Asn(v, e)}");
      //   }
      //   doInterpret(env, new_mem, v);
      // }
      case Paren(expr_) => eval((env, mem, expr_, value));
      case Block(f, s) => {
        val valorem = eval((env, mem, f, value));
        eval(env, valorem._2, s, value);
      }
      case PCall(ftn, arg) => {
        val valorem = eval((env, mem, ftn, value));
        valorem._4 match {
          case (func: ProcVal) => {
            val resulten = eval((env, valorem._2, arg, value));
            val new_env = func.env + (func.v -> resulten._4);
            eval((new_env, resulten._2, func.expr, value));
          }
          case (func: RecProcVal) => {
            val resulten = eval((env, valorem._2, arg, value));
            val new_env = func.env + (func.av -> resulten._4) + (func.fv -> valorem._4);
            eval(new_env, resulten._2, func.body, value);
          }
          case _ => throw new UndefinedSemantics(s"Type error: ${ftn}");
        }
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