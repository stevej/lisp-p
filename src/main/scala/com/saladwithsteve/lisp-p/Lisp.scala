package com.saladwithsteve.lisp

// lisp-p? => Lisp in Scala, Pretty Please?

import scala.collection.mutable._

object Lisp {
  def bareEnv(): Map[String, Any] = new HashMap()

  def car[T](xs: List[T]): T = xs.head
  def cdr[T](xs: List[T]): List[T] = xs.tail
  def cadr[T](xs: List[T]): T = car(cdr(xs))
  def caddr[T](xs: List[T]): T = car(cdr(cdr(xs)))
  def cadddr[T](xs: List[T]): T = car(cdr(cdr(xs)))

  /**
   * applies the function to it's arguments.
   */
  def apply[T](fn: T, x: List[Any], env: Map[String, Any]) = null

  /**
   * If we're just beginning to eval, then create a new environment.
   */
  def eval(expr: Any): Any = eval(expr, bareEnv())

  /**
   * evals the expression against the current environment.
   */
  def eval(expr: Any, env: Map[String, Any]): Any = expr match {
    case e: List[_] => {println(e); car(e) match {
      case Atom => cadr(e) match {
        case _: List[_] => false
        case _ => true
      }
      case i: Int => i
      case s: String => s
      case b: Boolean => b
      // Special Forms
      case Car(list: List[_]) => car(list)
      case _: Cdr => cdr(e)
      case Eq(list: List[Any]) if list.size > 1 => list.filter((a: Any) => a != list.head).isEmpty
        // Implement basic conditional logic flow: (if (cond) (body))
        case _: If => if (eval(cadr(e), env).asInstanceOf[Boolean]) {
          eval(caddr(e), env)
        } else {
          eval (cadddr(e), env)
        }
      // When we see a Lambda, add the function to our environment.
      // (make-function (cadr e) (cddr e) env)
      // structure: (lambda (x y) (body))
      // example: (eq 3 (lambda (x y) (+ x y)) (1 2))
      case _: Lambda => error("not implemented")
      // (update! (cadr e) env (eval (caddr e) env))
      case _: SetBang => error("not implemented")
      case Quote(list: List[_]) => list
      case Cons(a: Any, xs: List[Any]) => a :: xs
      case _ => error("unknown expression: " + expr)
    }}
    case _ => error("unknown expr: " + expr)
  }
}

/*
object LispX {
  // How to represent Assoc Lists
  List[(T, U)]
  // How to traverse Cons cells efficiently?
  def atom_?[T](other: T) = other match {
    case Cons(_, _) => false
    case _ => true
  }

  val env: Map[String, Any] = HashMap[String, Any]()

  def cons[T](head: T, tail: Cons[T]) = Cons[T](head, tail)

  def car[T](cons: Cons[T]): T = cons.head

  def cdr[T](cons: Cons[T]): Cons[T] = cons.tail

  def caar[T](t: Cons[T]): T = car(t) match {
    case c: Cons[T] => car(c)
    case x@_ => error("Cons cell expected. received: " + x)
  }

  def cadr[T](t: Cons[T]): T = car(cdr(t))

  def caddr[T](t: Cons[T]): T = car(cdr(cdr(t)))

  def cdar[T](t: Cons[T]): Cons[T] = car(t) match {
    case c: Cons[T] => cdr(c)
    case x@_ => error("Cons cell expected. received: " + x)
  }

  def cadar[T](t: Cons[T]): T = car(t) match {
    case c: Cons[T] => car(cdr(c))
    case x@_ => error("Cons cell expected. received: " + x)
  }
  */
  /**
   * @param a is an assoc-list structured like so:
   * ((k1 . v1)
   *  (k2 . v2)
   *  (k3 . v3))
   */
  /*
  def parlis[T](vars: Cons[T], vals: Cons[T], a: Cons[Cons[T]]): Cons[Cons[T]] = {
    if (vars == EmptyList || vals == EmptyList) {
      a
    } else {
      parlis(vars.tail, vals.tail, Cons(Cons(vars.head, vars.tail), a))
    }
  }

  def evcon[T](c: T, a: Cons[Cons[T]]): Cons[T] = c match {
    case c: Cons[Cons[T]] => if (eval(caar(c), a) != EmptyList) {
      eval(cadar(c), a)
    } else {
      EmptyList
    }
    case c: Cons[T] => evcon(cdr(c), a)
  }

  def evlis[T](m: Cons[T], a: T) = m match {
    case EmptyList => EmptyList
    case _ => cons(eval(car(m), a), evlis(cdr(m), a))
  }


  def apply[T](fn: T, x: T, a: Cons[Cons[T]]) = {
    if (atom_?(fn)) {
      (fn, x) match {
        case (Car, Cons) => caar(x)
        case (Cdr, Cons) => cdar(x)
        case (Cons, Cons) => cons(car(x), cadr(x))
        case (Atom, Cons) => atom_?(car(x))
        case (Eq, Cons) => car(x) == cadr(x)
        case _ => apply(eval(fn, a), x, a)
      }
    } else if(car(fn) == Lambda) {
      eval(caddr(fn), pairlis(cadr(fn), x, a))
    } else if(car(fn) == Label) {
      apply(caddr(fn), x, cons(cons(cadr(fn), caddr(fn)), a))
    }
  }

  def eval[T](e: Cons[T], a: Cons[Cons[T]]): Cons[T] = {
    if (atom_?(e)) {
      cdr(assoc(e, a))
    } else if (atom_?(car(e))) {
      car(e) match {
        case Quote => cadr(e)
        case Cond => evcon(cdr(e), a)
        case _ => apply(car(e), evlis(cdr(e), a), a)
      }
    } else {
      apply(car(e), evlis(cdr(e), a), a)
    }
  }
  */

  /**
   * If a is an assoc-list, then assoc returns the first pair that starts with x.
   */
  /*
  def assoc[T](x: T, a: Cons[Cons[T]]): Cons[T] = e match {
    case EmptyList => EmptyList
    case _ => if (x == caar(a)) {
      car(a)
    } else {
      assoc(x, cdr(a))
    }
  }
}*/

// Lisp Built-ins
case class Atom()
case class Car(xs: List[_])
case class Cdr()
case class Cond()
case class Cons(x: Any, xs: List[Any])
case class Eq(xs: List[_])
case class If()
case class Label()
case class Lambda()
case class Quote(xs: List[_])
// set!
case class SetBang()
