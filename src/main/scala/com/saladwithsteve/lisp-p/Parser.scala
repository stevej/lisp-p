package com.saladwithsteve.lisp

trait Expr
case class Define(val name: Name, val lambda: Lambda) extends Expr
case class Lambda(val exprs: Expr*) extends Expr
case class Function(val name: String) extends Expr
case class Number(val m: Int) extends Expr
case class Name(val value: String) extends Expr

class Parser {
  def parse(tokens: Stream[Token]): Expr = {
    new Expr {}
  }
}
