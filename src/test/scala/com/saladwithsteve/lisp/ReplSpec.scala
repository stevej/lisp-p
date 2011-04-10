package com.saladwithsteve.lisp

import org.specs.Specification
import java.io.StringReader

object ReplSpec extends Specification {
  "Repl" should {
    "not be null" in {
      val program = new StringReader("(define (add) (+ 1 1))")
      val repl = new Repl(program)
      //repl()
      repl mustNot beNull
    }
  }
}
