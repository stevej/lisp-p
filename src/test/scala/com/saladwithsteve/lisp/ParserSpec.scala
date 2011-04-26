package com.saladwithsteve.lisp

import org.specs.Specification
import org.specs.mock._

object ParserSpec extends Specification with Mockito {
  "Parser" should {
    val parser = new Parser()

    val simple_add = List(LParen, Word("add"), Number(1), Number(1), RParen)

    val define_add = List(LParen,
                         Word("define"),
                         LParen, Word("add"), RParen,
                         LParen, Word("+"), Word("1"), Word("1"), RParen,
                       RParen).toStream

    "Turn simple expression in Expr Tree" in {
      //parser.parse(simple_add) mustEqual Function
    }

    "Understand define" in {
      skip("no defines yet")
      parser.parse(define_add) mustEqual Define(Name("add"),
                                             Lambda(
                                               Function("+"),
                                               Number(1),
                                               Number(1)))
    }
  }
}
