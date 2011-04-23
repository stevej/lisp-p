package com.saladwithsteve.lisp

import org.specs.Specification

import java.io.StringReader

object TokenizerSpec extends Specification {

  "Tokenizer" should {
    val tokenizer = new Tokenizer
    "readWord" in {
      val inputStream = "abc)".toStream
      val (word, input) = tokenizer.readWord(inputStream)

      word mustEqual Word("abc".mkString)

      input.head mustEqual ')'
    }

    "charStreamToTokenStream" in {
      "can convert non-nested structures" in {
        import tokenizer.charStreamToTokenStream
        val charStream = "(set foo 1)".toStream
        val tokenStream: Stream[Token] = charStream

        val stream = tokenStream.force
        //stream foreach println
        stream mustEqual List(LParen, Word("set"), Word("foo"), Word("1"), RParen)
      }

      "can traverse nested structures" in {
        import tokenizer.charStreamToTokenStream
        val charStream = "(define (add) (+ 1 1))".toStream
        val tokenStream: Stream[Token] = charStream

        val stream = tokenStream.force
        stream mustEqual List(LParen,
                                Word("define"),
                                LParen, Word("add"), RParen,
                                LParen, Word("+"), Word("1"), Word("1"), RParen,
                              RParen)
      }
    }
  }
}
