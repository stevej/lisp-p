package com.saladwithsteve.lisp

import java.io.{File, InputStream, InputStreamReader, PushbackReader, Reader}


sealed trait Token
case object Space extends Token
case object LParen extends Token
case object RParen extends Token
case class Word(val name: String) extends Token

/**
 * Encodes the grammar of punctuation for Scheme.
 */
object Punctuation {
  val punc: Map[Char, Token] = Map(
    '\n' -> Space,
    '\t' -> Space,
    ' '  -> Space,
    ')'  -> RParen,
    '('  -> LParen
  )

  def contains(c: Char): Boolean = punc.contains(c)

  def apply(c: Char): Token = punc(c)
}



/**
 * Parser based on lazy Streams. Provides an implicit conversion from a Stream[Char]
 * to a Stream[Token].
 *
 * The Scheme grammar is baked in.
 */
class Tokenizer {

  /**
   * Converts a Reader of Ints into a Stream of Chars.
   */
  implicit def reader2Stream(r: { def read(): Int }): Stream[Char] = {
    val char = r.read() // FIXME: not actually a char, is an Int at this point

    if (char == -1) {
      Stream.empty
    } else {
      // FIXME: add a @tailrec
      Stream.cons(char.toChar, reader2Stream(r))
    }
  }

  /**
   * Returns the next Word and the remaining Stream
   */
  def readWord(input: Stream[Char]): (Word, Stream[Char]) = {
    val punc = { c: Char => !Punctuation.contains(c) }
    // It seems silly that I have to walk over the stream twice.
    // FIXME: switch this to an Iteratee.
    val name = (input takeWhile punc).mkString
    val tail = input dropWhile punc

    (Word(name), tail)
  }

  /**
   * TODO: make this tail recursive
   */
  implicit def charStreamToTokenStream(input: Stream[Char]): Stream[Token] = {
    val maybeFirst = input.headOption

    val tokenStream: Option[Stream[Token]] = maybeFirst.map { char =>
      char match {
        case s if !Punctuation.contains(s) =>
          readWord(input) match { case (word, stream) =>
            Stream.cons(word, charStreamToTokenStream(stream))
          }
        case s => {
          val punc = Punctuation(s)
          if (punc ne Space) {
            Stream.cons(punc, charStreamToTokenStream(input.tail))
          } else {
            charStreamToTokenStream(input.tail)
          }
        }
      }
    }

    tokenStream.getOrElse(Stream.empty[Token])
  }
}
