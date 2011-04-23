package com.saladwithsteve.lisp

import java.io.{File, InputStream, InputStreamReader, PushbackReader, Reader}


sealed trait Token
case object Space extends Token
case object LParen extends Token
case object RParen extends Token
case class Word(val name: String) extends Token

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

  def punctuation(c: Char): Boolean = c match {
    case '\n' => true
    case '\t' => true
    case ' '  => true
    case '('  => true
    case ')'  => true
    case _    => false
  }

  def makePunctuation(c: Char): Token = c match {
    case '\n' => Space
    case '\t' => Space
    case ' '  => Space
    case '('  => LParen
    case ')'  => RParen
  }

  /**
   * Returns the next Word and the remaining Stream
   */
  def readWord(input: Stream[Char]): (Word, Stream[Char]) = {
    val punc = { c: Char => !punctuation(c) }
    // It seems silly that I have to walk over the stream twice.
    // FIXME: switch this to an Iteratee.
    val name = (input takeWhile punc).mkString
    val tail = input dropWhile punc

    (Word(name), tail)
  }

  implicit def charStreamToTokenStream(input: Stream[Char]): Stream[Token] = {
    val maybeFirst = input.headOption

    val tokenStream: Option[Stream[Token]] = maybeFirst.map { char =>
      char match {
        case s if !punctuation(s) => {
          val (word, stream) = readWord(input)

          Stream.cons(word, charStreamToTokenStream(stream))
        }
        case s => Stream.cons(makePunctuation(s), charStreamToTokenStream(input.tail))
      }
    }

    // FIXME: this filter is wasteful
    tokenStream.getOrElse(Stream.empty[Token]).filter(_ != Space)
  }
}
