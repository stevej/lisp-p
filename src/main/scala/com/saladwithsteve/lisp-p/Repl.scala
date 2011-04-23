package com.saladwithsteve.lisp

import java.io.{File, InputStream, InputStreamReader, PushbackReader, Reader}

class InteractiveReader(prompt: String, underlying: Reader) extends PushbackReader(underlying) {
  override def read() = {
    val char = underlying.read()

    if (char == -1) {
      // print a prompt, wait for input
      // FIXME: abstract this into an OutputStream
      System.out.println(prompt)

      while (!underlying.ready()) {
        Thread.sleep(100)
      }

      read()
    } else {
      char
    }

  }
}


/**
 * Still a work in progress.
 */
class Repl(r: Reader) {
  val PROMPT = "> "
  // FIXME: pass in the shared PROMPT from our alternate constructor
  def this(stream: InputStream) =
    this(new InteractiveReader("> ", new InputStreamReader(stream)))

  /**
   * Default constructor builds a Repl around STDIN
   */
  def this() = this(System.in)

  def apply() {
    val tokenizer = new Tokenizer()
    import tokenizer._

    System.out.println(PROMPT)

    val charStream: Stream[Char] = r
    val tokenStream: Stream[Token] = charStream

    tokenStream foreach { println _ }
  }
}
