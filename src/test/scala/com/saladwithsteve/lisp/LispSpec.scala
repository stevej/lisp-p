/** Copyright 2008 Steve Jenson, Inc. */
package com.saladwithsteve.lisp

import scala.util.Sorting
import java.io.{File, FileInputStream}
import net.lag.configgy.Config
import org.specs._

object LispSpec extends Specification {

  private def sorted[T <% Ordered[T]](list: List[T]): List[T] = {
    val dest = list.toArray
    Sorting.quickSort(dest)
    dest.toList
  }

  "Lisp" should {
    doAfter {
    }

    "Simple unsugared syntax" in {
      "Car should return first element" in {
        Lisp.eval(List(Car(List('a, 'b)))) mustEqual 'a
      }
      
      "Quote returns a list" in {
        Lisp.eval(List(Quote(List('a, 'b)))) mustEqual List('a, 'b)
      }
      
      "Consing onto a List returns a new List with all the elements" in {
        Lisp.eval(List(Cons('a, List('b, 'c')))) mustEqual List('a, 'b, 'c')
      }
    }
  }
}
