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

      "Eq works on two items" in {
        Lisp.eval(List(Eq(List('a, 'b)))) mustEqual false
        Lisp.eval(List(Eq(List('a, 'a)))) mustEqual true
      }

      "Eq works on lists of items" in {
        Lisp.eval(List(Eq(List('a, 'a, 'a, 'a)))) mustEqual true
        Lisp.eval(List(Eq(List('a, 'a, 'a, 'b)))) mustEqual false
      }

      "Eq fails if only given one item" in {
        try {
          Lisp.eval(List(Eq(List('a)))) mustEqual false
          fail
        } catch {
          case e: Exception => {
            true mustEqual true
          }
        }
      }

      "Atom succeeds if given an Atom, fails otherwise" in {
        Lisp.eval(List(Atom('a))) mustEqual true
        Lisp.eval(List(Atom(Nil))) mustEqual false
      }
    }
  }
}
