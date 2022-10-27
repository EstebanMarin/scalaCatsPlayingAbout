package com.estebanmarin
package scalacats

import cats.kernel.Order

@main def Main(args: String*): Unit =
  println("─" * 100)

  given optionOrdering[A: Ordering]: Ordering[Option[A]] with
    override def compare(x: Option[A], y: Option[A]): Int = (x, y) match
      case (None, None) => 0
      case (_, None) => -1
      case (None, _) => 1
      case (Some(x), Some(y)) => summon[Ordering[A]].compare(x, y)

  val test = List(Some(6), Some(4), None, Some(2)).sorted

  trait Combinator[A]:
    def combine(x: A, y: A): A

  extension [A](list: List[Option[A]])
    def combineOptionAll(using Combinator[A]) =
      list.reduce((a, b) =>
        (a, b) match
          case (None, None) => None
          case (_, None) => a
          case (None, _) => b
          case (Some(x), Some(y)) => Some(summon[Combinator[A]].combine(x, y))
      )

  extension [A](list: List[A])
    def combineAll(using Combinator[A]): A =
      list.reduce(summon[Combinator[A]].combine)

  given Combinator[Char] with
    override def combine(x: Char, y: Char): Char = x

  given Combinator[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  case class Person(name: String):
    def greet: String = s"Hi, my $name, nice to meet you"

  extension (string: String) def greetWithPerson: String = Person(string).greet

  val listIntTest = List(1, 2, 3, 4, 5, 6)
  val listStringtes = "testString".toList

  val testListOption = List(Some(4), Some(4), Some(6))

  extension (n: Int) def isPrime = true

  val testSecondExtession: Boolean = 7.isPrime

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  extension [A](tree: Tree[A]) 
    def map[B](f: A => B): Tree[B] = 
      tree match
        case Leaf(value) => ???
        case Branch(left, right) => ???
      

  println("hello world")
  println(test.toString())
  println(s"${"Esteban".greetWithPerson}")
  println(testListOption.combineOptionAll)
  println(listStringtes.combineAll)

  println("─" * 100)
