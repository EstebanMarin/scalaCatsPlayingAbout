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

  extension [A](list: List[A])
    def combineAll(using Combinator[A]): A =
      list.reduce(summon[Combinator[A]].combine)

  given Combinator[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  given [A: Combinator]: Combinator[Option[A]] with
    override def combine(x: Option[A], y: Option[A]): Option[A] = ???

  case class Person(name: String):
    def greet: String = s"Hi, my $name, nice to meet you"

  extension (string: String) def greetWithPerson: String = Person(string).greet

  val listIntTest = List(1, 2, 3, 4, 5, 6)
  val listStringtes = "testString".toList

  println("hello world")
  println(test.toString())
  println(s"${"Esteban".greetWithPerson}")
  println(listIntTest.combineAll)

  println("─" * 100)
