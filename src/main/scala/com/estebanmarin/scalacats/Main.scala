package com.estebanmarin
package scalacats

import cats.*
import cats.syntax.*
import cats.implicits.*
import cats.data.Func
import scala.util.Success
import scala.util.Try

def Main(args: String*): Unit =
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

  case class Person(name: String, age: Int):
    def greet: String = s"Hi, my $name, nice to meet you"

  extension (string: String) def greetWithPerson: String = Person(string, 39).greet

  val listIntTest = List(1, 2, 3, 4, 5, 6)
  val listStringtes = "testString".toList

  val testListOption = List(Some(4), Some(4), Some(6))

  extension (n: Int) def isPrime = true

  val testSecondExtession: Boolean = 7.isPrime

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  extension [A](tree: Tree[A])
    def map[B](f: A => B): Tree[B] = tree match
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(left.map(f), right.map(f))

    def +(that: Tree[A]) = ???

    def combineAllTree(using Combinator[A]): A = tree match
      case Leaf(value) => value
      case Branch(left, right) =>
        summon[Combinator[A]].combine(left.combineAllTree, right.combineAllTree)

  extension (tree: Tree[Int])
    def sum: Int = tree match
      case Leaf(value) => value
      case Branch(left, right) => left.sum + right.sum

  val aTree = Branch(Branch(Leaf(3), Leaf(1)), Leaf(10))

  def methodWithContextArg(nonContextArg: Int)(using nonContextArg2: String): String = ???
  val functionWithContextArgument: Int => String ?=> String = methodWithContextArg

  class Animal
  class Dog(name: String) extends Animal
  // List is Covariant List[Animal]
  val lassie = new Dog("L")
  val lassie2 = new Dog("L2")
  val lassie3 = new Dog("L3")
  val list: List[Animal] = List(lassie, lassie2, lassie3)
  // covariant
  class MyList[+A]
  val aListofAnimals: MyList[Animal] = new MyList[Dog]
  given Functor[MyList] with
    override def map[A, B](fa: MyList[A])(f: A => B): MyList[B] = ???

  trait Vet[-A]:
    def heal(animal: A): Boolean

  val myVet: Vet[Dog] = new Vet[Animal]:
    override def heal(animal: Animal): Boolean = true

  // def reducteInts[A: Semigroup](l: List[A]) = l.reduce(summon[Semigroup[A]].combine)
  def reducteInts[A: Semigroup](l: List[A]) = l.reduce(_ |+| _)

  extension [A: Monoid](l: List[A])
    // def sumAll: A = l.fold(summon[Monoid[A]].empty)(summon[Monoid[A]].combine)
    def sumAll: A = l.fold(summon[Monoid[A]].empty)(_ |+| _)

  // println(s" [Monoid] ${List(1, 2, 3, 4, 5, 6).sumAll}")

  given Monoid[Person] with
    override def combine(x: Person, y: Person): Person =
      Person(s"${x.name} + ${y.name}", x.age + y.age)
    override def empty: Person = Person("", 0)

  // println(
  //   s"Monoid person => ${List(Person("E", 12), Person("S", 23), Person("T", 76), Person("E", 36)).sumAll}"
  // )

  //  ifit produces a value than is covariant ie lists
  // if it consumes a value then is contravariant
  // othervise invant

  // println("hello world")
  // println(test.toString())
  // println(s"${"Esteban".greetWithPerson}")
  // println(testListOption.combineOptionAll)
  // println(listStringtes.combineAll)
  // println(aTree.map(_ + 1))
  // println(aTree.combineAllTree)

  type PhoneBook = Map[String, Int]

  val listPhoneBooks = List(
    Map("Alice" -> 235, "Bob" -> 64),
    Map("Camilo" -> 86, "Adriana" -> 98),
    Map("Carlos" -> 87, "Diego" -> 89),
  )

  given Monoid[PhoneBook] with
    override def combine(x: PhoneBook, y: PhoneBook): PhoneBook = x ++ y
    override def empty: PhoneBook = Map.empty

  // println(listPhoneBooks.sumAll)

  case class ShoppingCart(items: List[String], total: Double)
  given Monoid[ShoppingCart] with
    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart =
      ShoppingCart(x.items |+| y.items, y.total |+| x.total)
    override def empty: ShoppingCart = ShoppingCart(List.empty, 0)

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    shoppingCarts.sumAll

  def do10x[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  def do10Shopping[F[_]: Functor](c: F[ShoppingCart]): F[ShoppingCart] =
    c.map(s => s.copy(total = s.total * 10))

  extension [F[_]: Functor](c: F[ShoppingCart])
    def do10Times = c.map(s => s.copy(total = s.total * 10))

  // println(do10x(List(1, 2, 3)))
  val listShoppingCarts = List(ShoppingCart(List("T", "S"), 10))
  val optionShoppingCarts = Option(ShoppingCart(List("T", "S"), 10))
  val tryShoppingCarts = Try(ShoppingCart(List("T", "S"), 10))

  // println(listShoppingCarts.do10Times)
  // println(optionShoppingCarts.do10Times)
  // println(tryShoppingCarts.do10Times)

  given Functor[Tree] with
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(left.map(f), right.map(f))

  aTree.map(_ + 1)
  Functor[Tree].map(aTree)(_ + 1)


  println("─" * 100)
