package com.estebanmarin.scalacats

import cats.*
import cats.implicits.given
import cats.syntax.*
import cats.data.OptionT

val listOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
val charOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b')))
val listOfTuples: OptionT[List, (Int, Char)] =
  for
    char <- charOptions
    int <- listOptions
  yield (int, char)

import cats.data.EitherT

val listOfEither: EitherT[List, String, Int] =
  EitherT(List(Left("Something w"), Right(43), Right(2)))

@main def more(args: String*): Unit =
  println(listOfTuples.value)
