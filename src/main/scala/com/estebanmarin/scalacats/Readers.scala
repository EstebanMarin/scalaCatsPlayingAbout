package com.estebanmarin.scalacats

import cats.*
import cats.implicits.given
import cats.syntax.*
import cats.data.WriterT

case class Configuration(
    dbUserName: String,
    dbPassword: String,
    host: String,
    port: Int,
    nThreads: Int,
  )

case class DbConnection(username: String, Password: String):
  def getOrderStatus(order: Long): String = "dispatched"

case class HttpService(host: String, port: Int):
  def start(): Unit = println("Started")

val configuration = Configuration("esteban", "esteban2", "localhost", 1234, 8)

import cats.data.Reader

val dBReader: Reader[Configuration, DbConnection] =
  Reader((conf: Configuration) => DbConnection(conf.dbUserName, conf.dbPassword))

val dbConnection: DbConnection = dBReader.run(configuration)

val orderStatus: Reader[Configuration, String] = dBReader.map(_.getOrderStatus(55))
val estabanORder: String = orderStatus.run(configuration)

import cats.data.Writer

val aWriter: Writer[List[String], Int] = Writer(List("Started Something"), 45)
val increase = aWriter.map(_ + 1)
val left = aWriter.mapWritten(_ :+ "another")
val mapBoth = aWriter.bimap(_ :+ "bimap", _ + 1)

import cats.Semigroupal
val aTupledOption: Option[(Int, String)] = Semigroupal[Option].product(Some(1234), Some("a string"))
val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b"))

@main def more(args: String*): Unit =
  println(aTupledList)
