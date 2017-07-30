package controllers

import models._
import scala.io.Source

object CSVDemo extends App {
  println("Train Id , Departure station , Departure time , Destination")

  var tmc = new TransmiMetroController
  var cars = tmc.loadRoutesAndPassengers
}

class TransmiMetroController {

  def loadRoutesAndPassengers: List[Car] = {
    val bufferedSource = Source.fromFile("routes.csv")
    var carList=Nil
    for (line <- bufferedSource.getLines.drop(1)) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
      var car = stringArrayToCarRoutes(cols)

    }
    bufferedSource.close

    return carList
  }

  // string array to FlightEvent
  def stringArrayToCarRoutes(cols: Array[String]) = new BigCar(cols(0).toInt, cols(1), cols(2), cols(3))
}
