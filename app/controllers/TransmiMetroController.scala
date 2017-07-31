package controllers

import java.io.File
import java.util.Date
import java.util.Calendar

import models._

import scala.io._

object CSVDemo extends App {

  var tmc = new TransmiMetroController
  var cars = tmc.loadRoutes
  var stations: List[Station] = tmc.loadPassengersXStation

  var time = tmc.splitTimeString("0400")

  while (time.before(tmc.splitTimeString("2359"))) {
    // Proceso para subida de pasajeros en el Metro
    for (car <- cars.filter(c => c.Stations.exists(st => st._2 equals time));// Se filtran los carros que tienen hora de recogida de "time"
         st <- stations.filter(s => car.Stations.contains(s.Name));// Se seleccionan las estaciones disponibles desde el itinerario del carro
         passAtTime <- st.InitTravelPassengers.filter(ps => ps.TimeOfEntrance.compareTo(time)<=0))// Se seleccionan los pasajeros q tienen en su destino alguna estacion de la ruta del carro
    {
      car.GetInPassengers(List(passAtTime))// Se cargan los pasajeros en el carro
      st.GetOffPassengers(List(passAtTime))// Se descargan los pasajeros en la estacion
    }

    // Proceso para bajada depasajeros del Metro
    for(car <- cars.filter(c => c.Stations.exists(st => st._2 equals time));
        pass <- car.GetOffPassengers(car.CurrentStation))
    {
      stations.filter(s=> s.Name equals car.CurrentStation).head.GetInPassengers(List(pass))
    }

    cars.filter(c => c.Stations.exists(st => st._2 equals time)).foreach(println)
    stations.foreach(println)

    Thread.sleep(50)

    val cal = Calendar.getInstance
    cal.setTime(time)
    cal.add(Calendar.MINUTE, 1)
    time = cal.getTime
    cars.filter(c => c.Stations.exists(st => st._2 equals time)).map(c => c.SetCurrentStation(time))
  }

}

class TransmiMetroController {

  var countBigCars = 0

  def loadRoutes: List[Car/*[BigCar]*/] = {
    val bufferedSource = Source.fromFile("C:\\ScalaProjects\\play-scala-websocket-example\\app\\controllers\\routes.csv")
    var carList: List[Car]=Nil

    println("Train Id , Departure station , Departure time , Destination")
    for (line <- bufferedSource.getLines.drop(1)) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")

      val car: Car = stringArrayToCarRoutes(cols)
      cols(1) match {
        case "Portal Americas" | "Calle 1" | "Calle 72" =>
          if(cols(1) equals cols(3))
            carList.filter(_.UniquePath equals car.UniquePath ).head.Stations.update(cols(1), car.DepartureTime)
          else {
            car.SetCurrentStation(car.DepartureTime)
            carList = carList:::List(car)
          }
        case x =>
          carList.filter(_.UniquePath equals car.UniquePath ).head.Stations.update(x, car.DepartureTime)
      }

      if(!carList.exists(_.UniquePath equals car.UniquePath))
        carList = carList:::List(car)
    }
    bufferedSource.close

    carList
  }

  def loadPassengersXStation: List[Station]={

    var passengers: List[Passenger]=Nil

    println("Id, Time , Destination")
    for (file <- new File("C:\\ScalaProjects\\play-scala-websocket-example\\app\\controllers\\Pasajeros").listFiles){
      val bufferedSource = Source.fromFile(file.getAbsolutePath)

      for (line <- bufferedSource.getLines.drop(1)) {
        val cols = line.split(",").map(_.trim)
        // do whatever you want with the columns here
        println(s"${cols(0)}|${cols(1)}|${cols(2)}")
        val nameEntrance = file.getName.split('.')(0)

        passengers = passengers:::List(Passenger(cols(0).toInt, splitTimeString(cols(1)), nameEntrance, cols(2)))

        /*nameEntrance match {
          case "Portal Americas" | "Calle 1" | "Calle 72" =>
            passengers = passengers:::List(Passenger(cols(0).toInt, splitTimeString(cols(1)), nameEntrance, cols(2) match {
              case "Portal Americas" | "Calle 1" | "Calle 72" => MainHubSt(cols(2))
              case _ => IntermediateSt(cols(2))
            }))
          case x =>
            passengers = passengers:::List(Passenger(cols(0).toInt, splitTimeString(cols(1)), IntermediateSt(nameEntrance), cols(2) match {
              case "Portal Americas" | "Calle 1" | "Calle 72" => MainHubSt(cols(2))
              case _ => IntermediateSt(cols(2))
            }))
        }*/
      }
    }

    var stations: List[Station]=Nil

    val x = passengers.groupBy(p => p.Entrance)

    x.foreach(xx => {
      xx._1 match {
        case "Portal Americas" | "Calle 1" | "Calle 72" =>
          stations = MainHubSt(xx._1)::stations
        case _ => stations = IntermediateSt(xx._1)::stations
      }

      stations.head.InitTravelPassengers = xx._2
    })

    /*for(y <- passengers.groupBy(p => p.Entrance)){
      val st = y._1
      st.InitTravelPassengers = y._2
      stations = stations:::List(st)
    }*/

    stations
  }

  // string array to FlightEvent
  def stringArrayToCarRoutes(cols: Array[String]): Car = {
    if (countBigCars < 12) {
      countBigCars += 1
      BigCar(cols(0).toInt, MainHubSt(cols(1)), splitTimeString(cols(2)), MainHubSt(cols(3)))
    }
    else
      ShortCar(cols(0).toInt, MainHubSt(cols(1)), splitTimeString(cols(2)), MainHubSt(cols(3)))
  }

  def splitTimeString(strTime: String): Date = {
    import java.text.SimpleDateFormat
    val f = new SimpleDateFormat("kkmm")
    var date = new Date()
    date = f.parse(strTime)
    date
  }
}
