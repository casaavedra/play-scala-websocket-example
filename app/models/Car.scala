package models

import java.util.Date

import scala.collection.mutable

abstract class Car (val Id: Int, val Departure: MainHubSt, val DepartureTime: Date, val Destination: MainHubSt) /*extends Ordered[T]*/{

  val Capacity: Int
  val UniquePath: String= s"$Id${Destination.Name}"
  var CurrentStation: String
  var Passengers: List[Passenger]
  var Stations: mutable.SortedMap[String, Date] = GetScheduleStations

  override def toString: String = s"Metro Id: $Id $CurrentStation ${Passengers.length} passengers"

  def CurrentPassengers(): Int = Passengers.length

  def SetCurrentStation(time: Date): Unit = {
    var x = Stations.filter(s => s._2.compareTo(time)<=0)
    CurrentStation = Stations.filter(s => s._2.compareTo(time)<=0).head._1
  }

  def GetOffPassengers(stName: String): List[Passenger] = {
    //Se filtran los pasajeros que tienen como Destino la estación actual
    val getOffPassengers = Passengers.filter(p => p.Destination equals stName)
    //Se remueven los pasajeros de la lista de pasajeros del carro
    this.Passengers = Passengers.filterNot(p => getOffPassengers.contains(p))

    getOffPassengers
  }

  def GetInPassengers(passengersIn: List[Passenger]): List[Passenger] = {
    //Calcula si pueden ingresar todos los pasajeros de acuerdo a la capacidad del carro
    //si no caben todos, se ingresa la cantidad para completar el cupo del carro
    if(Passengers.length + passengersIn.length <= Capacity) {
      Passengers = Passengers ::: passengersIn
    }
    else {
      this.Passengers = Passengers ::: passengersIn.take(Capacity - CurrentPassengers - passengersIn.length)
    }

    //Se envía la lista de pasajeros que no ingresaron
    passengersIn.filterNot(p => Passengers.contains(p))
  }

  def GetScheduleStations(): mutable.SortedMap[String, Date] = {

    val path1 = List("Portal Americas", "Calle 42 sur", "Carrera 80", "Kennedy", "Avenida Boyaca", "Carrera 68", "Carrera 50", "NQS", "Narino", "Calle 1")
    val path2 = List("Calle 1", "Calle 10", "Calle 26", "Calle 45", "Calle 63", "Calle 72")

    var timesPath1 = List.tabulate(path1.length-1)(_ => new Date())
    var timesPath2 = List.tabulate(path2.length-1)(_ => new Date())

    (Departure.Name, Destination.Name) match {
      case ("Portal Americas", "Calle 1") =>
        timesPath1 = DepartureTime::timesPath1
        mutable.SortedMap((path1 zip timesPath1) : _*)
      case ("Portal Americas", "Calle 72") =>
        timesPath1 = DepartureTime::timesPath1
        timesPath2 = (new Date)::timesPath2

        mutable.SortedMap((path1:::path2.filterNot(p => path1.contains(p)) zip timesPath1:::timesPath2) : _*)
      case ("Calle 72", "Calle 1") =>
        timesPath2 = timesPath2:::List(DepartureTime)
        mutable.SortedMap((path2 zip timesPath2).reverse : _*)
      case ("Calle 72", "Portal Americas") =>
        timesPath2 = timesPath2:::List(DepartureTime)
        mutable.SortedMap((path1:::path2.filterNot(p => path1.contains(p)) zip timesPath1:::timesPath2).reverse: _*)
      case ("Calle 1", "Portal Americas") =>
        timesPath1 = timesPath1:::List(DepartureTime)
        mutable.SortedMap((path1 zip timesPath1).reverse : _*)
      case _ =>
        timesPath2 = DepartureTime::timesPath2
        mutable.SortedMap((path2 zip timesPath2) : _*)
    }
  }
}

case class BigCar(override val Id: Int, override val Departure: MainHubSt, override val DepartureTime: Date, override val Destination: MainHubSt, Capacity: Int = 1800) extends Car/*[BigCar]*/ (Id, Departure, DepartureTime, Destination) {
  override var Passengers: List[Passenger] = Nil
  override var CurrentStation: String = _
  //var Stations: mutable.SortedMap[String, Date] = GetScheduleStations

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(st: String) = super.GetOffPassengers(st)
}

case class ShortCar(override val Id: Int, override val Departure: MainHubSt, override val DepartureTime: Date, override val Destination: MainHubSt, Capacity: Int = 900) extends Car/*[ShortCar]*/ (Id, Departure, DepartureTime, Destination) {
  override var Passengers: List[Passenger] = Nil
  override var CurrentStation: String = _
  //var Stations: mutable.SortedMap[String, Date] = GetScheduleStations

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(st: String) = super.GetOffPassengers(st)
}
