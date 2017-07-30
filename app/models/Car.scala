package models

import java.time._

abstract class Car (var Id: Int, var nameDepartureSt: String, var strDepartureTime: String, var nameDestinationSt: String) {

  var Capacity: Int
  var CurrentStation: Station
  var Departure: MainHubSt = new MainHubSt(nameDepartureSt)
  var Destination: MainHubSt = new MainHubSt(nameDestinationSt)
  var DepartureTime: LocalDateTime
  var Passengers: List[Passenger]
  var Stations: List[String]

  def CurrentPassengers(): Int = Passengers.length

  def GetOffPassengers(st: Station): List[Passenger] = {
    //Se filtran los pasajeros que tienen como Destino la estación actual
    val getOffPassengers = Passengers.filter(p => p.Destination equals(st))
    //Se remueven los pasajeros de la lista de pasajeros del carro
    Passengers = Passengers.filterNot(p => getOffPassengers.contains(p))

    return getOffPassengers
  }

  def GetInPassengers(passengersIn: List[Passenger]): List[Passenger] = {
    //Calcula si pueden ingresar todos los pasajeros de acuerdo a la capacidad del carro
    //si no caben todos, se ingresa la cantidad para completar el cupo del carro
    if(Passengers.length + passengersIn.length >= Capacity) {
      Passengers = Passengers ::: passengersIn
    }
    else {
      Passengers = Passengers ::: passengersIn.take(Capacity - CurrentPassengers - passengersIn.length)
    }

    //Se envía la lista de pasajeros que no ingresaron
    passengersIn.filterNot(p => Passengers.contains(p))
  }

  def GetScheduleStation(): Unit = {

    val path1 = List("Portal Americas", "Calle 42 sur", "Carrera 80", "Kennedy", "Avenida Boyaca", "Carrera 68", "Carrera 50", "NQS", "Narino", "Calle 1")
    val path2 = List("Calle 1", "Calle 10", "Calle 26", "Calle 45", "Calle 63", "Calle 72")

    (Departure.Name, Destination.Name) match {
      case ("Portal Americas", "Calle 1") => Stations = path1
      case ("Portal Americas", "Calle 72") => Stations = path1:::path2.filterNot(p => path1.contains(p))
      case ("Calle 72", "Calle 1") => Stations = path2.reverse
      case ("Calle 72", "Portal Americas") => (path1:::path2.filterNot(p => path1.contains(p))).reverse
      case ("Calle 1", "Portal Americas") => Stations = path1.reverse
      case _ => Stations = path2
    }
  }
}

class BigCar(Id: Int, nameDepartureSt: String, strDepartureTime: String, nameDestinationSt: String) extends Car (Id, nameDepartureSt, strDepartureTime, nameDestinationSt) {
  override var DepartureTime: LocalDateTime = _
  override var Passengers: List[Passenger] = _
  override var CurrentStation: Station = _
  override var Departure: MainHubSt = _
  override var Destination: MainHubSt = _
  override var Stations: List[Station] = _
  override var Capacity: Int = 1800

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(st: Station) = super.GetOffPassengers(st)
}

class ShortCar(Id: Int, nameDepartureSt: String, strDepartureTime: String, nameDestinationSt: String) extends Car(Id, nameDepartureSt, strDepartureTime, nameDestinationSt) {
  override var CurrentPassengers: Int = _
  override var DepartureTime: LocalDateTime = _
  override var Passengers: List[Passenger] = _
  override var Stations: List[Station] = _
  override var CurrentStation: Station = _
  override var Departure: MainHubSt = _
  override var Destination: MainHubSt = _
  override var Capacity: Int = 900

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(st: Station) = super.GetOffPassengers(st)
}
