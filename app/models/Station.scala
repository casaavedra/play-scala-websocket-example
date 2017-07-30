package models

abstract class Station(var Name: String) {
  var CurrentPassengers: Int
  var Passengers: List[Passenger]

  def GetInPassengers(passengersIn: List[Passenger]): Int = {
    Passengers = Passengers:::passengersIn
    Passengers.length
  }

  def GetOffPassengers(nameStation: String): List[Passenger] ={
    //Se filtran los pasajeros que tienen como Destino la estación actual
    val getOffPassengers = Passengers.filter(p => p.Destination.Name equals(nameStation))
    //Se remueven los pasajeros de la lista de pasajeros del carro
    Passengers = Passengers.filterNot(p => getOffPassengers.contains(p))

    return getOffPassengers
  }
}

class MainHubSt(Name: String) extends Station(Name){
  override var CurrentPassengers: Int = _
  override var Passengers: List[Passenger] = _

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(nameStation: String): List[Passenger] = super.GetOffPassengers(nameStation)
}

class IntermediateSt(Name: String) extends Station(Name){
  override var CurrentPassengers: Int = _
  override var Passengers: List[Passenger] = _

  override def GetInPassengers(passengersIn: List[Passenger]) = super.GetInPassengers(passengersIn)

  override def GetOffPassengers(nameStation: String): List[Passenger] = super.GetOffPassengers(nameStation)
}
