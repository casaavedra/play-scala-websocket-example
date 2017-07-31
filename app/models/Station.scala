package models

abstract class Station(val Name: String) {
  var InitTravelPassengers: List[Passenger]
  var EndTravelPassengers: List[Passenger]

  override def toString: String = s"$Name: Total ${InitTravelPassengers.length+EndTravelPassengers.length} passengers. In travel: ${InitTravelPassengers.length}. Ending travel: ${EndTravelPassengers.length}"

  def GetInPassengers(passengersIn: List[Passenger]): Int = {
    EndTravelPassengers = EndTravelPassengers:::passengersIn
    EndTravelPassengers.length
  }

  def GetOffPassengers(passengersIn: List[Passenger]): Int ={
    //Se remueven los pasajeros de la lista de pasajeros de la estacion
    this.InitTravelPassengers = InitTravelPassengers.filterNot(p => passengersIn.contains(p))
    InitTravelPassengers.length
  }

  def GetOffPassengers(nameStation: String): List[Passenger] ={
    //Se filtran los pasajeros que tienen como Destino la estación actual
    val getOffPassengers: List[Passenger] = InitTravelPassengers.filter(p => p.Destination equals nameStation)
    //Se remueven los pasajeros de la lista de pasajeros del carro
    this.InitTravelPassengers = InitTravelPassengers.filterNot(p => getOffPassengers.contains(p))

    getOffPassengers
  }
}

case class MainHubSt(override val Name: String) extends Station(Name) {
  override var InitTravelPassengers: List[Passenger] = Nil
  override var EndTravelPassengers: List[Passenger] = Nil
}

case class IntermediateSt(override val Name: String) extends Station(Name){
  override var InitTravelPassengers: List[Passenger] = Nil
  override var EndTravelPassengers: List[Passenger] = Nil
}
