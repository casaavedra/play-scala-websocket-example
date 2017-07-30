package models

import java.time._

abstract class Passenger {
  var Id:Int
  var TimeOfEntrance: LocalDateTime
  var Destination: Station

}

object Passenger {

}
