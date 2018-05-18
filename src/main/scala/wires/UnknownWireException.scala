package wires

case class UnknownWireException(wireName: String) extends RuntimeException(wireName) 
