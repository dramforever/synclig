package synclig.types

case class StateId(unique: Unique = Unique.gen):
  override def toString: String = s"[${unique}]"

case class Name(name: String, unique: Unique = Unique.gen):
  override def toString: String = s"_${name}${unique}"
