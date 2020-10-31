package synclig.types

case class Unique private(id: Long):
  override def toString: String = s"#${id.toString}"

object Unique:
  private var current: Long = 0

  def gen: Unique =
    val res = current
    current += 1
    Unique(res)

  private[Unique] def apply(id: Long) = new Unique(id)
