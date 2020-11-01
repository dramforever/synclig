package synclig.pprint

import synclig.types._

def pprint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit =
  import Expr._

  val indent = "  " * depth
  val prettyName = paramName.fold("")(x => s"$x: ")
  val ptype = obj match { case _: Iterable[Any] => "" case obj: Product => obj.productPrefix case _ => obj.toString }

  print(f"[${System.identityHashCode(obj)}%08x] ")

  obj match
    case seq: Iterable[Any] =>
      println(s"$prettyName$ptype")
      seq.foreach:
        x =>
          print(s"${indent}  ")
          pprint(x, depth + 1)
    case (name: Name, value: Expr) =>
      print(s"${name} := ")
      pprint(value, depth + 1)
    case (cond: Expr, value: Boolean) =>
      if value then
        print(s"${indent}true  ")
        pprint(cond, depth + 1)
      else
        print(s"${indent}false ")
        pprint(cond, depth + 1)
    case Mux(branches, default) =>
      println(s"$prettyName$ptype")
      branches.foreach:
        case (cond, value) =>
          print(s"${indent}if ")
          pprint(cond, depth + 1)
          print(s"${indent}  then ")
          pprint(value, depth + 2)
      default match
        case Some(value) =>
          print(s"${indent}default ")
          pprint(value, depth + 1)
        case _ =>
          println(s"${indent}no default")
        println(s"${indent}end")
    case obj: Name => println(s"${obj.toString}")
    case obj: StateId => println(s"${obj.toString}")
    case obj: Expr => println(s"${obj.toString}")
    case obj: Product =>
      println(s"$prettyName$ptype")
      (obj.productIterator zip obj.productElementNames)
        .foreach:
          case (subObj, paramName) =>
            print(s"${indent}  ")
            pprint(subObj, depth + 1, Some(paramName))
    case _ =>
      println(s"$prettyName$ptype")
