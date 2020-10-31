package synclig.builder

import synclig.types.Expr.VarReg
import synclig.types._

sealed trait Flow:
  def ++(other: Flow): Flow
  def merge(other: Flow): Flow

  def apply(sl: Slice): Flow.Through

object Flow:
  case class Through(pass: Slice, store: Slice):
    def merge(other: Through) = Through(
      pass = pass merge other.pass,
      store = store merge other.store
    )

  case class Split(mid: Slice, in: Slice => Slice, out: Slice) extends Flow:
    override def ++(other: Flow): Flow = other match
      case Split(mid1, in1, out1) => Split(
        mid = mid merge mid1 merge in1(out),
        in = in,
        out = out1
      )

      case Joined(through1) => through1(out) match
        case Through(pass1, store1) => Split(
          mid = mid merge store1, in = in, out = pass1
        )

    override def merge(other: Flow): Flow = other match
      case Split(mid1, in1, out1) => Split(
        mid = mid merge mid1,
        in = sl => in(sl) merge in1(sl),
        out = out merge out1
      )
      case Joined(_) => Joined(sl => apply(sl) merge other(sl))

    override def apply(sl: Slice): Through = Through(
      pass = out,
      store = in(sl) merge mid,
    )

  case class Joined(through: Slice => Through) extends Flow:
    override def ++(other: Flow): Flow = other match
      case Split(mid1, in1, out1) => Split(
        mid = mid1,
        in = (sl: Slice) => through(sl) match {
          case Through(pass1, store1) => store1 merge in1(pass1)
        },
        out = out1
      )

      case Joined(through1) => Joined(
        through(_) match {
          case Through(pass1, store1) => through1(pass1) match {
            case Through(pass2, store2) => Through(
              pass = pass2,
              store = store1 merge store2
            )
          }
        })

      override def merge(other: Flow): Flow =
        Joined(sl => through(sl) merge other(sl))

      override def apply(sl: Slice): Through = through(sl)

  import synclig.types.Expr

  def cond(expr: Expr, value: Boolean): Flow = Joined(sl =>
    Through(
      pass = sl.cond(expr, value),
      store = Slice.Empty
    ))

  def start(out: Slice): Flow = Split(
    mid = Slice.Empty,
    in = _ => Slice.Empty,
    out = out
  )

  def assign(name: Name, value: Expr): Flow = Joined(sl =>
    Through(sl.assign(name, value), Slice.Empty))

  val empty: Flow = Joined(sl =>
    Through(
      pass = sl,
      store = Slice.Empty
    )
  )
