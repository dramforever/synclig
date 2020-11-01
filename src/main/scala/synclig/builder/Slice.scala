package synclig.builder

import scala.language.implicitConversions

import synclig.types._

sealed trait Slice:
  def merge(other: Slice): Slice
  def cond(expr: Expr.WithSubst, value: Boolean): Slice
  def assign(name: Name, value: Expr.WithSubst): Slice

object Slice:
  case class NonEmpty(valid: Slice.Valid) extends Slice:
    override def merge(other: Slice) = other match
      case NonEmpty(valid1) => NonEmpty(valid merge valid1)
      case Empty => this

    override def cond(expr: Expr.WithSubst, value: Boolean): Slice =
      NonEmpty(valid.copy(cond = valid.cond + (expr(using Expr.Subst(valid.vars)) -> value)))

    override def assign(name: Name, value: Expr.WithSubst): Slice =
      NonEmpty(
        valid.copy(
          vars = valid.vars + (name -> value(using Expr.Subst(valid.vars)))
        )
      )

    override def toString: String = valid.toString

  case object Empty extends Slice:
    override def merge(other: Slice) = other
    override def cond(_expr: Expr.WithSubst, _value: Boolean): Slice = this
    override def assign(_name: Name, _value: Expr.WithSubst): Slice = this

  case class Valid(
    vars: Map[Name, Expr],
    states: Set[StateId],
    cond: Map[Expr, Boolean]):

    def merge(other: Valid): Valid =
      import Expr._

      def muxShort(mux: (Expr, Expr) => Expr)(a: Expr, b: Expr): Expr =
        if a eq b then a
        else mux(a, b)

      def muxMap[K](a: Map[K, Expr], b: Map[K, Expr])(mux: (Expr, Expr) => Expr) =
        (a.keySet intersect b.keySet).toSeq
          .map(k => k -> muxShort(mux)(a(k), b(k))).toMap

      def splitCond: Option[Expr] = (cond.keySet intersect other.cond.keySet)
        .find(k => cond(k) != other.cond(k))

      val newVars = if (states intersect other.states).isEmpty then
        muxMap(vars, other.vars)((a, b) =>
          Mux(states.toIndexedSeq.map(s => Equal(StateReg, OfStateId(s)) -> a)
            ++ other.states.toIndexedSeq.map(s => Equal(StateReg, OfStateId(s)) -> b)))
      else splitCond match
        case Some(sp) => muxMap(vars, other.vars)((a, b) =>
          Mux(IndexedSeq(sp -> a), Some(b)))
        case None => throw new RuntimeException("Cannot merge slices")

      Valid(
        vars = newVars,
        states = states union other.states,
        cond = (cond.keySet intersect other.cond.keySet).toIndexedSeq
          .flatMap(k => Some(k -> cond(k)).filter(_._2 == other.cond(k))).toMap
      )
