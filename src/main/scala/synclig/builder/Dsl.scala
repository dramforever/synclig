package synclig.builder

import scala.collection.mutable

import synclig.types._

object Dsl:
  import Expr._

  case class Context(
    stateReg: Name,
    validNames: mutable.Set[Name] = mutable.Set(),
    var flow: Flow = Flow.empty):

    def gen(body: WithContext[Unit]): Flow =
      body(using this)
      flow

  def context: WithContext[Context] = summon[Context]

  type WithContext[T] = Context ?=> T

  def reg(name: String, init: Expr.WithSubst = Undefined): WithContext[Reg] =
    val res = Name(name)
    context.validNames += res
    context.flow ++= Flow.assign(res, init)
    Reg(res)

  def (name: Reg) := (value: Expr.WithSubst): WithContext[Unit] =
    context.flow ++= Flow.assign(name.name, value)

  def skip: WithContext[Unit] =
    val state = StateId()
    val f = Flow.Split(
      mid = Slice.Empty,
      in = sl => sl.assign(context.stateReg, OfStateId(state)),
      out = Slice.NonEmpty(
        Slice.Valid(
          vars = context.validNames.toIndexedSeq
            .map(v => v -> VarReg(v)).toMap
              + (context.stateReg -> StateReg),
          states = Set(state),
          cond = Map()
        )
      )
    )

    context.flow ++= f

  def when(branches: (Expr.WithSubst, WithContext[Unit])*): WithContext[Unit] =
    context.flow ++= branches.foldRight(Flow.empty) {
      case ((cond, body), next) =>
        val sub = context.copy(flow = Flow.cond(cond, true)).gen(body)
        sub merge (Flow.cond(cond, false) ++ next)
    }

  def when(cond: Expr.WithSubst)(body: WithContext[Unit]): WithContext[Unit] =
    when(((using subst: Subst) => cond(using subst)) -> ((using ctx: Context) => body(using ctx)))

  def whilst(cond: Expr.WithSubst)(body: WithContext[Unit]): WithContext[Unit] =
    val sub = context.copy(flow = Flow.cond(cond, true)).gen(body)
    sub match
      case _: Flow.Joined =>
        throw new Dsl.CombinatorialLoop
      case Flow.Split(mid, in, out) =>
        context.flow ++=
          Flow.Joined { sl =>
            val m = sl merge out
            Flow.Through(
              pass = m.cond(cond, false),
              store = mid merge in(m.cond(cond, true))
            )
          }

  def loop(body: WithContext[Unit]): WithContext[Unit] =
    val sub = context.copy(flow = Flow.empty).gen(body)
    sub match
      case _: Flow.Joined =>
        throw new Dsl.CombinatorialLoop
      case Flow.Split(mid, in, out) =>
        context.flow ++= Flow.Split(
          mid = mid merge in(out),
          in = in,
          out = Slice.Empty,
        )

  def run(body: WithContext[Unit]): Slice =
    val reset = StateId()
    val stateReg = Name("state")
    val init = Slice.NonEmpty(
      Slice.Valid(
        vars = Map(stateReg -> StateReg),
        states = Set(reset),
        cond = Map()
      )
    )
    val ctx = new Context(stateReg = stateReg)
    val res = ctx.gen(body)
    res(init) match
      case Flow.Through(pass, store) =>
        pass match
          case Slice.Empty => store
          case _ => throw new FallThrough

  class CombinatorialLoop extends RuntimeException:
    override def toString: String = "Combinatorial loop"

  class FallThrough extends RuntimeException:
    override def toString: String = "Process may fall through"
