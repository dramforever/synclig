package synclig.types

sealed abstract class Expr extends Equals:
  override def equals(obj: Any): Boolean = obj match
    case expr: Expr => this eq expr
    case _ => false

  override def hashCode(): Int = System.identityHashCode(this)

object Expr:
  case class Mux(branches: IndexedSeq[(Expr, Expr)], default: Option[Expr] = None) extends Expr:
    override def toString: String =
      s"${branches.mkString("[", ",", "")}, def ${default}]"

  case class Equal(lhs: Expr, rhs: Expr) extends Expr:
    override def toString: String =
      s"${lhs} == ${rhs}"

  case class OfStateId(state: StateId) extends Expr:
    override def toString: String =
      s"state.${state}"

  case class VarReg(name: Name) extends Expr:
    override def toString: String =
      s"var.${name}"

  case object StateReg extends Expr:
    override def toString: String =
      s"state_reg"

  case object Undefined extends Expr:
    override def toString: String =
      s"und"

  case class Binary(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr:
    override def toString: String =
      s"(${lhs} ${op} ${rhs})"

  case class IntLit(sign: Signedness, value: Long) extends Expr:
    override def toString: String =
      s"${value}${sign}"

  sealed trait BinaryOp
  case object Add extends BinaryOp:
    override def toString: String = "+"
  case object Sub extends BinaryOp:
    override def toString: String = "-"

  sealed trait Signedness
  case object IsSigned extends Signedness:
    override def toString: String = "s"
  case object IsUnigned extends Signedness:
    override def toString: String = "u"

  def (lhs: Expr) + (rhs: Expr): Expr = Binary(Add, lhs, rhs)
  def (lhs: Expr) - (rhs: Expr): Expr = Binary(Sub, lhs, rhs)

  extension (value: Long):
    def U: Expr = IntLit(IsUnigned, value)
    def S: Expr = IntLit(IsSigned, value)

  case class Subst(vars: Map[Name, Expr])

  type WithSubst = Subst ?=> Expr

  case class Reg(name: Name)

  implicit def regToWithSubst(reg: Reg): WithSubst =
    (using subst: Subst) => subst.vars(reg.name)
