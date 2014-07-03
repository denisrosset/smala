package smala

import spire.algebra.Ring
import spire.implicits._

trait RingTree extends Tree {
  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
  import org.kiama.output.{PrettyPrinter, ParenPrettyPrinter,
    PrettyExpression, PrettyBinaryExpression, PrettyUnaryExpression}
  import org.kiama.rewriting.Rewriter._

  case class PlusNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = PlusNode(newChildren)
  }

  case class TimesNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = TimesNode(newChildren)
  }

  case class IntPowerNode(base: Node, exponent: Int) extends ParentNode {
    def children = Seq(base)
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newBase) => IntPowerNode(newBase, exponent)
    }
  }

  case class NegNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => NegNode(n)
    }
  }

  def zeroNode: Node
  def oneNode: Node
  def nodeFromInt(i: Int): Node
  def nodeToInt(n: Node): Option[Int]


  val expandTimes = rule[Node] {
    case TimesNode(seq) if seq.exists(_.isInstanceOf[PlusNode]) =>
      val (before, Seq(PlusNode(terms), after@_*)) = seq.span(!_.isInstanceOf[PlusNode])
      PlusNode(terms.map(term => TimesNode((before :+ term) ++ after)))
  }

  val expandIntPower = rule[Node] {
    case IntPowerNode(base, exponent) => TimesNode(Seq.fill(exponent)(base))
  }

  val ringExpand = expandIntPower + expandTimes

  val flattenTimes = rule[Node] {
    case TimesNode(seq) if seq.exists(_.isInstanceOf[TimesNode]) =>
      val (before, Seq(TimesNode(terms), after@_*)) = seq.span(!_.isInstanceOf[TimesNode])
      TimesNode(before ++ terms ++ after)
  }

  val flattenPlus = rule[Node] {
    case PlusNode(seq) if seq.exists(_.isInstanceOf[PlusNode]) =>
      val (before, Seq(PlusNode(terms), after@_*)) = seq.span(!_.isInstanceOf[PlusNode])
      PlusNode(before ++ terms ++ after)
  }

  val removeEmpty = rule[Node] {
      case TimesNode(Seq()) => oneNode
      case PlusNode(Seq()) => zeroNode
  }

  val unpackSeqOfOne = rule[Node] {
    case TimesNode(Seq(one)) => one
    case PlusNode(Seq(one)) => one
  }

  val doubleNeg = rule[Node] {
    case NegNode(NegNode(node)) => node
  }

  val applyNegToPlus = rule[Node] {
    case NegNode(PlusNode(seq)) => PlusNode(seq.map(-_))
  }

  val extractNegFromTimes = rule[Node] {
    case TimesNode(seq) if seq.exists(_.isInstanceOf[NegNode]) =>
      val count = seq.count(_.isInstanceOf[NegNode])
      val newTimesNode = TimesNode(seq.map {
        case NegNode(node) => node
        case node => node
      })
      if (count % 2 == 0)
        newTimesNode
      else
        NegNode(newTimesNode)
  }

  val ringSimplify = flattenTimes + flattenPlus + removeEmpty + unpackSeqOfOne + doubleNeg + applyNegToPlus + extractNegFromTimes

  class NodeRing extends Ring[Node] {
    def zero = zeroNode
    def one = oneNode
    override def fromInt(n: Int) = nodeFromInt(n)

    def negate(x: Node): Node = x match {
      case NegNode(node) => node
      case PlusNode(seq) => PlusNode(seq.map(negate(_)))
      case _ => NegNode(x)
    }

    def plus(x: Node, y: Node) = (x, y) match {
      case (PlusNode(a), PlusNode(b)) => PlusNode(a ++ b)
      case (PlusNode(a), b) => PlusNode(a :+ b)
      case (a, PlusNode(b)) => PlusNode(a +: b)
      case (a, b) => PlusNode(Seq(a, b))
    }

    override def minus(x: Node, y: Node): Node = (x, y) match {
      case (PlusNode(a), PlusNode(b)) => PlusNode(a ++ b.map(negate(_)))
      case (PlusNode(a), b) => PlusNode(a :+ negate(b))
      case (a, PlusNode(b)) => PlusNode(a +: b.map(negate(_)))
      case (a, b) => PlusNode(Seq(a, negate(b)))
    }

    def times(x: Node, y: Node): Node = (x, y) match {
      case (TimesNode(a), TimesNode(b)) => TimesNode(a ++ b)
      case (TimesNode(a), NegNode(b)) => negate(TimesNode(a :+ b))
      case (NegNode(a), TimesNode(b)) => negate(TimesNode(a +: b))
      case (NegNode(a), NegNode(b)) => TimesNode(Seq(a, b))
      case (a, NegNode(b)) => negate(TimesNode(Seq(a, b)))
      case (NegNode(a), b) => negate(TimesNode(Seq(a, b)))
      case (a, b) => TimesNode(Seq(a, b))
    }

    override def pow(base: Node, exponent: Int): Node = exponent match {
      case 0 => oneNode
      case 1 => base
      case _ => IntPowerNode(base, exponent)
    }
  }

  implicit def nodeRing: Ring[Node] = new NodeRing

  def TreeParser: RingTreeParserTrait

  trait RingTreeParserTrait extends TreeParserTrait {
    def buildPowerNode(base: Node, exponent: Node): Node =
      IntPowerNode(base, nodeToInt(exponent).getOrElse(sys.error(s"Only integer powers are supported and $exponent is not an integer")))

    lazy val p_op: PackratParser[Node] =
      ((primary <~ "**") ~ u_expr) ^^ {
        case base ~ exponent => buildPowerNode(base, exponent)
      }

    lazy val u_op: PackratParser[Node] =
      (("-" ~> u_expr) ^^ (n => NegNode(n))) |||
      (("+" ~> u_expr) ^^ identity)

    lazy val m_op: PackratParser[Node] =
      ((m_expr <~ "*") ~ u_expr) ^^ { case x ~ y => TimesNode(Seq(x, y)) }

    lazy val a_op: PackratParser[Node] =
      ((a_expr <~ "+") ~ m_expr) ^^ { case x ~ y => PlusNode(Seq(x, y)) }

    lazy val s_op: PackratParser[Node] =
      ((a_expr <~ "-") ~ m_expr) ^^ { case x ~ y => PlusNode(Seq(x, NegNode(y))) }
  }

  import org.kiama.output._
  override def pretty(node: Node): PrettyExpression = node match {
    case IntPowerNode(base, exponent) => new PrettyBinaryExpression {
      val left = pretty(base)
      val op = "^"
      val right = pretty(nodeFromInt(exponent))
      val priority = 10
      val fixity = Infix(RightAssoc)
    }
    case NegNode(n) => new PrettyUnaryExpression {
      val exp = pretty(n)
      val op = "-"
      val priority = 20
      val fixity = Infix(LeftAssoc)
    }
    case PlusNode(Seq(head, tail@_*)) => (pretty(head) /: tail) {
      case (l, NegNode(r)) => new PrettyBinaryExpression {
        val left = l
        val op = "-"
        val right = pretty(r)
        val priority = 40
        val fixity = Infix(LeftAssoc)
      }
      case (l, r) => new PrettyBinaryExpression {
        val left = l
        val op = "+"
        val right = pretty(r)
        val priority = 40
        val fixity = Infix(LeftAssoc)
      }
    }
    case TimesNode(Seq(head, tail@_*)) => (pretty(head) /: tail) {
      case (l, r) => new PrettyBinaryExpression {
        val left = l
        val op = "*"
        val right = pretty(r)
        val priority = 30
        val fixity = Infix(LeftAssoc)
      }
    }
    case _ => super.pretty(node)
  }
}
