package smala

import spire.algebra.{Ring, Field}
import spire.implicits._

trait FieldTree extends RingTree {
  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
  import org.kiama.output.{PrettyPrinter, ParenPrettyPrinter,
    PrettyExpression, PrettyBinaryExpression, PrettyUnaryExpression}
  import org.kiama.rewriting.Rewriter._

  case class InvNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => InvNode(n)
    }
  }

  val doubleInv = rule[Node] {
    case InvNode(InvNode(node)) => node
  }

  val invNeg = rule[Node] {
    case InvNode(NegNode(node)) => NegNode(InvNode(node))
  }

  val applyInvToTimes = rule[Node] {
    case InvNode(TimesNode(seq)) => TimesNode(seq.map(_.reciprocal))
  }

  val fieldSimplify = doubleInv + invNeg + applyInvToTimes

  class NodeField extends NodeRing with Field[Node] {
    def gcd(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def mod(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def quot(a: Node,b: Node): Node =
      sys.error("Not implemented")

    override def reciprocal(x: Node): Node = x match {
      case InvNode(node) => node
      case TimesNode(seq) => TimesNode(seq.map(reciprocal(_)))
      case _ => InvNode(x)
    }

    def div(x: Node, y: Node): Node = (x, y) match {
      case (TimesNode(a), TimesNode(b)) => TimesNode(a ++ b.map(_.reciprocal))
      case (TimesNode(a), b) => TimesNode(a :+ (b.reciprocal))
      case (a, TimesNode(b)) => TimesNode(a +: b.map(_.reciprocal))
      case (a, b) => TimesNode(Seq(a, b.reciprocal))
    }
  }

  implicit def nodeField: Field[Node] = new NodeField

  def TreeParser: FieldTreeParserTrait

  trait FieldTreeParserTrait extends RingTreeParserTrait {
    lazy val d_op: PackratParser[Node] = ((m_expr <~ "/") ~ u_expr) ^^ { case x ~ y => TimesNode(Seq(x, InvNode(y))) }
  }

  import org.kiama.output._
  override def pretty(node: Node): PrettyExpression = node match {
    case in: InvNode => pretty(TimesNode(Seq(oneNode, in)))
    // overrides TimesNode from RingTree
    case TimesNode(Seq(head, tail@_*)) => (pretty(head) /: tail) {
      case (l, InvNode(r)) => new PrettyBinaryExpression {
        val left = l
        val op = "/"
        val right = pretty(r)
        val priority = 30
        val fixity = Infix(LeftAssoc)
      }
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
