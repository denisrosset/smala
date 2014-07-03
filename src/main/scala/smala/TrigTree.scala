package smala

import spire.algebra.{Field, Ring, Trig}
import spire.implicits._

trait TrigTree extends FieldTree {
  import scala.collection.immutable.Seq

  case class FunNode(name: String, node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => FunNode(name, n)
    }
  }

  /** Atom for constants pi and e (and potientially others!). */
  case class ConstantNode(name: String) extends AtomNode

  /** A node representing left to the power right. */
  case class PowerNode(left: Node, right: Node) extends BinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => PowerNode(newLeft, newRight)
    }
  }

  // no additional simplifications, is not overriding trySimplified

  class NodeTrig extends NodeField with Trig[Node] {
    def e = ConstantNode("e")
    def pi = ConstantNode("pi")

    def exp(node: Node) = FunNode("exp", node)
    def expm1(node: Node) = FunNode("exp", node - oneNode)
    def log(node: Node) = FunNode("log", node)
    def log1p(node: Node) = FunNode("log", oneNode + node)

    def sin(node: Node) = FunNode("sin", node)
    def cos(node: Node) = FunNode("cos", node)
    def tan(node: Node) = FunNode("tan", node)

    def asin(node: Node) = FunNode("asin", node)
    def acos(node: Node) = FunNode("acos", node)
    def atan(node: Node) = FunNode("atan", node)
    def atan2(y: Node, x: Node) = sys.error("TODO: not yet implemented")

    def sinh(node: Node) = FunNode("sinh", node)
    def cosh(node: Node) = FunNode("cosh", node)
    def tanh(node: Node) = FunNode("tanh", node)

    def toRadians(node: Node) = node / fromInt(180) * pi
    def toDegrees(node: Node) = node * fromInt(180) / pi

    def fpow(x: Node, y: Node) = PowerNode(x, y)
    def nroot(a: Node, n: Int) = PowerNode(a, fromInt(n).reciprocal)
  }

  implicit def nodeTrig = new NodeTrig

  def TreeParser: TrigTreeParserTrait

  trait TrigTreeParserTrait extends FieldTreeParserTrait {
    // TODO: implement NRootTree with Rational exponents
    override def buildPowerNode(base: Node, exponent: Node) =
      nodeToInt(exponent).map(i => IntPowerNode(base, i)).getOrElse(PowerNode(base, exponent))

    lazy val trigConstant: PackratParser[Node] = 
      ("pi" ^^^ ConstantNode("pi")) |
      ("e" ^^^ ConstantNode("e"))

    lazy val call: PackratParser[Node] = identifier ~ (("(" ~> a_expr) <~ ")") ^^ {
      case name ~ node => FunNode(name, node)
    }

    def identifier = """[a-zA-z]+[0-9a-zA-z]*""".r
  }

  import org.kiama.output._
  override def pretty(node: Node): PrettyExpression = node match {
    case ConstantNode(name) => PrettyString(name)
    case PowerNode(l, r) => new PrettyBinaryExpression {
      val left = pretty(l)
      val op = "^"
      val right = pretty(r)
      val priority = 10
      val fixity = Infix(RightAssoc)
    }
    case FunNode(name, node) => PrettyEnclosed(name + "(", pretty(node), ")")
    case _ => super.pretty(node)
  }
}
