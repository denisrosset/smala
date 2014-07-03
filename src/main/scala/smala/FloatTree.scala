package smala

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

trait FloatTree extends FieldTree {
  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
  import org.kiama.output.{PrettyPrinter, ParenPrettyPrinter,
    PrettyExpression, PrettyBinaryExpression, PrettyUnaryExpression}
  import org.kiama.rewriting.Rewriter._

  case class FloatNode(bd: BigDecimal, powerOfTen: Int) extends AtomNode

  val negFloat = rule[Node] {
    case NegNode(FloatNode(bd, powerOfTen)) => FloatNode(-bd, powerOfTen)
  }

  val floatSimplify = negFloat

  def TreeParser: FloatTreeParserTrait

  trait FloatTreeParserTrait extends FieldTreeParserTrait {
    lazy val float: PackratParser[Node] =
      (bigdecimal ~ opt(("e" | "E") ~> int))  ^^ { 
        case bd ~ None => FloatNode(bd, 0)
        case bd ~ Some(exp) => FloatNode(bd, exp)
      }

    val int = """-?(0|([1-9]\d*))""".r ^^ { case s: String => s.toInt }
    val bigdecimal = """(0|([1-9]\d*))?[.]\d*""".r ^^ { case s: String => BigDecimal(s) }
  }

  def printBigDecimal(bd: BigDecimal): String = bd.scale match {
    case 0 => bd.toString + "."
    case _ => bd.toString
  }

  override def pretty(node: Node): PrettyExpression = node match {
    case FloatNode(bd, powerOfTen) if bd < 0 =>
      pretty(NegNode(FloatNode(-bd, powerOfTen)))
    case FloatNode(bd, 0) if bd >= 0 =>
      PrettyString(printBigDecimal(bd))
    case FloatNode(bd, powerOfTen) if bd >= 0 =>
      PrettyString(s"${printBigDecimal(bd)}e${powerOfTen}")
    case _ => super.pretty(node)
  }
}
