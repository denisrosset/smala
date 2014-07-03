package smala

import spire.math.Rational
import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

trait RationalTree extends FieldTree {
  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
  import org.kiama.output.{PrettyPrinter, ParenPrettyPrinter,
    PrettyExpression, PrettyBinaryExpression, PrettyUnaryExpression}
  import org.kiama.rewriting.Rewriter._

  case class RationalNode(r: Rational) extends AtomNode

  val oneNode = RationalNode(Rational.one)
  val zeroNode = RationalNode(Rational.zero)

  def nodeFromInt(n: Int) = RationalNode(Rational(n))
  def nodeToInt(n: Node): Option[Int] = {
    val e = RationalTree.evaluator(RationalTree.this)
    val r: Rational = scala.util.Try(e.value(n.asInstanceOf[e.tree.Node])).getOrElse(sys.error(s"Node $n should be an integer"))
    if (r.isWhole) Some(r.toInt) else None
  }

  val negRational = rule[Node] {
    case NegNode(RationalNode(r)) => RationalNode(-r)
  }

  val invRational = rule[Node] {
    case InvNode(RationalNode(r)) => RationalNode(r.reciprocal)
  }

  val plusRational = rule[Node] {
    case PlusNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
      val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
      val r = rational.map(_.asInstanceOf[RationalNode].r).reduceOption(_+_).getOrElse(Rational.zero)
      if (r != 0)
        PlusNode(nonRational :+ RationalNode(r))
      else
        PlusNode(nonRational)
  }

  val timesRational = rule[Node] {
    case TimesNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
      val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
      val r = rational.map(_.asInstanceOf[RationalNode].r).reduceOption(_*_).getOrElse(Rational.one)
      if (r == 1)
        TimesNode(nonRational)
      else
        r.signum match {
          case 0 => zeroNode
          case -1 => NegNode(TimesNode(RationalNode(-r) +: nonRational))
          case 1 => TimesNode(RationalNode(r) +: nonRational)
        }
  }

  val rationalSimplify = negRational + invRational + plusRational + timesRational

  def TreeParser: RationalTreeParserTrait

  trait RationalTreeParserTrait extends FieldTreeParserTrait {
    lazy val integer: PackratParser[Node] =
      biginteger ^^ { case i => RationalNode(Rational(i)) }

    val biginteger = """0|([1-9]\d*)""".r ^^ { case s: String => BigInt(s) }
  }

  override def pretty(node: Node): PrettyExpression = node match {
    case RationalNode(r) if r < 0 => pretty(NegNode(RationalNode(-r)))
    case RationalNode(r) if r.denominator != 1 =>
      pretty(TimesNode(Seq(RationalNode(r.numerator), RationalNode(r.denominator))))
    case RationalNode(r) if r >=0 && r.denominator == 1 =>
      PrettyString(r.toString)
    case _ => super.pretty(node)
  }
}

object RationalTree {
  def evaluator(rt: RationalTree): FieldEvaluator[RationalTree, Rational] =
    new FieldEvaluator[RationalTree, Rational] {
      val tree = rt
      implicit val scalarAlgebra = Rational.RationalAlgebra
      override def value(node: tree.Node): Rational = node match {
        case tree.RationalNode(r) => r
        case _ => super.value(node)
      }
    }
}
