package smala

import spire.math.Real
import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

object RealTree extends FloatTree with RationalTree with TrigTree {
  lazy val allSimplifyRules = floatSimplify + rationalSimplify + fieldSimplify + ringSimplify

  object TreeParser extends RationalTreeParserTrait with FloatTreeParserTrait with TrigTreeParserTrait {
    lazy val atom: PackratParser[Node] = float | integer | trigConstant
  }
}

object RealTreeEvaluator extends TrigEvaluator[RealTree.type, Real] {
  val tree = RealTree
  implicit val scalarAlgebra = Real.algebra
  def fpow(a: Real, b: Real) = a.fpow(b)
  override def value(node: tree.Node): Real = node match {
    case tree.RationalNode(r) => Real(r)
    case tree.FloatNode(bd, powerOfTen) => Real(bd) * Real(10).fpow(powerOfTen)
    case _ => super.value(node)
  }
}
