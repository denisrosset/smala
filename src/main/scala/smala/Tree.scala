package smala

/** A trait for expression trees that can be parsed and pretty-printed. */
trait Tree {
  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
  import org.kiama.output.{PrettyPrinter, ParenPrettyPrinter,
    PrettyExpression, PrettyBinaryExpression, PrettyUnaryExpression}
  import org.kiama.rewriting.{Rewritable, Strategy}

  case class PrettyString(string: String) extends PrettyExpression
  case class PrettyEnclosed(left: String, exp: PrettyExpression, right: String) extends PrettyExpression

  def pretty(node: Node): PrettyExpression =
    sys.error(s"Unsupported node $node")

  object Printer extends PrettyPrinter with ParenPrettyPrinter {
    override def toParenDoc(e: PrettyExpression): Doc = e match {
      case PrettyEnclosed(left, exp, right) => left <> toParenDoc(exp) <> right
      case PrettyString(string) => text(string)
      case _ => super.toParenDoc(e)
    }
  }

  def print(node: Node): String = Printer.pretty(Printer.toParenDoc(pretty(node)))

  /** A node in an expression tree. */
  sealed trait Node {
    def tree = Tree.this
  }

  /** An atomic node in an expression tree, which can be
    * used for values or symbols. Should be extended. */
  trait AtomNode extends Node

  /** Node with children. */
  trait ParentNode extends Node with Rewritable {
    // to implement
    def children: Seq[Node]
    def build(newChildren: Seq[Node]): ParentNode

    // implementing Rewritable
    def arity = children.size
    def deconstruct = children match {
      case seq: scala.collection.immutable.Seq[Node] => seq
      case _ => scala.collection.immutable.Seq(children: _*)
    }
    def reconstruct(components: Seq[Any]) = build(components.map(_.asInstanceOf[Node]))
  }

  /** A node representing a binary operation. */
  trait BinaryNode extends ParentNode {
    def children = Seq(left, right)
    def left: Node
    def right: Node
  }

  /** Sequence node representing a sum or a product of terms.
    * 
    * Subtraction and division are handled by adding the opposite
    * or multiplying by the inverse.
    */
  trait SeqNode extends ParentNode {
    def children = args
    def build(newChildren: Seq[Node]): SeqNode
    def args: Seq[Node]
  }

  /** Node representing unary operations. */
  trait UnaryNode extends ParentNode {
    def node: Node
    def children = Seq(node)
  }

  def allSimplifyRules: Strategy
  import org.kiama.rewriting.Rewriter
  import Rewriter.innermost
  lazy val simplifyRules = innermost(allSimplifyRules)
  def simplified(node: Node): Node = Rewriter.rewrite[Node](simplifyRules)(node)

  def parse(str: String): Parsers#ParseResult[Node] = TreeParser.parse(str)

  /** Parser for an expression. */
  def TreeParser: TreeParserTrait

  trait TreeParserTrait extends RegexParsers with PackratParsers {
    import scala.util.parsing.input.CharSequenceReader

    def parse(s: String): ParseResult[Node] =
      phrase(a_expr)(new PackratReader(new CharSequenceReader(s)))

    // grammar adapted from the Python grammar:
    // https://docs.python.org/3/reference/expressions.html
    lazy val primary: PackratParser[Node] = atom | call | parenth_form

    def atom: Parser[Node]

    lazy val enclosure: PackratParser[Node] = parenth_form

    lazy val p_expr: PackratParser[Node] = primary |||
      p_op

    def p_op: Parser[Node]

    lazy val u_expr: PackratParser[Node] = p_expr |||
      u_op

    /** Unary operators */
    def u_op: Parser[Node]

    lazy val m_expr: PackratParser[Node] = u_expr |||
      (m_op | d_op)

    /** Multiplication */
    def m_op: Parser[Node]
    /** Division */
    def d_op: Parser[Node]

    lazy val a_expr: PackratParser[Node] = m_expr |||
      (a_op | s_op)

    /** Addition */
    def a_op: Parser[Node]

    /** Subtraction */
    def s_op: Parser[Node]

    def call: Parser[Node]

    lazy val parenth_form: PackratParser[Node] = "(" ~> a_expr <~ ")"
  }
}
