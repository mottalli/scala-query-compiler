import scala.reflect.ClassTag

trait DataType

trait Expression {
  var alias: Option[String] = None

  def as(name: String): this.type = { alias = Some(name); this }
  def matchesName(name: String): Boolean = alias match {
    case Some(a) => name == a
    case None => false
  }

  def >(other: Expression): ExpressionConstraint = ???
  def >=(other: Expression): ExpressionConstraint = ???
  def <(other: Expression): ExpressionConstraint = ???
  def <=(other: Expression): ExpressionConstraint = ???
  def ===(other: Expression): ExpressionConstraint = ???
  def =!=(other: Expression): ExpressionConstraint = ???

  def +(other: Expression): Expression = SumExpression(this, other)
  def -(other: Expression): Expression = ???
  def *(other: Expression): Expression = ???
  def /(other: Expression): Expression = ???

  def asc: OrderClause = OrderClause(this, ascending=true)
  def desc: OrderClause = OrderClause(this, ascending=false)
}

case class SumExpression(lhs: Expression, rhs: Expression) extends Expression

case class ConstantExpression[T: ClassTag](constant: T) extends Expression {
  protected def _matchesName(name: String): Boolean = false
}

class ExpressionList(val expressions: List[Expression]) extends Seq[Expression] {
  def resolveExpression(expressionName: String): Option[Expression] = expressions.find(_.matchesName(expressionName))
  def apply(expressionName: String): Expression = resolveExpression(expressionName).get

  def length: Int = expressions.length
  def apply(idx: Int): Expression = expressions.apply(idx)
  def iterator: Iterator[Expression] = expressions.iterator
}

trait ExpressionConstraint {
  def or(other: ExpressionConstraint): ExpressionConstraint = ???
  def and(other: ExpressionConstraint): ExpressionConstraint = ???
  def not: ExpressionConstraint = ???
}

case class OrderClause(expression: Expression, ascending: Boolean=true)

abstract class QueryNode {
  val producedExpressions: ExpressionList

  def filter(f: ExpressionList => ExpressionConstraint): QueryNode = FilterNode(this, f(producedExpressions))
  def map(f: ExpressionList => Seq[Expression]): QueryNode = ???
  def sortBy(f: ExpressionList => Seq[OrderClause]): QueryNode = ???
  def groupBy(f: ExpressionList => Seq[Expression]): GroupNode = ???
}

class SingleChildNode(val childNode: QueryNode) extends QueryNode {
  val producedExpressions: ExpressionList = childNode.producedExpressions
}

case class FilterNode(child: QueryNode, constraint: ExpressionConstraint) extends SingleChildNode(child)

case class GroupNode(child: QueryNode, keys: ExpressionList) extends SingleChildNode(child) {
  def reduceByKey(f: (ExpressionList, ExpressionList) => Seq[Expression]): QueryNode = ???
}

class Column(val table: Table, val name: String, val dataType: DataType)

class Table {
  def scan(columnNames: String*): QueryNode = ???
}

object Main extends App {
  // SELECT foo, SUM(bar) AS sum, COUNT(*) AS cnt
  // FROM table1
  // WHERE foo > 4
  // GROUP BY foo
  // HAVING sum < 10 OR sum > :var1
  // ORDER BY cnt DESC
  /*
  table1
    .scan("foo", "bar")
    .filter(_("foo") > 4)
    .map(row => row ++ (1L as "one"))
    .groupBy(_("foo"))
    .reduceByKey( (v1, v2) => (
      (v1("bar")+v2("bar")) as "sum",
      (v1("one")+v2("one")) as "cnt"
      ))
    .filter(row => (row("sum") < 10) or (row("sum") > ?))
    .sortBy(_("cnt") desc)
   */
  val table1 = new Table
  table1
    .scan("foo", "bar")
    .filter(row => row("foo") > ConstantExpression(4))
    .map(row => row :+ (ConstantExpression(1) as "one"))
    .groupBy(row => row("foo") :: Nil)
    .reduceByKey{ case (r1, r2) =>
      (r1("var")+r2("var") as "sum") ::
      (r1("one")+r2("one") as "cnt") ::
      Nil
    }
    .filter(row => row("sum") < ConstantExpression(10) or row("sum") > ConstantExpression(20))
    .sortBy(_("cnt").desc :: Nil)
}
