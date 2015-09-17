object ConstraintType extends Enumeration {
  type ConstraintType = Value
  val EQ, NE, GT, GE, LT, LE, IN = Value
}

import ConstraintType.ConstraintType

sealed trait Constraint extends Value {
  def or(rhs: Constraint) = OrConstraint(this, rhs)
  def and(rhs: Constraint) = AndConstraint(this, rhs)
}
case class ComparisonConstraint(lhs: Value, constraintType: ConstraintType, rhs: Value) extends Constraint
case class NotConstraint(what: Constraint) extends Constraint
case class AndConstraint(lhs: Constraint, rhs: Constraint) extends Constraint
case class OrConstraint(lhs: Constraint, rhs: Constraint) extends Constraint
case class InConstraint(what: Value, in: QueryNode) extends Constraint

sealed trait Value {
  var alias: Option[String] = None

  def >(other: Value) = ComparisonConstraint(this, ConstraintType.GT, other)
  def <(other: Value) = ComparisonConstraint(this, ConstraintType.LT, other)
  def sum = SumAggregation(this)
  def count = CountAggregation(this)
  def as(theAlias: String): this.type = { alias = Some(theAlias); this }
  def asc: SortClause = SortClause(this, SortOrder.ASC)
  def desc: SortClause = SortClause(this, SortOrder.DESC)

  def name: String = alias match {
    case Some(a) => a
    case None => toString
  }
}

sealed trait AggregatedValue extends Value {
}

case class SumAggregation(value: Value) extends AggregatedValue
case class CountAggregation(value: Value) extends AggregatedValue
case class ColumnValue(column: Column) extends Value

case class NullValue() extends Value
case class AllValues() extends Value
case class ConstantValue[T <: AnyVal](constant: T) extends Value
object Implicits {
  implicit def constantToVal[T <: AnyVal](v: T): ConstantValue[T] = ConstantValue(v)
  val * : Value = AllValues()
}
import Implicits._

case class Column(name: String)

case class Table(name: String) {
  def scan(columns: List[String]) = ScanNode(this, columns.map(name => ColumnValue(Column(name))))
}

class ValueNotResolvedException(name: String) extends Exception(s"Value '$name' could not be resolved")
class AmbiguousNameException(name: String) extends Exception(s"Value '$name' is ambiguous")

trait QueryNode {
  val producedValues: List[Value]

  def resolveValueByName(name: String): Option[Value] = producedValues.find { v =>
    if (v.name == name) true else v match {
      case ColumnValue(c) => c.name == name
      case _ => false
    }
  }

  def apply(valueName: String): Value = resolveValueByName(valueName).getOrElse { throw new ValueNotResolvedException(valueName) }

  //def scan(dataSource: DataSource) = ScanNode(dataSource)
  def filter(constraintFunc: QueryNode => Constraint) = ConstraintNode(this, constraintFunc(this))
  def aggregate(aggregatesFunc: QueryNode => List[AggregatedValue]) = AggregateNode(this, aggregatesFunc(this))
  def groupBy(groupFunc: QueryNode => List[Value]) = GroupNode(this, groupFunc(this))
  def orderBy(orderFunc: QueryNode => List[SortClause]) = SortNode(this, orderFunc(this))
  def select(selectFunc: QueryNode => List[Value]) = SelectNode(this, selectFunc(this))
  def project = select _
}

trait SingleChildNode extends QueryNode {
  val childNode: QueryNode
  val producedValues: List[Value] = childNode.producedValues
}

trait JoinNode extends QueryNode {
  val leftNode: QueryNode
  val rightNode: QueryNode

  val producedValues: List[Value] = leftNode.producedValues ++ rightNode.producedValues

  override def resolveValueByName(name: String): Option[Value] = {
    val leftValue = leftNode.resolveValueByName(name)
    val rightValue = rightNode.resolveValueByName(name)

    // If we have both a left and a right value, it means the name was ambiguous
    if (leftValue.isDefined && rightValue.isDefined)
      throw new AmbiguousNameException(name)
    leftValue.orElse(rightValue)
  }
}

case class ScanNode(table: Table, columns: List[ColumnValue]) extends QueryNode {
  val producedValues: List[Value] = columns
}

case class ConstraintNode(childNode: QueryNode, constraint: Constraint) extends SingleChildNode
case class GroupNode(childNode: QueryNode, groupValues: List[Value]) extends SingleChildNode
case class AggregateNode(childNode: QueryNode, aggregatedValues: List[AggregatedValue]) extends SingleChildNode {
  override val producedValues: List[Value] = childNode.producedValues ++ aggregatedValues
}
case class SelectNode(childNode: QueryNode, values: List[Value]) extends SingleChildNode {
  override val producedValues: List[Value] = values
}

object SortOrder extends Enumeration {
  type SortOrder = Value
  val ASC, DESC = Value
}
import SortOrder.SortOrder

case class SortClause(value: Value, order: SortOrder)
case class SortNode(childNode: QueryNode, sortBy: List[SortClause]) extends SingleChildNode

object QueryPlanOptimizer {
  def optimizeQuery(rootNode: QueryNode): QueryNode = {
    rootNode
  }
}

object Main extends App {
  // SELECT foo, SUM(bar) AS sum, COUNT(*) AS cnt
  // FROM table1
  // WHERE foo > 4
  // GROUP BY foo
  // HAVING sum < 10 OR sum > 20
  // ORDER BY cnt DESC

  val table1 = new Table("table1")

  val query = table1.scan(List("foo", "bar"))
    .groupBy(row => List(row("foo")))
    .aggregate(row => List(row("bar").sum as "sum", *.count as "cnt"))
    .filter(row => row("sum") < 10 or row("sum") > 20)
    .filter(row => row("foo") > 4)
    .orderBy(row => List(row("cnt") desc))
    .select(row => List(row("foo"), row("sum"), row("cnt")))

  println(query)
}
