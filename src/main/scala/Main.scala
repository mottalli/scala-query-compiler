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
  def as(theAlias: String): this.type = { alias = Some(theAlias); this }
  def asc: SortClause = SortClause(this, SortOrder.ASC)
  def desc: SortClause = SortClause(this, SortOrder.DESC)
}

/** Represents a reference to a non-resolved, named value */
case class NamedValue(name: String) extends Value

/** A variable that comes from an external source */
case class VariableValue(name: String) extends Value

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
  implicit def stringToNamedValue(name: String): Value = NamedValue(name)

  val * : Value = AllValues()
  def SUM(value: Value) = SumAggregation(value)
  def COUNT(value: Value) = SumAggregation(value)
}
import Implicits._

case class Column(name: String)

case class Table(name: String) {
  def scan = ScanNode(this)
}

class ValueNotResolvedException(name: String) extends Exception(s"Value '$name' could not be resolved")
class AmbiguousNameException(name: String) extends Exception(s"Value '$name' is ambiguous")

trait QueryNode {
  def filter(constraint: Constraint) = ConstraintNode(this, constraint)
  def aggregate(aggregatedValues: List[AggregatedValue]) = AggregateNode(this, aggregatedValues)
  def groupBy(groupValues: List[Value]) = GroupNode(this, groupValues)
  def orderBy(clauses: List[SortClause]) = SortNode(this, clauses)
  def select(values: List[Value]) = SelectNode(this, values)
  def project = select _
}

trait SingleChildNode extends QueryNode {
  val childNode: QueryNode
}

trait JoinNode extends QueryNode {
  val leftNode: QueryNode
  val rightNode: QueryNode
}

case class ScanNode(table: Table) extends QueryNode

case class ConstraintNode(childNode: QueryNode, constraint: Constraint) extends SingleChildNode
case class GroupNode(childNode: QueryNode, groupValues: List[Value]) extends SingleChildNode
case class AggregateNode(childNode: QueryNode, aggregatedValues: List[AggregatedValue]) extends SingleChildNode
case class SelectNode(childNode: QueryNode, values: List[Value]) extends SingleChildNode

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
  // HAVING sum < 10 OR sum > :var1
  // ORDER BY cnt DESC

  val table1 = new Table("table1")

  val query = table1.scan
    .filter("foo" > ConstantValue(4))
    .groupBy(List("foo"))
    .aggregate(List(SUM("bar") as "sum", COUNT(*) as "cnt"))
    .filter("sum" < ConstantValue(20) or "sum" > VariableValue("var1"))
    .orderBy(List("cnt" desc))
    .select(List("foo", "sum", "cnt"))

  println(query)
}
