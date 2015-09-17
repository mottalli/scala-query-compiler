sealed trait Constraint extends Value {
  def or(rhs: Constraint) = OrConstraint(this, rhs)
  def and(rhs: Constraint) = AndConstraint(this, rhs)
}
case class GreaterThanConstraint(lhs: Value, rhs: Value) extends Constraint
case class SmallerThanConstraint(lhs: Value, rhs: Value) extends Constraint
case class NotConstraint(what: Constraint) extends Constraint
case class AndConstraint(lhs: Constraint, rhs: Constraint) extends Constraint
case class OrConstraint(lhs: Constraint, rhs: Constraint) extends Constraint
case class InConstraint(what: Value, in: QueryNode) extends Constraint

sealed trait Value {
  def >(other: Value) = GreaterThanConstraint(this, other)
  def <(other: Value) = SmallerThanConstraint(this, other)
  def sum = SumAggregation(this)
  def as(alias: String) = AliasedValue(this, alias)
}

sealed trait AggregatedValue extends Value {
}

case class SumAggregation(value: Value) extends AggregatedValue
case class AliasedValue(value: Value, alias: String) extends Value
case class ColumnValue(column: Column) extends Value

case class NullValue() extends Value
case class ConstantValue[T <: AnyVal](constant: T) extends Value
object Implicits {
  implicit def constantToVal[T <: AnyVal](v: T): ConstantValue[T] = ConstantValue(v)
}
import Implicits.constantToVal

trait Column {
  val name: String
}

class TableColumn(val name: String) extends Column

trait DataSource {
}

trait QueryNode {
  val producedValues: List[Value]

  def resolveValueByName(name: String): Option[Value] = producedValues.find {
    case ColumnValue(col) => col.name == name
    case AliasedValue(_, alias) => alias == name
    case _ => false
  }

  def apply(valueName: String): Value = resolveValueByName(valueName).get

  def scan(dataSource: DataSource) = ScanNode(dataSource)
  def filter(constraintFunc: QueryNode => Constraint) = ConstraintNode(this, constraintFunc(this))
  def map(mapFunc: QueryNode => List[Value]) = MapNode(this, mapFunc(this))
  def groupBy(groupFunc: QueryNode => List[Value]) = GroupNode(this, groupFunc(this))
}

trait SingleChildNode extends QueryNode {
  val childNode: QueryNode
  val producedValues = childNode.producedValues
}

case class ScanNode(source: DataSource) extends QueryNode {
  val producedValues: List[Value] = (new TableColumn("foo") :: new TableColumn("bar") :: Nil).map(ColumnValue(_))
}

case class ConstraintNode(childNode: QueryNode, constraint: Constraint) extends SingleChildNode
case class GroupNode(childNode: QueryNode, groupValues: Seq[Value]) extends SingleChildNode
case class MapNode(childNode: QueryNode, groupValues: Seq[Value]) extends SingleChildNode

object Main extends App {
  val query = ScanNode(new DataSource {})
    .filter(row => row("foo") > 4)
    .map(row => (row("bar").sum as "sum") :: Nil)
    .groupBy(row => row("foo") :: Nil)
    .filter(row => row("sum") > 10 and row("sum") < 20)

  println(query)
  println(query.##)
}
