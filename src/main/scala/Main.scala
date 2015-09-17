trait Constraint
case class GreaterThanConstraint(lhs: Value, rhs: Value) extends Constraint

trait Value {
  def >(other: Value) = GreaterThanConstraint(this, other)
  def sum = SumAggregation(this)
  def as(alias: String) = AliasedValue(this, alias)
}

case class SumAggregation(value: Value) extends Value
case class AliasedValue(value: Value, alias: String) extends Value

class Column

case class ColumnValue(column: String) extends Value
case class ConstantValue(constant: Int) extends Value

trait DataSource {
}

trait QueryNode {
  def producedValues: Map[String, Value]

  def apply(valueName: String): Value = producedValues(valueName)

  def scan(dataSource: DataSource) = ScanNode(dataSource)
  def filter(constraintFunc: QueryNode => Constraint) = ConstraintNode(this, constraintFunc(this))
  def map(mapFunc: QueryNode => Seq[Value]) = MapNode(this, mapFunc(this))
  //def groupBy(groupFunc: QueryNode => Seq[Value]) = GroupNode(this, groupFunc(this))
  def groupBy(groupFunc: QueryNode => Value) = GroupNode(this, Seq(groupFunc(this)))
}

trait SingleChildNode extends QueryNode {
  val childNode: QueryNode
  def producedValues = childNode.producedValues
}

case class ScanNode(source: DataSource) extends QueryNode {
  def producedValues: Map[String, Value] = Map(
    "foo" -> ColumnValue("foo"),
    "bar" -> ColumnValue("bar")
  )
}

case class ConstraintNode(childNode: QueryNode, constraint: Constraint) extends SingleChildNode
case class GroupNode(childNode: QueryNode, groupValues: Seq[Value]) extends SingleChildNode
case class MapNode(childNode: QueryNode, groupValues: Seq[Value]) extends SingleChildNode

object Main extends App {
  val query = ScanNode(new DataSource {})
    .filter(_("foo") > ConstantValue(3))
    .map(r => Seq(r("bar").sum as "sum"))
    .groupBy(_("foo"))

  println(query)
}
