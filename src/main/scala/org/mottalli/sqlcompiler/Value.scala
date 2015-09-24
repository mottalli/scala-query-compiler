package org.mottalli.sqlcompiler

trait Value {
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

sealed trait AggregatedValue extends Value

case class SumAggregation(value: Value) extends AggregatedValue
case class CountAggregation(value: Value) extends AggregatedValue
case class ColumnValue(column: Column) extends Value

case class NullValue() extends Value
case class AllValues() extends Value
case class ConstantValue[T <: AnyVal](constant: T) extends Value
