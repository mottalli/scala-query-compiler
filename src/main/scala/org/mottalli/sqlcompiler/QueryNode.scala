package org.mottalli.sqlcompiler

trait QueryNode {
  def filter(constraint: Constraint) = ConstraintNode(this, constraint)
  def aggregate(aggregatedValues: List[AggregatedValue]) = AggregateNode(this, aggregatedValues)
  def groupBy(groupValues: List[Value]) = GroupNode(this, groupValues)
  def orderBy(clauses: List[SortClause]) = SortNode(this, clauses)
  def select(values: List[Value]) = SelectNode(this, values)
  def project = select _

  def createPlan(): QueryPlan = QueryPlan.createPlanForQuery(this)
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
case class SortNode(childNode: QueryNode, sortBy: List[SortClause]) extends SingleChildNode
