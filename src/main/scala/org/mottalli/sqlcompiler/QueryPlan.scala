package org.mottalli.sqlcompiler

case class QueryPlan(rootNode: QueryPlanNode)

object QueryPlan {
  def createPlanForQuery(query: SelectNode): QueryPlan = {
    ???
  }

  /** Resolve the named values to their actual values.
    * E.g. translates the string "table.column" to the actual ColumnValue
    */
  private def resolveNamedValues(node: QueryNode): QueryNode = {
    ???
  }
}
