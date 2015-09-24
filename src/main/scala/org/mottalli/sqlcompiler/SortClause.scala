package org.mottalli.sqlcompiler

object SortOrder extends Enumeration {
  type SortOrder = Value
  val ASC, DESC = Value
}

case class SortClause(value: Value, order: SortOrder.SortOrder)

