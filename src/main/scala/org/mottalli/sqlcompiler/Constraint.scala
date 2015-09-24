package org.mottalli.sqlcompiler

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

