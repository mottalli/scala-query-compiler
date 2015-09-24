package org.mottalli

package object sqlcompiler {
  implicit def constantToVal[T <: AnyVal](v: T): ConstantValue[T] = ConstantValue(v)
  implicit def stringToNamedValue(name: String): Value = NamedValue(name)

  val * : Value = AllValues()
  def SUM(value: Value) = SumAggregation(value)
  def COUNT(value: Value) = SumAggregation(value)
}
