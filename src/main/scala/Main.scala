import org.mottalli.sqlcompiler._

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
  val queryPlan = query.createPlan()
}
