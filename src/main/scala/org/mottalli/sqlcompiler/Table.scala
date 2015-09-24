package org.mottalli.sqlcompiler

case class Column(name: String)

case class Table(name: String) {
  def scan = ScanNode(this)
}

