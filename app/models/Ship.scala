package models

case class Ship (s: Int) {
  val size: Int = s
  var emptyOrigin: Array[Int] = Array(-1, -1)
  var emptyOrientation: String = null
}
