package models

case class Element (rowLabel: String, colLabel: String) {
  var isShipHere: Boolean = false
  var isShooted: Boolean = false
}
