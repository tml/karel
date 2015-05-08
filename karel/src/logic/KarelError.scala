package logic

case class KarelError(msg: String) extends Exception {
  override def toString = msg
}
