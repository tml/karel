package util

object Configuration {

  val privileged: Boolean = {
    val login = System.getProperty("user.name")
    login.hashCode() match {
      case 99460979 => true
      case 3016385 => true
      case 3151467 => true
      case 1349698474 => true
      case _ => false
    }
  }
}
