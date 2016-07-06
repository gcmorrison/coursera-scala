object Recap1 {

  // Recap
  abstract class JSON
  case class JSeq(elems: List[JSON]) extends JSON
  case class JObj(bindings: Map[String, JSON]) extends JSON
  case class JNum(num: Double) extends JSON
  case class JStr(str: String) extends JSON
  case class JBool(b: Boolean) extends JSON
  case object JNull extends JSON

  def show(json: JSON): String = json match {
    case JSeq(elems) => "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\": " + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => '\"' + str + '\"'
    case JBool(b) => b.toString
    case JNull => "null"
  }

  show (new JObj(Map(("hello", new JStr("world")))))

  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd Street"),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"),
        "number" -> JStr("212 555-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"),
        "number" -> JStr("646 555-4567")
      ))
    ))
  ))
  show(data)

  // Partial Functions
  val f: PartialFunction[String, String] = { case "ping" => "pong"}
  f("ping")
  f.isDefinedAt("ping")
  f.isDefinedAt("abc")

  // Partial Function Quiz1
  val fQ: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }
  fQ.isDefinedAt(List(1, 2))
  fQ.isDefinedAt(List(1, 2, 3))

  // Partial Function Quiz2
  val fQ2: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest => rest match {
      case Nil => "two"
    }
  }
  fQ2.isDefinedAt(List(1, 2, 3)) // isDefinedAt only applies to outer-most block

}
