package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  //def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = ???

  def cpParser[Parser[+_]](P: CpParsers[Parser]): Parser[JSON] = {
    import P.*

    val bool: Parser[JSON] = (string("false").map(_ => JBool(false)) | string("true").map(_ => JBool(true))).token

    val num: Parser[JSON] = regex("\\d+".r).flatMap{numStr =>
      try succeed(JNumber(numStr.toDouble))
      catch case ex: Throwable =>
        fail(s"Failed to parse $numStr as a double ${ex.getMessage}")
    }.token

    def jsval = bool | num | obj | array

    def array: Parser[JSON] = (string("[").token *> jsval.seqWithSep(",") <* string("]")).map(arr => JArray(arr)).token

    def obj: Parser[JSON] = ???
    ???
  }