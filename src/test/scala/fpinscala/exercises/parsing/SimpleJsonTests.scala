package fpinscala.exercises.parsing

class SimpleJsonTests extends munit.FunSuite {

//  val json = JSON.cpParser(SimpleParsers)
  import SimpleParsers.*
  import JSON.*

  // test("parse null") {
  //   assertEquals(json.run("null"), Success(JNull, ""))
  // }

  // test("parse boolean true") {
  //   assertEquals(json.run("true"), Success(JBool(true), ""))
  // }

  // test("parse boolean false") {
  //   assertEquals(json.run("false"), Success(JBool(false), ""))
  // }

  test("parse a verbatim string"){
    val parser = string("blabla")
    val input = "not blabla"
    val res = parser.run(input)
    assert(res.isInstanceOf[Error])
  }

}
