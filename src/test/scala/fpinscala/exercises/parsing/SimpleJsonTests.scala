package fpinscala.exercises.parsing

class SimpleJsonTests extends munit.FunSuite {

  val json = JSON.cpParser(SimpleParsers)
  import SimpleParsers.*
  import JSON.*

  test("parse null") {
    assertEquals(json.run("null"), Right(JNull))
  }

  test("parse boolean true") {
    assertEquals(json.run("true"), Right(JBool(true)))
  }

  test("parse boolean false") {
    assertEquals(json.run("false"), Right(JBool(false)))
  }

}
