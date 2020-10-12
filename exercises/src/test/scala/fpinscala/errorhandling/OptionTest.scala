package fpinscala.errorhandling

import org.scalatest.FlatSpec

class OptionTest extends FlatSpec {

  val intOp: Option[Int] = Some(1)
  val emptyOp: Option[Int] = None

  "Method `map`" should "apply provided function on Some and return Some" in {
    assert(intOp.map(_ * 2) == Some(2))
  }

  "Method `map`" should "apply provided function on None and return None" in {
    assert(emptyOp.map(_ * 2) == None)
  }

  "Method `getOrElse`" should "return Option value" in {
    assert(intOp.getOrElse(5) == 1)
  }

  "Method `getOrElse`" should "return default value" in {
    assert(emptyOp.getOrElse(5) == 5)
  }

  "Method `flatMap`" should "apply provided on Some and return Some" in {
    assert(intOp.flatMap(x => Some(x * 2)) == Some(2))
  }

  "Method `flatMap`" should "apply provided on None and return None" in {
    assert(emptyOp.flatMap(x => Some(x * 2)) == None)
  }

  "Method `filter`" should "return Option of value if value satisfies provided condition" in {
    assert(intOp.filter(_ < 2) == Some(1))
  }

  "Method `filter`" should "return Option of none if value does not satisfies provided condition" in {
    assert(intOp.filter(_ > 2) == None)
  }

  "Method `filter`" should "return Option of none if option is None" in {
    assert(emptyOp.filter(_ > 2) == None)
  }

  "Method `map2`" should "combines two Some values using a binary function" in {
    assert(Option.map2(intOp, intOp)(_ + _) == Some(2))
  }

  "Method `map2`" should "combines Some and None value using a binary function" in {
    assert(Option.map2(intOp, emptyOp)(_ + _) == None)
    assert(Option.map2(emptyOp, emptyOp)(_ + _) == None)
  }

  "Method `sequence`" should "combines a list of Options into one Option contains a list of all the Some values" in {
    assert(Option.sequence(List(intOp, intOp, intOp)) == Some(List(1, 1, 1)))
    assert(Option.sequence(List(intOp, intOp, emptyOp)) == None)
    assert(Option.sequence(List(emptyOp, emptyOp, emptyOp)) == None)
  }

  "Method `traverse`" should "traverse the list and apply function on each element" in {
    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch {
        case e: Exception => None
      }

    assert(Option.traverse(List("1", "2", "3"))(x => Try {
      x.toInt
    }) == Some(List(1, 2, 3)))
    assert(Option.traverse(List("1", "a", "2"))(x => Try {
      x.toInt
    }) == None)
  }

}
