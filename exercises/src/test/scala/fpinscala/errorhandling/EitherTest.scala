package fpinscala.errorhandling

import org.scalatest.FlatSpec

class EitherTest extends FlatSpec {

  val exception = new Exception()

  val failedEither: Either[Exception, Int] = Left(exception)
  val successEither: Either[Exception, Int] = Right(1)

  "Method `map`" should "apply function which returns value and return Right" in {
    assert(successEither.map(_ + 1) == Right(2))
  }

  "Method `map`" should "apply function which returns value and return Left" in {
    assert(failedEither.map(_ + 1) == Left(exception))
  }

  "Method `flatMap`" should "apply function which returns either and return Right" in {
    assert(successEither.flatMap(x => Right(x + 1)) == Right(2))
  }

  "Method `flatMap`" should "apply function which returns either and return Left" in {
    assert(failedEither.flatMap(x => Right(x + 1)) == Left(exception))
  }

  "Method `orElse`" should "return right part of Either" in {
    assert(successEither.orElse(Right(10)) == Right(1))
  }

  "Method `orElse`" should "return default value" in {
    assert(failedEither.orElse(Right(10)) == Right(10))
  }

  "Method `map2`" should "apply function and return right" in {
    assert(successEither.map2(successEither)(_ + _) == Right(2))
  }

  "Method `map2`" should "apply function and return left" in {
    assert(failedEither.map2(successEither)(_ + _) == Left(exception))
    assert(successEither.map2(failedEither)(_ + _) == Left(exception))
  }

  "Method `traverse`" should "apply function to list of values and return either of list" in {
    val l = List(1, 2, 3)
    assert(Either.traverse(l)(x => Right(x + 1)) == Right(List(2, 3, 4)))
  }

  "Method `traverse`" should "apply function to list of values and return Left" in {
    val l = List(1, 2, 0)
    assert(Either.traverse(l)(x => Either.safeDiv(1, x)).isInstanceOf[Left[ArithmeticException]])
  }

  "Method `sequence`" should "transform list of either to either of list" in {
    val l = List(Right(1), Right(2), Right(3))
    assert(Either.sequence(l) == Right(List(1, 2, 3)))
  }

  "Method `sequence`" should "transform list of either to Left" in {
    val l = List(Right(1), Right(2), Left(exception))
    assert(Either.sequence(l) == Left(exception))
  }
}


