package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamTest extends FlatSpec {

  val intStream: Stream[Int] = Stream(1, 2, 3)
  val emptyStream: Stream[Int] = Stream()

  "Method `toList`" should "transform stream to list" in {
    assert(intStream.toList == List(1, 2, 3))
  }

  "Method `toList`" should "transform empty stream to empty list" in {
    assert(emptyStream.toList == List())
  }

  "Method `take`" should "return first n elements of stream" in {
    assert(intStream.take(2).toList == List(1, 2))
  }

  "Method `take`" should "return whole stream if n is bigger the size of stream" in {
    assert(intStream.take(10).toList == List(1, 2, 3))
  }

  "Method `drop`" should "drop n first elements of stream" in {
    assert(intStream.drop(1).toList == List(2, 3))
  }

  "Method `drop`" should "drop all elements of stream if n is bigger then size of stream" in {
    assert(intStream.drop(10).toList == List())
  }

  "Method `takeWhile`" should "return elements until condition is true" in {
    assert(intStream.takeWhile(x => x <= 2).toList == List(1, 2))
  }

  "Method `takeWhile`" should "return empty stream if condition is never true" in {
    assert(intStream.takeWhile(x => x < 0).toList == List())
  }

  "Method `forAll`" should "return true if all elements in the Stream match a given predicate" in {
    assert(intStream.forAll(x => x < 5))
  }

  "Method `forAll`" should "return true if at least one element in the Stream does not match a given predicate" in {
    assert(!intStream.forAll(x => x < 2))
  }

  "Method `takeWhileViaFoldRight`" should "return elements until condition is true" in {
    assert(intStream.takeWhileViaFoldRight(x => x <= 2).toList == List(1, 2))
  }

  "Method `takeWhileViaFoldRight`" should "return empty stream if condition is never true" in {
    assert(intStream.takeWhileViaFoldRight(x => x < 0).toList == List())
  }

  "Method `headOption`" should "return option of stream first element" in {
    assert(intStream.headOption.contains(1))
    assert(emptyStream.headOption.isEmpty)
  }

  "Method `map`" should "apply function to every element in stream" in {
    assert(intStream.map(x => x + 1).toList == List(2, 3, 4))
  }

  "Method `filter`" should "return elements of stream which satisfy condition" in {
    assert(intStream.filter(x => x % 2 != 0).toList == List(1, 3))
  }

  "Method `append`" should "add stream in parameter at the end of of original stream" in {
    val newSteam = Stream(4, 5, 6)
    assert(intStream.append(newSteam).toList == List(1, 2, 3, 4, 5, 6))
  }

  "Method `flatMap`" should "apply function to every element in stream" in {
    assert(intStream.flatMap(x => Stream(x + 1)).toList == List(2, 3, 4))
  }

  "Method `constant`" should "generate infinite stream of a given value" in {
    assert(Stream.constant(5).take(10).toList == List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  }

  "Method `from`" should "generate infinite stream of starting from n, then n + 1, ..." in {
    assert(Stream.from(0).take(10).toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "Method `fib" should "generate infinite stream of Fibonacci numbers" in {
    assert(Stream.fib().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "Method `unfold`" should "generate stream by applying provided function on initial state" in {
    assert(Stream.unfold(1)(x => Some((x, x + 1))).take(3).toList == List(1, 2, 3))
  }

  "Method `onesViaUnfold`" should "generate infinite stream of 1" in {
    assert(Stream.onesViaUnfold().take(10).toList == List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  }

  "Method `constantViaUnfold`" should "generate infinite stream of a given value" in {
    assert(Stream.constantViaUnfold(2).take(10).toList == List(2, 2, 2, 2, 2, 2, 2, 2, 2, 2))
  }

  "Method `fromViaUnfold`" should "generate infinite stream of starting from n, then n + 1, ..." in {
    assert(Stream.fromViaUnfold(0).take(10).toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "Method `fibViaUnfold" should "generate infinite stream of Fibonacci numbers" in {
    assert(Stream.fibViaUnfold().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "Method `mapViaUnfold`" should "apply function to every element in stream" in {
    assert(intStream.mapViaUnfold(x => x + 1).toList == List(2, 3, 4))
  }

  "Method `takeViaUnfold`" should "return first n elements of stream" in {
    assert(intStream.takeViaUnfold(2).toList == List(1, 2))
  }

  "Method `takeWhileViaUnfold`" should "return elements until condition is true" in {
    assert(intStream.takeWhileViaUnfold(x => x <= 2).toList == List(1, 2))
  }

  "Method `zipWith`" should "generate stream combining elements of two streams" in {
    val s: Stream[Int] = Stream(10, 20, 30, 40, 50)
    assert(intStream.zipWith(s)((x, y) => x + y).toList == List(11, 22, 33))
    assert(intStream.zipWith(emptyStream)((x, y) => x + y).toList == List())
  }

  "Method `zipAll`" should "generate stream combining elements of two streams even if one is empty" in {
    val s: Stream[Int] = Stream(10, 20, 30, 40, 50)
    assert(intStream.zipAll(s).toList ==
      List((Some(1), Some(10)), (Some(2), Some(20)), (Some(3), Some(30)), (None, Some(40)), (None, Some(50))))

    assert(intStream.zipAll(emptyStream).toList == List((Some(1), None), (Some(2), None), (Some(3), None)))
  }

  "Method `startsWith`" should "verify if one stream is prefix of the other" in {
    val s: Stream[Int] = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(s.startsWith(intStream))
    assert(!s.startsWith(emptyStream))
  }

  "Method `tails`" should "return stream of suffixes of the input sequence" in {
    val result: List[List[Int]] = intStream.tails.toList.map(s => s.toList)
    assert(result == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }
}