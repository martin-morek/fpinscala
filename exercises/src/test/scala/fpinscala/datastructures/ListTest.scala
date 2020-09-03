package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListTest extends FlatSpec{

  var emptyList: List[Int] = List()
  var intList: List[Int] = List(1,2,3,4,5,6)
  var doubleList: List[Double] = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)

  "Method `tail`" should "return original list without first element" in {
    assert(List.tail(intList) == List(2,3,4,5,6))
    assert(List.tail(emptyList) == List())
  }

  "Method `setHead`" should "replace list first element with provided element" in {
    assert(List.setHead(intList,5) == List(5,2,3,4,5,6))
    assert(List.setHead(emptyList,1) == List(1))
  }

  "Method `drop`" should "remove n first elements from list" in {
    assert(List.drop(intList,3) == List(4,5,6))
    assert(List.drop(intList,10) == List())
    assert(List.drop(intList,0) == List(1,2,3,4,5,6))
  }

  "Method `dropWhile`" should "remove elements from list until condition if true" in {
    assert(List.dropWhile(intList)(x => x < 4) == List(4,5,6))
    assert(List.dropWhile(intList)(x => x < 10) == List())
    assert(List.dropWhile(intList)(x => x < 0) == List(1,2,3,4,5,6))
    assert(List.dropWhile(emptyList)(x => x < 8) == List())
  }

  "Method `init`" should "return list without last element" in {
    assert(List.init(intList) == List(1,2,3,4,5))
    assert(List.init(emptyList) == List())
  }

  "Method `length`" should "return length of list" in {
    assert(List.length(intList) == 6)
    assert(List.length(emptyList) == 0)
  }

  "Method `sumWithFoldLeft`" should "return sum of list elements" in {
    assert(List.sumWithFoldLeft(intList) == 21)
    assert(List.sumWithFoldLeft(emptyList) == 0)
  }

  "Method `productWithFoldLeft`" should "return product of list elements" in {
    assert(List.productWithFoldLeft(doubleList) == 720.0)
  }

  "Method `lengthWithFoldLeft`" should "return length of list" in {
    assert(List.lengthWithFoldLeft(intList) == 6)
    assert(List.lengthWithFoldLeft(emptyList) == 0)
  }

  "Method `reverse`" should "return list with elements in reverse order" in {
    assert(List.reverse(intList).equals(List(6,5,4,3,2,1)))
    assert(List.reverse(emptyList).equals(List()))
  }

  "Method `appendWithFoldRight`" should "append second list at the end of the first one" in {
    assert(List.appendWithFoldRight(intList, List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(List.appendWithFoldRight(intList, List()) == List(1,2,3,4,5,6))
  }

  "Method `concat`" should "concatenates a list of lists into a single list" in {
    val l = List(List(1,2,3),List(4,5,6),List(7,8,9))
    assert(List.concat(l) == List(1,2,3,4,5,6,7,8,9))
  }

  "Method `addOne`" should "transform a list of integers by adding 1 to each element" in {
    assert(List.addOne(intList) == List(2,3,4,5,6,7))
  }

  "Method 'doubleToString'" should "turns each value in a Double list in to a String" in {
    assert(List.doubleToString(doubleList) == List("1.0", "2.0", "3.0", "4.0", "5.0", "6.0"))
  }

  "Method `map`" should " apply provided function on each element of a list" in {
    assert(List.map(intList)(x => x*2) == List(2,4,6,8,10,12))
  }

  "Method `filter`" should "filter out list elements which doesn't satisfy given predicate" in {
    assert(List.filter(intList)(_ % 2 == 0) == List(2,4,6))
  }

  "Method `flatMap`" should "apply provided function on each element of a list and flatten result" in {
    assert(List.flatMap(intList)(i => List(i,i)) == List(1,1,2,2,3,3,4,4,5,5,6,6))
  }

  "Method `filterWithFlatMap`" should "filter out list elements which doesn't satisfy given predicate" in {
    assert(List.filterWithFlatMap(intList)(_ % 2 == 0) == List(2,4,6))
  }

  "Method `addLists`" should "add corresponding elements in lists" in {
    assert(List.addLists(intList, intList) == List(2,4,6,8,10,12))
  }

  "Method `zipWith`" should "zip two lists with provided function" in {
    assert(List.zipWith(doubleList, intList)((x, y) =>  x +"-"+ y) == List("1.0-1", "2.0-2", "3.0-3", "4.0-4", "5.0-5", "6.0-6"))
  }
}
