package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {
  val testTree =
    Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(7)),
        Branch(
          Leaf(378),
          Leaf(35))
      ),
      Leaf(2)
    )

  "Method `size`" should "return number of nodes (leaves and branches) in a tree" in {
    assert(Tree.size(testTree) == 9)
  }

  "Method `maximum`" should "return maximum element in Tree[Int]" in {
    assert(Tree.maximum(testTree) == 378)
  }

  "Method `depth`" should "return maximum path length from the root of a tree to any leaf" in {
    assert(Tree.depth(testTree) == 4)
  }

  "Method `map`" should "apply provided function to all elements in a list" in {
    assert(Tree.map(testTree)(x => x*2) ==
      Branch(Branch(Branch(Leaf(2),Leaf(14)),Branch(Leaf(756),Leaf(70))),Leaf(4)))
  }

  "Method `sizeViaFold`" should "return number of nodes (leaves and branches) in a tree" in {
    assert(Tree.sizeViaFold(testTree) == 9)
  }

  "Method `maximumViaFold`" should "return maximum element in Tree[Int]" in {
    assert(Tree.maximumViaFold(testTree) == 378)
  }

  "Method `depthViaFold`" should "return maximum path length from the root of a tree to any leaf" in {
    assert(Tree.fold(testTree)(_ => 1)((x, y) => 1 + (x max y)) == 4)
  }

  "Method `mapViaFold`" should "apply provided function to all elements in a list" in {
    assert(Tree.mapViaFold(testTree)(x => x*2) ==
      Branch(Branch(Branch(Leaf(2),Leaf(14)),Branch(Leaf(756),Leaf(70))),Leaf(4)))
  }
}
