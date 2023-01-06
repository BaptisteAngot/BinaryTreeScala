import org.scalatest.flatspec.AnyFlatSpec

class TreeTests extends AnyFlatSpec{
  val tree: Tree[Int] = Branch(Leaf(1), Leaf(2))

  "size" should "return the number of nodes in the tree" in {
    assert(Tree.size(tree) == 3)
  }

  "maximum" should "return the maximum value in the tree" in {
    assert(Tree.maximum(tree) == 2)
  }

  "minimum" should "return the minimum value in the tree" in {
    assert(Tree.minimum(tree) == 1)
  }

  "depth" should "return the maximum path length from the root of a tree to any leaf" in {
    assert(Tree.depth(tree) == 1)
  }

  "map" should "transform the values in the tree using a given function" in {
    val tree2 = Tree.map(tree)(_ * 2)
    assert(Tree.size(tree2) == 3)
    assert(Tree.maximum(tree2) == 4)
    assert(Tree.depth(tree2) == 1)
  }

  "flatMap" should "transform the values in the tree using a given function" in {
    val tree2 = Tree.flatMap(tree)(n => Leaf(n * 2))
    assert(Tree.size(tree2) == 3)
    assert(Tree.maximum(tree2) == 4)
    assert(Tree.depth(tree2) == 1)
  }

  "search" should "return the first value that satisfies the given predicate" in {
    val p = (n: Int) => n % 2 == 0
    assert(Tree.search(tree, p).contains(2))
  }

  "isLeaf" should "return true if the node is a leaf, false otherwise" in {
    assert(Tree.isLeaf(Leaf(1)))
    assert(!Tree.isLeaf(Branch(Leaf(1), Leaf(2))))
  }

  "foldRight" should "fold the values in the tree using a given function" in {
    val sum = Tree.foldRight(tree, 0)(_ + _)
    assert(sum == 3)
  }

  "foldLeft" should "fold the values in the tree using a given function" in {
    val sum = Tree.foldLeft(tree, 0)(_ + _)
    assert(sum == 3)
  }
}
