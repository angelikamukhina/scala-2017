import org.scalatest.FunSuite
import ru.spbau.scala.MultiSet._

class MultiSetTest extends FunSuite {
  test("testAddElement") {
    val multiset = EmptyMultiSet
    assert(multiset.apply(3).find(3).get == 3)
    assert(multiset.apply(1).apply(3).apply(3).getCount(3) == 2)
  }

  test("testFindElement") {
    val multiSetTest = EmptyMultiSet
    assert(multiSetTest.apply(3).find(3).get == 3)
    assert(multiSetTest.apply(3).find(2).isEmpty)
  }

  test("testForComprehension") {
    val multiSet = MultiSet.apply(1, 2, 3, 4, 4, 5)
    var sum = 0
    for (elem <- multiSet) {
      sum += elem
    }
    assert(sum == 19)
  }

  test("testUnionWithEmptyMultiSet") {
    val multiset1 = EmptyMultiSet.apply(1).apply(2).apply(3)
    val emptySet1 = EmptyMultiSet
    val union = multiset1 | emptySet1
    assert(union.find(3).get == 3)
    assert((emptySet1 | multiset1).find(3).get == 3)
  }

  test("testUnionOfNotEmptyMultiSets") {
    val multiset1 = MultiSet.apply(1, 2, 3)
    val multiset2 = MultiSet.apply(1, 2, 3)
    val union = multiset1 | multiset2
    assert(union.getCount(2) == 2)
  }

  test("testIntersectionOfMultiSets") {
    val multiset1 = MultiSet.apply(1, 2, 3)
    val multiset2 = MultiSet.apply(1, 2, 3)
    val intersection1 = multiset1 & multiset2
    assert(intersection1.getCount(1) == 1)

    val multiset3 = MultiSet.apply(1, 2, 3, 4, 5)
    val multiset4 = MultiSet.apply(2, 3, 4)
    val intersection2 = multiset3 & multiset4
    assert(intersection2.getCount(1) == 0)
  }

  test("testPatternMatching") {
    val multiset = MultiSet.apply(1, 2, 3, 4, 5)
    assertCompiles("multiset match { " +
      "case NotEmptyMultiSet(_, _, _) => " +
      "case EmptyMultiSet => " +
      "}")
  }
}
