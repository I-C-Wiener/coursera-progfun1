package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Fork(Leaf('A', 8), Fork(Fork(Leaf('B', 3), Fork(Leaf('C', 1), Leaf('D', 1), List('C','D'), 2), List('B','C','D'), 5), Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E','F'), 2), Fork(Leaf('G', 1), Leaf('H', 1), List('G', 'H'), 2), List('E','F','G','H'), 4), List('B','C','D','E','F','G','H'), 9), List('A','B','C','D','E','F','G','H'), 17)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times works") {
    assert(times(List('h', 'e', 'l', 'l')).contains(('h', 1)))
    assert(times(List('h', 'e', 'l', 'l')).contains(('e', 1)))
    assert(times(List('h', 'e', 'l', 'l')).contains(('l', 2)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode a text") {
    new TestTrees {
      assert(decode(t3, List(1,0,0,0,1,0,1,0)) === "BAC".toList)
    }
  }

  test("encode a text") {
    new TestTrees {
      assert(encode(t3)(List('B','A','C')) === List(1,0,0,0,1,0,1,0))
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0,0)), ('b', List(0,1)), ('d', List(1))))
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
