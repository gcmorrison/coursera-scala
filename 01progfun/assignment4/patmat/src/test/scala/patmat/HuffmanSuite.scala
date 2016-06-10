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


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"a, b, a, a, c, a, b\")") {
    assert(times(List('a', 'b', 'a', 'a', 'c', 'a', 'b')).sorted === List(('a', 4), ('b', 2), ('c', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('e', 2), ('t', 1), ('x', 3))) === List(Leaf('t',1), Leaf('e',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until") {
    assert(createCodeTree(List('a', 'b', 'c', 'b', 'c', 'd', 'b')) === Fork(Fork(Fork(Leaf('a', 1), Leaf('d', 1), List('a', 'd'), 2), Leaf('c', 2), List('a', 'd', 'c'), 4), Leaf('b', 3), List('a', 'd', 'c', 'b'), 7))
  }

  test("decode only") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("encode only") {
    assert(secret === encode(frenchCode)("huffmanestcool".toList))
  }

  test("encode and then decode a very short text") {
    new TestTrees {
      private val codeTree: CodeTree = createCodeTree("abcbcdb".toList)
      assert(encode(codeTree)("abcd".toList) === List(0, 0, 0, 1, 0, 1, 0, 0, 1))
      assert(decode(codeTree, encode(codeTree)("abcd".toList)) === "abcd".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a very short text by CodeTable") {
    new TestTrees {
      private val codeTree: CodeTree = createCodeTree("abcbcdb".toList)
      assert(encode(codeTree)("abcd".toList) === List(0, 0, 0, 1, 0, 1, 0, 0, 1))

      assert(convert(codeTree) === List(('a', List(0, 0, 0)), ('d', List(0, 0, 1)), ('c', List(0, 1)), ('b', List(1))))

      assert(quickEncode(codeTree)("abcd".toList) === List(0, 0, 0, 1, 0, 1, 0, 0, 1))
      assert(decode(codeTree, quickEncode(codeTree)("abcd".toList)) === "abcd".toList)
    }
  }
}
