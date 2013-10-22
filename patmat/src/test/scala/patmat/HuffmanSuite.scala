package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times Nil") {
    assert(times(Nil) === Nil)
  }

  test("times abacab") {
    assert(times('a' :: 'b' :: 'a' :: 'c' :: 'a' :: 'b' :: Nil) === List(('b', 2), ('a', 3), ('c', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decodedSecret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("encode") {
    assert(encode(frenchCode)("huffmanestcool".toList) === secret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val t = ('a', List(0, 0, 0)) :: ('b', List(0, 0, 1)) :: ('c', List(0, 1, 0)) :: Nil
    assert(codeBits(t)('a') === List(0, 0, 0))
    assert(codeBits(t)('b') === List(0, 0, 1))
    assert(codeBits(t)('c') === List(0, 1, 0))
  }

  test("convert") {
    val t: CodeTree = Fork(Leaf('a', 1), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), List('a', 'e', 't'), 4)
    println(convert(t))
  }
}
