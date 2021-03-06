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


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      println("encoded: ", encoded)
      val decoded = decode(t1, encoded)
      println("decoded: ", decoded)
      assert(decoded === "ab".toList)
    }
  }

  test("times") {
    assert(times(List('a','b','c', 'a')) == List(('a', 2), ('b', 1), ('c', 1)))
  }

  test("combine") {
    val tree = List(new Leaf('a', 1), new Leaf('b', 1), new Leaf('c', 1), new Leaf('d', 4))
    assert(combine(tree) == List(new Leaf('c', 1), new Fork(new Leaf('a', 1), new Leaf('b', 1), List('a','b'), 2), new Leaf('d', 4)))
  }

  test("until") {
    val tree = List(new Leaf('a', 1), new Leaf('b', 1), new Leaf('c', 1), new Leaf('d', 4))
    println(until(singleton, combine)(tree))
  }

  test("createCodeTree") {
    println(createCodeTree("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabc".toList))
  }

  test("find") {
    new TestTrees {
      assert(find(t2, 'a') === List(0,0))
      assert(find(t2, 'b') === List(0,1))
      assert(find(t2, 'd') === List(1))
    }
  }

  test("traverse") {
    new TestTrees {
      val (char, bits) = traverse(t1, List(0, 1))
      assert(char === 'a')
      assert(bits === List(1))
    }
  }

  test("decode") {
   new TestTrees {
     val decoded = decode(t1, List(0,1,0,1))
     println("decoded: ", decoded)
   }
  }

  test("decodesecret") {
    println(decodedSecret)
  }

  test("convert") {
    new TestTrees {
      val table = convert(t1)
      assert(table == List(('a', List[Bit](0)), ('b', List[Bit](1))))
    }
  }

  test("quickEncode") {
    new TestTrees {
      val actual = quickEncode(t2)(List('b','a','d','d','a','d'))
      val expected = List(0,1,0,0,1,1,0,0,1)
      assert(actual == expected)
    }
  }
}
