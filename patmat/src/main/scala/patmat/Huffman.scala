package patmat

import scala.collection.immutable.Map

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case f: Fork => f.weight
    case l: Leaf => l.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case f: Fork => f.chars
    case l: Leaf => l.char :: Nil
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    * val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    * val theChar = pair._1
    * val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    * pair match {
    * case (theChar, theInt) =>
    * println("character is: "+ theChar)
    * println("integer is  : "+ theInt)
    * }
    */

  def times(chars: List[Char]): List[(Char, Int)] =
    chars.foldLeft(Map[Char, Int]() withDefaultValue 0)((m, c) => m + (c -> (m(c) + 1))).toList

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs.map(pair => new Leaf(pair._1, pair._2)).sortBy(leaf => leaf.weight)

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: Nil => true
    case _ => false
  }

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (trees.length < 2) trees
    (makeCodeTree(trees(0), trees(1)) :: trees.takeRight(trees.length - 2)) sortBy (t => weight(t))
  }

  /**
    * This function will be called in the following way:
    *
    * until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the list of
    * code trees contains only one single tree, and then return that singleton list.
    *
    * Hint: before writing the implementation,
    *  - start by defining the parameter types such that the above example invocation
    * is valid. The parameter types of `until` should match the argument types of
    * the example invocation. Also define the return type of the `until` function.
    *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */
  def until[A](endstate: List[A] => Boolean, op: List[A] => List[A])(seq: List[A]): List[A] = {
    if (endstate(seq)) seq
    else until(endstate, op)(op(seq))
  }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val leaves = makeOrderedLeafList(times(chars))
    until(singleton, combine)(leaves).head
  }


  // Part 3: Decoding

  type Bit = Int

  // this is mine -- traverse list until leaf is found -- return char and unused portion of bit list
  def traverse(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = {
//    println("called with ", tree, bits)
    tree match {
      case l: Leaf => (l.char, bits)
      case f: Fork => bits match {
        case Nil => throw new Exception("wtf")
        case 0 :: tail => traverse(f.left, tail)
        case 1 :: tail => traverse(f.right, tail)
      }
    }
  }

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char]  = {
    def iter(tree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
      val (char, tail) = traverse(tree, bits)
      tail match {
        case Nil => (char :: acc).reverse
        case l: List[Bit] => iter(tree, l, char :: acc)
      }
    }
    iter(tree, bits, List())
  }

  // this doesn't work
  //  def decode(orgTree: CodeTree, bits: List[Bit]): List[Char] = {
  //    def iter(tree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
  //      println("bits: ", bits)
  //      println("acc: ", acc)
  //      bits match {
  //        case Nil => { println("match nil"); acc.reverse }
  //        case b :: tail => tree match {
  //          case l: Leaf => { println("match leaf"); iter(orgTree, bits, l.char :: acc) }
  //          case f: Fork => { println("match fork"); if (b == 0) iter(f.left, tail,  acc) else iter(f.right, tail, acc) }
  //        }
  //      }
  //    }
  //    iter(orgTree, bits, List())
  //  }
  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the `frenchCode' Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
//  decode(tree: CodeTree, bits: List[Bit]): List[Char]
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree


  // this is mine: separate function just for traversing the tree until letter
  // is found; return List[Bit]
  def find(tree: CodeTree, c: Char): List[Bit] = {
    def iter(tree: CodeTree, acc: List[Bit]): List[Bit] = tree match {
      case l: Leaf => if (l.char == c) acc.reverse else Nil
      case f: Fork => if (f.chars.contains(c)) iter(f.left, 0 :: acc) ::: iter(f.right, 1 :: acc) else Nil
    }
    iter(tree, List())
  }

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text.foldLeft(List[Bit]())((acc: List[Bit], c: Char) => acc ::: find(tree, c))
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(_._1 == char).get._2

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = {
    def iter(tree: CodeTree, acc: List[Bit]): CodeTable = tree match {
      case l: Leaf => ( l.char, acc.reverse ) :: Nil
      case f: Fork => iter(f.left, 0 :: acc) ::: iter(f.right, 1 :: acc)
    }
    iter(tree, List())
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  //     text.foldLeft(List[Bit]())((acc: List[Bit], c: Char) => acc ::: find(tree, c))
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    println("table: ", table)
    text.flatMap(c => codeBits(table)(c))
  }
}

/*
Your overall score for this assignment is 8.36 out of 10.00


The code you submitted did not pass all of our tests: your submission achieved a score of
6.36 out of 8.00 in our tests.

In order to find bugs in your code, we advise to perform the following steps:
 - Take a close look at the test output that you can find below: it should point you to
   the part of your code that has bugs.
 - Run the tests that we provide with the handout on your code.
 - The tests we provide do not test your code in depth: they are very incomplete. In order
   to test more aspects of your code, write your own unit tests.
 - Take another very careful look at the assignment description. Try to find out if you
   misunderstood parts of it. While reading through the assignment, write more tests.

Below you can find a short feedback for every individual test that failed.

Our automated style checker tool could not find any issues with your code. You obtained the maximal
style score of 2.00.

======== LOG OF FAILED TESTS ========
Your solution achieved a testing score of 214 out of 269.

Below you can see a short feedback for every test that failed,
indicating the reason for the test failure and how many points
you lost for each individual test.

Tests that were aborted took too long too complete or crashed the
JVM. Such crashes can arise due to infinite non-terminating
loops or recursion (StackOverflowException) or excessive memory
consumption (OutOfMemoryException).

[Test Description] quick encode gives the correct byte sequence
[Observed Error] List(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0) did not equal List(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1)
[Lost Points] 20

[Test Description] convert: code table is created correctly
[Observed Error] List((a,List(0, 0)), (b,List(1, 0)), (d,List(1))) did not equal List((a,List(0, 0)), (b,List(0, 1)), (d,List(1)))
[Lost Points] 20

[Test Description] combine of a singleton or nil
[Observed Error] 1
[exception was thrown] detailed error message in debug output section below
[Lost Points] 5

[Test Description] decode and quick encode is identity
[Observed Error] List(' ', 'u', 'u', ',', ' ', '0', 'r', 'e', 's', ' ', 's', 'c', 's', ',', 'a', ' ', 'i', 'b', 'e', ' ', 'c', 'n', 'a', 'i', 'i', 'y', ',', 'b', 't', 's', 'i', 'w', 'r', ' ', 'f', 'c', 'l', 'm', 'a', 'l', ' ', 'k', 'd', 'd', ' ', 's', 'r', 'l', 'r', 'a', 'n', 't', ' ', 't', '0', 'i', 'h', 'n', 'r', 's', 't', 'm', 'e', 'm') did not equal List('t', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.', ' ', 'R', 'i', 'c', 'h', 'a', 'r', 'd', ' ', 'M', 'c')
[Lost Points] 10

======== TESTING ENVIRONMENT ========
Limits: memory: 256m,  total time: 850s,  per test case time: 240s

======== DEBUG OUTPUT OF TESTING TOOL ========
[test failure log] test name: HuffmanSuite::combine of a singleton or nil::5
java.lang.IndexOutOfBoundsException: 1
scala.collection.LinearSeqOptimized$class.apply(LinearSeqOptimized.scala:65)
scala.collection.immutable.List.apply(List.scala:84)
patmat.Huffman$.combine(Huffman.scala:115)
patmat.HuffmanSuite$$anonfun$15.apply$mcV$sp(HuffmanSuite.scala:138)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:124)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:122)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:122)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
 */