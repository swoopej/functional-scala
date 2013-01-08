package patmat

import common._

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
  } // tree match ...

  def chars(tree: CodeTree): List[Char] = tree match {
    case f: Fork => f.chars
    case l: Leaf => List(l.char)
  } // tree match ...

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
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    val emptyList = List[(Char, Int)]()
    times0(chars, emptyList)
  }
  
  //helper function that takes an accumulator parameter in addition to the list of chars.  Returns a list of (Char, Int)
  //pairs indicating how many times each char occurs in the list 
  def times0(chars: List[Char], accu: List[(Char, Int)]): List[(Char, Int)] = {
    if (chars.isEmpty) accu
    else {
      val searchChar = chars.head
      
      //if the accumulator already contains the search character
      if (accu.contains(searchChar)) {
    	val emptyList = List[(Char, Int)]()
        copyList(chars.head, accu, emptyList)
      }
      
      //if we need to add the search character as a new element in the list
      else{
        val newTuple = (chars.head, 1)
        times0(chars.tail, newTuple :: accu)
      } 
    }
  }
  
  //function to create a new list with the integer incremented one in the pair that matches the search character 
  //(lists are immutable - can't just increment in the same list)
  //Function calls itself recursively with the tail of oldAccu until it is empty and all the elements
  //have been added to the newAccu
  def copyList(char: Char, oldAccu: List[(Char, Int)], newAccu: List[(Char, Int)]): List[(Char, Int)] = {
      if (oldAccu.isEmpty) newAccu
      else if (oldAccu.head._1 == char) {
        val newNum = oldAccu.head._2 + 1
        (char, newNum) :: newAccu
      } else {
        oldAccu.head :: newAccu
      }
      copyList(char, oldAccu.tail, newAccu) 
  }
  
 

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortBy(_._2).map(x => Leaf(x._1, x._2))
    
    //freqs.sortBy(x => (x._2, x)).reverse.map(x => Leaf(x._1, x._2))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    if (trees.length == 1) true else false
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
    if (trees.length < 3) trees
    else {
      val forkNode = makeCodeTree(trees.head, trees.tail.head)
      val newTrees = trees.drop(2)
      combine0(forkNode, newTrees)
    }
  }
   
  //helper function that takes in two Lists of code trees and inserts one into the other in sorted order based on weights (a into b)
  //first parameter needs to be a fork node 
  def combine0(forkNode: Fork, bigTrees: List[CodeTree]): List[CodeTree] = { 
    if (weight(forkNode) < weight(bigTrees.head)) {
      val newTree = forkNode :: bigTrees
      newTree
    } else {
      val newTree = bigTrees.head :: combine0(forkNode, bigTrees.tail)
      newTree
    }
  }
  	




  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singletonCheck: List[CodeTree] => Boolean, reducer: List[CodeTree] => List[CodeTree])(tree: List[CodeTree]): List[CodeTree] = {
    if (tree.isEmpty) tree
    else if (singletonCheck(tree)) tree
    else until(singleton, combine)(combine(tree))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
	  if (chars.isEmpty) null //throw an exception?
	  else{
	    val list = until(singleton, combine)(makeOrderedLeafList(times(chars)))
	    val tree = list.head
	    tree
	  }
  }
  
  /*
   * chars match {
   *case emp: chars.isEmpty => emp
    case sing: chars.length == 1 => sing
    case _: times(chars)
    */



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): (List[Char]) = {
    val decodedString = decodingAccumulator(tree, bits, List[Char]())
    decodedString
  }
  
  //Stores the decoded string in the chars argument on recursive calls.  Continues calling itself until
  //the bits argument is empty, then it returns the accumulated chars
  def decodingAccumulator(tree: CodeTree, bits: List[Bit], chars: List[Char]): List[Char] = {
    if (!bits.isEmpty){
      var newTuple = findLeafChar(tree, bits, List[Char]()) //return value from findLeafChar is (List[Char], List[Bit])
      decodingAccumulator(tree, newTuple._2, newTuple._1)  //recursively call with the the modified values from findLeafChar
    } else {
      chars
    }
  }
    
    
    //this function will take in the tree and traverse to a leaf.  When it reaches the leaf,
    //it adds the character at that leaf to the accumulator and returns the accumulator as well as a bit
    //sequence that has been shortened at the head to the current point
    def findLeafChar(tree: CodeTree, bits: List[Bit], accu: List[Char]): (List[Char], List[Bit]) = tree match { 
     
      case l: Leaf => { //if tree is a leaf node - the current char needs to be added to accu
        l.char :: accu //add current char to accu and call with top of tree 
        if (bits.isEmpty) (accu, bits)
        else (accu, bits.tail) //return newList with char added
      }
       //if tree is a fork node, recursive call findLeaf with the right or left subtree
      case f: Fork => {
        if (!bits.tail.isEmpty) {
          if (bits.head == 1) findLeafChar(f.left, bits.tail, accu) else findLeafChar(f.right, bits.tail, accu) 
        }
        else (accu, bits)
      }
    }
    
/*
 *       //if tree is a fork node, recursive call findLeaf with the right or left subtree
      case f: Fork => {
        if (!bits.tail.isEmpty) {
          if (bits.head == 1) findLeafChar(f.left, bits.tail, accu) else findLeafChar(f.right, bits.tail, accu) 
        }
        else (accu, bits)  
 */
  

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = {
    val message = decode(frenchCode, secret)
    println(message)
    message
  }



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    
    if(!text.isEmpty) {
      val t = findLeafBit(tree, text, List[Bit]())
      t
    } else {
      List[Bit]()
    }
  }
  
  //use pattern matching to find the leaf nodes containing the characters in text
  def findLeafBit(tree: CodeTree, text: List[Char], bits: List[Bit]): List[Bit] = tree match {
    case f: Fork => { 
      if (chars(f.left).contains(text.head)) { //if left subtree contains target character
        val newBitList = List[Bit](0) 
        newBitList :: bits
        findLeafBit(f.left, text, newBitList)
      } else { //right subtree must contain target character
        val newBitList = List[Bit](1) 
        newBitList :: bits
        findLeafBit(f.right, text, newBitList)
      } 
    }
    
    case l: Leaf => bits //reached a leaf, no more traversing
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
     val newTuple: (Char, List[Bit]) = table.find(x => x._1 == char).get
     newTuple._2 
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = { 
    val emptyTable: CodeTable = List[(Char, List[Bit])]()
    convert0(tree, emptyTable)
  } 
  
  //helper function for converting a codetree to a codetable.  this function takes a tree to convert and 
  //a codetable that it uses as an accumulator
  def convert0(tree: CodeTree, accuTable: CodeTable): CodeTable = {
    tree match{
      
    case f: Fork => {
      convert0(f.left, accuTable) 
      convert0(f.right, accuTable)
    }//end case f
      
    case l: Leaf => {
      val newBitList = findLeafBit(l, List[Char](l.char), List[Bit]())
      val newTuple = (l.char, newBitList)
      newTuple :: accuTable //add new tuple to accumulator
      }//end case l
    }//end pattern matching
    accuTable
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a :: b
    b
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val newTable = convert(tree)
    val newBitList = List[Bit]()
    quickEncode0(text, newTable, newBitList)
  }
  
  def quickEncode0(text: List[Char], newTable: CodeTable, accuBitList: List[Bit]): List[Bit] = {
      if (!text.isEmpty) {
        val newBitList = codeBits(newTable)(text.head)
        accuBitList:: newBitList
        quickEncode0(text.tail, newTable, newBitList)
      }
      accuBitList //return accumulator
  }
}


