package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  val s1 = singletonSet(4)
  val s2 = singletonSet(8)
  val s3 = singletonSet(3)
  
  val y = union(s1, s2)
  val z = union(y, s3)
  printSet(y)
  printSet(s3)
  printSet(map(z, x => x * x))
}


