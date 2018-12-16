import forcomp.Anagrams.Occurrences

val a = List(('a', 2),('b', 2), ('c', 3)).foldLeft(List[(Char, Int)]()) {
  (acc, pair) => {
    pair :: acc
  }
}
print(a)


print(List(1) ++ List())


def calledWhat(l: List[Int]): List[Int] = {
  println("called: ", l)
 for {
   i <- l
   rest <- calledWhat(l.tail)
 } yield rest
}

calledWhat(List(3,2,1))

println(List() :: List())

val b = List(('b', 1))
val c = List()
println(b ::: c)

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))

val z = x.foldLeft(List[(Char, Int)]())( (acc: Occurrences, pair: (Char, Int)) => {
  val z = y.toMap.withDefaultValue(0)
  val (char, int) = pair
  val zz = int - z(char)
  if (zz > 0) (char, int-zz) :: acc else acc
})

println(z)
