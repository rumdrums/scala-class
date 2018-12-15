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

