object Scratch {
  List(0,1,2,3).map(x => x * x)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x, y)  => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (_, acc) => acc + 1 )

  val xs = List(0,1,2,3)

  mapFun[Int, Int](xs, (x => x * x))

  lengthFun(xs)
}