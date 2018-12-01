
object week6 {
  val n = 7
  val j = (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j)))
}
