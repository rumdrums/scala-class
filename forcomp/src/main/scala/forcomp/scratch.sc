val a = "abcA".toLowerCase.count(_ == '1')
println(a)

type Occurrences = List[(Char, Int)]

//def wordOccurrences(w: String): Occurrences =
//  w.toLowerCase.groupBy(c => w.count(_ == c))

val w = "babcA"
val x = w.toLowerCase()
val z = x.groupBy(c => x.toLowerCase.count(_ == c))

println(x.foldLeft(Map[Char, Int]() withDefaultValue 0)((m, c) => m + (c -> (m(c) + 1))).toList)


println(x.groupBy(c => c).map { case (c, s) => (c, s.length) }.toList.sorted)


import forcomp.Anagrams.{Occurrences, _}
import forcomp.loadDictionary

//val wordOccurrences: List[(Occurrences, List[Word])] = loadDictionary.map(w => wordOccurrences(w) -> (w :: Nil))

//    loadDictionary.foldLeft(Map[Occurrences, List[Word]]().withDefaultValue(List[Word]()))
//    ((acc: Map[Occurrences, List[Word]], w: Word) => acc + ( wordOccurrences(w) -> w :: acc(wordOccurrences(w))) )

println("wtf")
loadDictionary.head

val test: List[(Occurrences, List[Word])] = loadDictionary.map(w => wordOccurrences(w) -> (w :: Nil))
println(test)
