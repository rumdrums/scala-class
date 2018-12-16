import forcomp.Anagrams._


val sentence = List("Hey", "fuck", "you")
val occs = sentenceOccurrences(sentence)
//println(occs)

val combos = combinations(occs)
combos.foreach(println)
val allWords = combos.flatMap(occ => dictionaryByOccurrences(occ))
