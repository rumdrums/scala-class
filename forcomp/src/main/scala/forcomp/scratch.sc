import forcomp.Anagrams._


val sentence = List("Yes", "man")
val occs = sentenceOccurrences(sentence)
println("occs: ", occs)

val combos = combinations(occs)
println("combos: ", combos)
combos.foreach(i => i.foreach(j => println("this combo: \n", j)))
//val allWords = combos.flatMap(occ => dictionaryByOccurrences(occ))

