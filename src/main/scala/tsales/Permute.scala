package tsales

object Permute {

  // I know that List already has a permitations operation, but it's
  // fun to implement this once in a while

  def permutations(places: List[String]) : Set[List[String]] = {

    def removeOne(i: Int, l: List[String]) : List[String] = 
      l.take(i) ::: l.drop(i + 1)

    def prependItem(s: String, ll: Set[List[String]]): Set[List[String]] = 
      ll.map(l ⇒ s :: l).toSet

    if (places.length == 0)
      Set(List())
    else {
      places
        .indices
        .flatMap(i ⇒ prependItem(places(i), permutations(removeOne(i, places)))).toSet
    }
  }
}
