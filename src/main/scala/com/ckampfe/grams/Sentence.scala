package com.ckampfe.grams

object Sentence {
  /**
   *
   * @param n The size of the resulting ngrams
   * @param sentence Input collection, a sentence
   * @tparam A Underlying input type
   * @return A collection of symbol ngrams of size n
   */
  def ngrams[A](n: Int)(sentence: List[A]): List[List[Symbol]] = sentence.map { word =>
    Symbol(word.toString)
  }.sliding(n).toList

  /**
    *
    * @param ngrams A collection of ngrams
    * @tparam A The underlying ngram type
    * @return A map of ngram heads to a collection of member ngrams
    */
  def groupByFirstWord[A](ngrams: List[List[A]]): Map[A, List[List[A]]] =
    ngrams.groupBy(_.head).map { case (headWord, ngram) =>
      (headWord, ngram)
    }
}
