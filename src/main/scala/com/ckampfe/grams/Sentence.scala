package com.ckampfe.grams

object Sentence {
  /**
   *
   * @param n The size of the resulting ngrams
   * @param sentence Input collection, a sentence
   * @tparam A Underlying input type
   * @return A collection of symbol ngrams of size n
   */
  def ngrams[A](n: Int)(sentence: Seq[A]): Vector[Seq[Symbol]] = sentence.map { word =>
    Symbol(word.toString)
  }.sliding(n).toVector

  /** aggregate ngrams by their first word, and then all following for that first word
    *
    * @param ngrams A collection of ngrams
    * @tparam A The underlying ngram type
    * @return A map of ngram heads to a collection of collections of their tails
    */
  def groupByFirstWord[A](ngrams: Seq[Seq[A]]): Map[A, Seq[Seq[A]]] =
    ngrams.groupBy(_.head).map { gramMap =>
      (gramMap._1, gramMap._2.map(_.tail))
    }
}
