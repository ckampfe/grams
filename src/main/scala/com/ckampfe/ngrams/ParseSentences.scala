package com.ckampfe.ngrams

object ParseSentences {
  def main(args: Array[String]): Unit = {
    val s = new SentenceParser(args(0))
    s.parsedWords foreach println
  }
}