package com.ckampfe.grams

import java.util

import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._

import edu.stanford.nlp.dcoref.CorefChain._
import edu.stanford.nlp.dcoref.CorefCoreAnnotations._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations._
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations._
import edu.stanford.nlp.util.CoreMap._

import java.util.Properties

class SentenceParser(inputText: String) {
  val props = new Properties

  //props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
  props.put("annotators", "tokenize, ssplit, pos")
  // props.put("tokenize.whitespace", "true")

  val pipeline = new StanfordCoreNLP(props)
  val document = new Annotation(inputText)

  pipeline.annotate(document)

  lazy val sentences: Vector[CoreMap] = document.get(classOf[SentencesAnnotation]).toVector


  lazy val parsedWords: Vector[Vector[String]] = sentences.map { sentence =>
    // to words
    sentence.get(classOf[TokensAnnotation]).toVector.map { token =>
      // println(token)
      val word = token.get(classOf[TextAnnotation])
      val pos = token.get(classOf[PartOfSpeechAnnotation])
      val ne = token.get(classOf[NamedEntityTagAnnotation])

      word
    }

    // val tree = sentence.get(classOf[TreeAnnotation])
    // val dependencies = sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation])

    //, tree, dependencies)
  }

  val graph = document.get(classOf[CorefChainAnnotation])
}

object SentenceParser {
  def apply(inputText: String) = new SentenceParser(inputText)
}