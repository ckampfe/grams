package com.ckampfe.grams

import java.io.{PrintWriter, File}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}

/**
 * Created by clark on 12/21/14.
 */
object MakeNgrams {

  val bigrams = Sentence.ngrams(2) _
  val trigrams = Sentence.ngrams(3) _

  def parseSentencesForFiles(
    filesList: List[String])(f: List[(Future[Map[Symbol, Seq[(Seq[Symbol], Int)]]], String)] => Unit
  ): Unit = {
    val inputs: List[BufferedSource] = filesList.map { fileName => Source.fromFile(fileName) }

    val ngrams: List[Future[Map[Symbol, Seq[(Seq[Symbol], Int)]]]] = inputs.map { file =>
      makeFileString(file).
        flatMap(makeSentenceParser).
        flatMap(parseDocument).
        flatMap(makeNgrams).
        flatMap(groupNgramsByFirstWord).
        flatMap(tallyOccurrences).
        flatMap(sortOccurrences)
    }

    val sentenceFilenamePairs: List[(Future[Map[Symbol, Seq[(Seq[Symbol], Int)]]], String)] =
      ngrams.zip(filesList)

    f(sentenceFilenamePairs)
  }

  def makeFileString(inFile: BufferedSource): Future[String] = Future { inFile.mkString }

  def makeSentenceParser(fileString: String): Future[SentenceParser] = Future {
    SentenceParser(fileString)
  }

  def parseDocument(sentenceParser: SentenceParser): Future[Vector[Vector[String]]] = Future {
    sentenceParser.parsedWords
  }

  def makeNgrams(sentences: Seq[Seq[String]]): Future[Seq[Seq[Symbol]]] = Future {
    sentences.flatMap(trigrams)
  }

  def groupNgramsByFirstWord(ngrams: Seq[Seq[Symbol]]): Future[Map[Symbol, Seq[Seq[Symbol]]]] = Future {
    Sentence.groupByWord(ngrams)
  }

  def tallyOccurrences(
    ngramGroups: Map[Symbol, Seq[Seq[Symbol]]]
  ): Future[Map[Symbol, Seq[(Seq[Symbol], Int)]]] = Future {
    ngramGroups.mapValues { ngramTails =>
      ngramTails.groupBy { x => x }.map { (y: (Seq[Symbol], Seq[Seq[Symbol]])) =>
        (y._1, y._2.length)
      }.toSeq
    }
  }

  def sortOccurrences(ngramGroups: Map[Symbol, Seq[(Seq[Symbol], Int)]]) = Future {
    ngramGroups.mapValues { ngramGroup => ngramGroup.sortBy(_._2).reverse }
  }

  def writeFragment(fragment: Seq[_], writer: PrintWriter): Unit = Future {
    fragment.mkString(" ") + "\n"
  } andThen {
    case Success(fragment) => writer.write(fragment)
  }

  def main(args: Array[String]): Unit = {

    val filesList = args.toList

    parseSentencesForFiles(filesList) { sentencesWithFilenames =>
      sentencesWithFilenames.map { sentencesWithFilename =>
        sentencesWithFilename._1 andThen {
          case Success(ngramGroup) =>
            ngramGroup.foreach { case (ngramHead, ngramTail) =>
              println(s"WORD GROUP FOR: ${ngramHead.name}")
              ngramTail.foreach{ syms => println(s"${syms._1}: ${syms._2}") }
              println("---------------")
            }
            //val frontWriter = new PrintWriter(new File(sentencesWithFilename.fileName + "-front"))
            //val backWriter  = new PrintWriter(new File(sentencesWithFilename.fileName + "-back"))

          //  sentences.foreach { sentence =>
          //    writeFragment(sentence.front, frontWriter)
          //    writeFragment(sentence.back, backWriter)
          //  }

          //  frontWriter.close()
          //  backWriter.close()
          case Failure(ex) => throw ex
        }
      }
    }
  }
}
