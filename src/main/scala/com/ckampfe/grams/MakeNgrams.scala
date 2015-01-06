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

  val bigrams  = Sentence.ngrams(2) _
  val trigrams = Sentence.ngrams(3) _

  def run(
    filesList: List[String]
  )(
    f: List[(Future[Map[Symbol, List[(List[Symbol], Int)]]], String)] => Unit
  ): Unit = {
    val inputs: List[BufferedSource] = filesList.map { fileName => Source.fromFile(fileName) }

    val ngrams: List[Future[Map[Symbol, List[(List[Symbol], Int)]]]] = inputs.map { file =>
      makeFileString(file).
        flatMap(makeSentenceParser).
        flatMap(parseDocument).
        flatMap(makeNgrams).
        flatMap(groupNgramsByFirstWord).
        flatMap(tallyOccurrences).
        flatMap(sortOccurrences)
    }

    val sentenceFilenamePairs: List[(Future[Map[Symbol, List[(List[Symbol], Int)]]], String)] =
      ngrams.zip(filesList)

    f(sentenceFilenamePairs)
  }

  def makeFileString(inFile: BufferedSource): Future[String] = Future { inFile.mkString }

  def makeSentenceParser(fileString: String): Future[SentenceParser] = Future {
    SentenceParser(fileString)
  }

  def parseDocument(sentenceParser: SentenceParser): Future[List[List[String]]] = Future {
    sentenceParser.parsedWords
  }

  def makeNgrams(sentences: List[List[String]]): Future[List[List[Symbol]]] = Future {
    sentences.flatMap(trigrams)
  }

  def groupNgramsByFirstWord(ngrams: List[List[Symbol]]): Future[Map[Symbol, List[List[Symbol]]]] = Future {
    Sentence.groupByFirstWord(ngrams)
  }

  def tallyOccurrences(
    ngramGroups: Map[Symbol, List[List[Symbol]]]
  ): Future[Map[Symbol, List[(List[Symbol], Int)]]] = Future {
    ngramGroups.mapValues { ngrams =>
      ngrams.groupBy{ ngram => ngram }.map { case (headWord, ngramGroup) =>
        (headWord, ngramGroup.length)
      }.toList
    }
  }

  def sortOccurrences(ngramGroups: Map[Symbol, List[(List[Symbol], Int)]]) = Future {
    ngramGroups.mapValues { ngramGroup =>
      ngramGroup.sortBy(_._2).reverse
    }
  }

  def writeFragment(fragment: List[_], writer: PrintWriter): Unit = Future {
    fragment.mkString(" ") + "\n"
  } andThen {
    case Success(fragment) => writer.write(fragment)
  }

  def main(args: Array[String]): Unit = {

    val filesList = args.toList

    run(filesList) { sentencesWithFilenames =>
      sentencesWithFilenames.map { sentencesWithFilename =>
        sentencesWithFilename._1 andThen {
          case Success(ngramGroup) =>
            ngramGroup.foreach { case (headWord, ngram) =>
              println(s"WORD GROUP FOR: ${headWord.name}")

              ngram.foreach { case (words, occurrences) =>
                println(s"${words}: ${occurrences}")
              }

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
