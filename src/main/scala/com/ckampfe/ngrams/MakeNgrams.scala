package com.ckampfe.ngrams

import java.io.{PrintWriter, File}

import scala.concurrent.{Promise, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Try, Success}

/**
 * Created by clark on 12/21/14.
 */
object MakeNgrams {

  val bigrams = Sentence.ngrams(2) _
  val trigrams = Sentence.ngrams(3) _

  def parseSentencesForFiles(
    filesList: List[String])(f: List[(Future[Map[Symbol, Seq[Seq[Symbol]]]], String)] => Unit
  ): Unit = {
    val inputs: List[BufferedSource] = filesList.map { fileName => Source.fromFile(fileName) }

    val fileSentences: List[Future[Map[Symbol, Seq[Seq[Symbol]]]]] = inputs.map { file =>
      makeFileString(file).
        flatMap(makeSentenceParser).
        flatMap(parseDocument).
        flatMap(makeNgrams).
        flatMap(groupNgramsByFirstWord)
    }

    val sentenceFilenamePairs: List[(Future[Map[Symbol, Seq[Seq[Symbol]]]], String)] =
      fileSentences.zip(filesList)

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
          case Success(gramGroup) =>
            gramGroup.foreach { case (w, ws) =>
              println("---------------")
              println(s"WORD GROUP FOR: ${w.name}")
              ws.map(syms => syms.map(sym => sym.name)).foreach(println)
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
