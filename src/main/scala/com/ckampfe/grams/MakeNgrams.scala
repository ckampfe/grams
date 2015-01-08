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

  def stripExtension(file: String): (String, String, String) = {
    val re = """(.*/)??([\w-_]+)\.*(.*)""".r
    file match {
      case re(path, fileName, extensions) => (path, fileName, extensions)
    }
  }

  def writeFragment(fragment: List[_], writer: PrintWriter): Unit = Future {
    fragment.mkString(" ") + "\n"
  } andThen {
    case Success(fragment) => writer.write(fragment)
  }

  def main(args: Array[String]): Unit = {

    val filesList = args.toList

    run(filesList) { ngramGroupsAndFiles =>
      ngramGroupsAndFiles.map { ngramGroupsAndFile =>
        ngramGroupsAndFile._1 andThen {
          case Success(groups) =>

            // creating output file and writer
            val (path, fileName, extension) = stripExtension(ngramGroupsAndFile._2)
            val writer = new PrintWriter(new File(path + fileName + "-out"))

            println(s"begin writing ${path + fileName + extension} to ${path + fileName}-out")

            groups.foreach { case (headWord, ngram) =>
              writer.write(s"<<< ${headWord.name} >>>\n")

              ngram.foreach { case (words, occurrences) =>
                writer.write(s"${words.map(_.name).mkString(" && ")} && ${occurrences}\n")
              }
            }

            println(s"done writing ${path + fileName + extension} to ${path + fileName}-out")
            writer.close()
          case Failure(ex) => throw ex
        }
      }
    }
  }
}
