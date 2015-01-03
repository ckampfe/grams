name := "ngrams"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0" classifier "models"
)
