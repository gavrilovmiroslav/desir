
name := "desir"

organization := "edu.ucsb.cs.pllab"

version := "0.0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test" withSources() withJavadoc()
)

// ***************************************************************** maybe not needed?
// https://mvnrepository.com/artifact/com.github.javaparser/javaparser-core
// libraryDependencies += "com.github.javaparser" % "javaparser-core" % "3.6.26"

// https://mvnrepository.com/artifact/com.github.javaparser/javaparser-symbol-solver-core
// libraryDependencies += "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.6.26"
// ***************************************************************** maybe not needed?

// https://mvnrepository.com/artifact/org.apache.commons/commons-text
libraryDependencies += "org.apache.commons" % "commons-text" % "1.6"

// https://mvnrepository.com/artifact/org.apache.commons/commons-exec
libraryDependencies += "org.apache.commons" % "commons-exec" % "1.3"

// https://mvnrepository.com/artifact/org.apache.commons/commons-lang3
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.8.1"

// https://mvnrepository.com/artifact/com.rabbitmq/amqp-client
libraryDependencies += "com.rabbitmq" % "amqp-client" % "5.4.3"

// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

initialCommands := "import desir._"

fork in run := true

javaOptions += "-Xmx4096M"

assemblyOutputPath in assembly := file("./desir.jar")