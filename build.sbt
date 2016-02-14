name := "codecup"

version := "0.3"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "0.1.2"

libraryDependencies ++= Seq(
  "org.deeplearning4j" % "deeplearning4j-core" % "0.4-rc3.8",
  "org.deeplearning4j" % "deeplearning4j-ui" % "0.4-rc3.8",
  "org.nd4j" % "nd4j-x86" % "0.4-rc3.8"
)
