name := "pascal-compiler"
version := "0.1.0"
scalaVersion := "3.5.2"

libraryDependencies ++= Seq(
  // Parser combinators
  "com.lihaoyi" %% "fastparse" % "3.1.1",

  // LLVM bindings via JavaCPP
  "org.bytedeco" % "llvm-platform" % "20.1.7-1.5.12",

  // Testing
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// Scala compiler options
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings"
)

// Assembly plugin for fat JAR
assembly / assemblyJarName := "pascal-compiler.jar"

assembly / mainClass := Some("pascal.Main")

// Merge strategy for assembly
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("module-info.class") => MergeStrategy.discard
  case x => MergeStrategy.first
}
