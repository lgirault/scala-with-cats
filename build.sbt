name := "scala-with-cats"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

val catsVersion = "1.0.0-RC1"
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion

libraryDependencies ++=
  Seq(
    "org.typelevel" %% "cats-laws" % catsVersion % Test,
    "org.typelevel" %% "cats-testkit" % catsVersion % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
  )

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
