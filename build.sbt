ThisBuild / organization := "es.eriktorr"
ThisBuild / version := "1.0.0"
ThisBuild / idePackagePrefix := Some("es.eriktorr.password_validation")
Global / excludeLintKeys += idePackagePrefix

ThisBuild / scalaVersion := "3.1.3"

Global / cancelable := true
Global / fork := true
Global / onChangedBuildSource := ReloadOnSourceChanges

Compile / compile / wartremoverErrors ++= Warts.unsafe.filter(_ != Wart.DefaultArguments)
Test / compile / wartremoverErrors ++= Warts.unsafe.filter(_ != Wart.DefaultArguments)

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Yexplicit-nulls", // https://docs.scala-lang.org/scala3/reference/other-new-features/explicit-nulls.html
  "-Ysafe-init", // https://docs.scala-lang.org/scala3/reference/other-new-features/safe-initialization.html
)

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "password-validation",
    Universal / maintainer := "https://eriktorr.es",
    Compile / mainClass := Some("es.eriktorr.password_validation.PasswordValidationApp"),
    libraryDependencies ++= Seq(
      "org.apache.logging.log4j" % "log4j-api" % "2.18.0" % Runtime,
      "org.apache.logging.log4j" % "log4j-core" % "2.18.0" % Runtime,
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.18.0" % Runtime,
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-kernel" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.3.14",
      "org.typelevel" %% "cats-effect-kernel" % "3.3.14",
      "org.typelevel" %% "cats-effect-std" % "3.3.14",
      "org.typelevel" %% "log4cats-slf4j" % "2.4.0",
      "org.typelevel" %% "log4cats-core_sjs1" % "2.4.0",
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
      "org.typelevel" %% "scalacheck-effect" % "1.0.4" % Test,
      "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test,
    ),
    onLoadMessage := {
      s"""Custom tasks:
         |check - run all project checks
         |""".stripMargin
    },
  )

addCommandAlias(
  "check",
  "; undeclaredCompileDependenciesTest; unusedCompileDependenciesTest; scalafixAll; scalafmtSbtCheck; scalafmtCheckAll",
)
