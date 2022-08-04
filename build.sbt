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
  .settings(
    name := "password-validation",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-kernel" % "2.8.0",
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
