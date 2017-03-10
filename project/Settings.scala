import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._


/**
 * Defines settings for the projects:
 */
object Settings {

  object versions {
    val akka = "2.4.17"
    val akkaHttp = "10.0.4"
    val autowire = "0.2.5"
    val sigarLoader = "1.6.6-rev002"
    val akkaLog4j = "0.2.0"
    val bootstrap = "3.3.2"
    val d3 = "3.5.5-1"
    val jQuery = "1.11.1"
    val log4j = "2.3"
    val scala = "2.11.7"
    val scalaTest = "2.2.5"
    val logBack = "1.1.2"
    val scalaLogging = "3.1.0"
    val config = "1.3.0"
    val react = "15.4.2"
    val scalacheck = "1.12.2"
    val scalaCSS = "0.3.0"
    val scalajsDom = "0.8.0"
    val scalajsReact = "0.11.3"
    val scalaRx = "0.2.8"
    val scalaTags = "0.5.2"
    val scalaz = "7.1.2"
    val upickle = "0.3.6"
    val utest = "0.3.1"
    val nScalaTimeVersion  = "2.2.0"
    val scalaD3 = "0.2.0"
  }


  val name = "cluster-console"
  val version = "0.1"
  val scalaVersion = "2.11.7"
  val scalacOptions = Seq("-encoding", "UTF-8", "-target:jvm-1.8", "-deprecation", "-language:postfixOps")
  //  val javacOptions  = Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-Xlint:deprecation", "-Xlint:-options"),

  /** Set some basic options when running the project with Revolver */
  val jvmRuntimeOptions = Seq(
    "-Xmx1G"
  )


  val sharedDependencies = Def.setting(Seq(
    "com.lihaoyi" %%% "autowire" % versions.autowire,
    "com.lihaoyi" %%% "upickle" % versions.upickle,
    "com.lihaoyi" %%% "utest" % "0.3.1",
    "org.webjars" % "font-awesome" % "4.3.0-1" % Provided,
    "org.webjars" % "bootstrap" % versions.bootstrap % Provided
  ))

  /** Dependencies only used by the JVM project */
  val jvmDependencies = Def.setting(Seq(
    "com.typesafe.akka" %% "akka-actor" % versions.akka,
    "com.typesafe.akka" %% "akka-cluster" % versions.akka,
    "com.typesafe.akka" %% "akka-contrib" % versions.akka,
    "com.typesafe.akka" %% "akka-cluster-metrics" % versions.akka,
    "com.typesafe.akka" %% "akka-http" % versions.akkaHttp,
    "com.typesafe.akka" %% "akka-http-testkit" % versions.akkaHttp,
    "com.typesafe.akka" %% "akka-stream" % versions.akka,
    "com.typesafe.akka" %% "akka-slf4j" % versions.akka,
    "com.typesafe.akka" %% "akka-multi-node-testkit" % versions.akka,
    "com.typesafe.akka" %% "akka-testkit" % versions.akka,
    "org.apache.logging.log4j" % "log4j-core" % versions.log4j,
    "org.scalatest" %% "scalatest" % versions.scalaTest % "test",
    "ch.qos.logback" % "logback-classic" % versions.logBack,
    "com.typesafe.scala-logging" %% "scala-logging" % versions.scalaLogging,
    "com.typesafe" % "config" % versions.config,
    "org.scalaz" %% "scalaz-core" % versions.scalaz,
    "com.lihaoyi" %% "scalatags" % versions.scalaTags,
    "io.kamon" % "sigar-loader" % versions.sigarLoader,
    "com.github.nscala-time" %% "nscala-time" % versions.nScalaTimeVersion
  ))

  //  /** Dependencies only used by the JS project (note the use of %%% instead of %%) */
  val scalajsDependencies = Def.setting(Seq(
    "com.github.japgolly.scalajs-react" %%% "core" % versions.scalajsReact,
    "com.github.japgolly.scalajs-react" %%% "extra" % versions.scalajsReact,
    "com.github.japgolly.scalacss" %%% "ext-react" % versions.scalaCSS,
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "com.lihaoyi" %%% "scalarx" % "0.2.8",
    "org.singlespaced" %%% "scalajs-d3" % versions.scalaD3,
    "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.5.0"
  ))

  val jsDependencies = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react % "compile"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % versions.react % "compile"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars" % "jquery" % versions.jQuery / "jquery.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" dependsOn "jquery.js",
    "org.webjars" % "d3js" % versions.d3 / "d3.js"
  ))

  /** Same dependecies, but for production build, using minified versions */
  val jsDependenciesProduction = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react % "compile"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % versions.react % "compile"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",
    "org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js",
    "org.webjars" % "d3js" % versions.d3 / "d3.js" minified "d3.min.js" commonJSName "d3"
  ))


}

