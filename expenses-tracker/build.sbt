ThisBuild / scalaVersion := "2.13.12"

ThisBuild / version := "1.0-SNAPSHOT"

val play_version = "2.9.2"
val play_slick_version = "5.3.0"
val play_json_version = "2.10.4"
val doobie_version = "1.0.0-RC1"
val zio_version   = "2.0.13"
val cast_version = "2.10.0"
val cats_effect_version = "3.4.8"
val cats_log_version = "2.6.0"
val apache_codec_version = "1.1.3"
val new_type_version = "0.4.4"
val enumeratum_version = "1.7.3"
val scalaj_http_version = "2.4.2"
val jansi_version = "2.4.1"
val jsoup_version = "1.17.2"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)

  .settings(
    name := """expenses-tracker""",
    libraryDependencies ++= Seq(
      guice
      , "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test
      , "com.typesafe.play" %% "play-slick" % play_slick_version
      , "com.typesafe.play" %% "play" % play_version
      , "com.typesafe.play" %% "play-json" % play_json_version
      , "org.mariadb" % "r2dbc-mariadb" % apache_codec_version
      , "org.tpolecat" %% "doobie-core" % doobie_version
      , "org.tpolecat" %% "doobie-postgres" % doobie_version
      , "org.tpolecat" %% "doobie-hikari" % doobie_version
      , "io.estatico" %% "newtype" % new_type_version
      , "com.beachape" %% "enumeratum" % enumeratum_version
      , "org.scalaj" %% "scalaj-http" % scalaj_http_version
      , "org.typelevel" %% "cats-core" % cast_version
      , "org.typelevel" %% "cats-effect" % cats_effect_version
      , "org.typelevel" %% "log4cats-slf4j" % cats_log_version
      , "org.typelevel" %% "log4cats-core" % cats_log_version
      , "org.fusesource.jansi" % "jansi" % jansi_version
      , "org.jsoup" % "jsoup" % jsoup_version
    )
  )