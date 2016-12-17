name         := "soapower"

version      := "2.1.7"

lazy val root = (project in file(".")).enablePlugins(PlayScala,SbtWeb)

scalaVersion := "2.11.6"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
    cache, 
    ws,
    "org.reactivemongo" %% "reactivemongo" % "0.12.0",
    "org.reactivemongo" %% "play2-reactivemongo" % "0.12.0-play24"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

mappings in Universal ++= baseDirectory.map { dir => (dir / "soapowerctl.sh").*** --- dir pair relativeTo(dir) }.value

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](
  name,
  version,
  scalaVersion,
  sbtVersion,
  BuildInfoKey.map(name) { case (k, v) => "project" + k.capitalize -> v.capitalize },
  BuildInfoKey.map(version) { case (k, v) => "versionDoc" -> (v.dropRight(1) + "x") } // version 2.0.0 -> 2.0.x
)

buildInfoPackage := "soapower.build.info"

