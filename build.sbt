resolvers ++= Seq(
	// other resolvers here
	// if you want to use snapshot builds (currently 0.3-SNAPSHOT), use this.
	"Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies  ++= Seq(
	"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"org.scalanlp" %% "breeze-math" % "0.2.3",
	"org.scalanlp" %% "breeze-learn" % "0.2.3",
	"org.scalanlp" %% "breeze-process" % "0.2.3",
	"org.scalanlp" %% "breeze-viz" % "0.2.3"
)

// Scala 2.9.2 is still supported for 0.2.1, but is dropped afterwards.
scalaVersion := "2.10.1"