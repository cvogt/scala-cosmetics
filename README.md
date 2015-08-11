scalac-cosmetics: pretty printer for Scala type errors
======================================================

What's the problem?
---------------------

Scala's type errors can be hard to read, in particular if there are complex typed involved.


How does scalac-cosmetics help?
------------------------------------------

It acts as a post-processor for scalac output and pretty-prints error messages, in particular types. Currently the only supported mode is piping sbt output throw scalac-cosmetics (interactive sessions supported).

Before
______

![before](https://raw.github.com/cvogt/scalac-cosmetics/master/before.png)

After
_____

![after](https://raw.github.com/cvogt/scalac-cosmetics/master/after.png)


State of this project
------------------------------------------

Early stage. Things can change at any time.


Installation:
------------------------------------------

	git clone git@github.com:cvogt/scalac-cosmetics.git
	cd scalac-cosmetics
	sbt assembly


Usage (adjust path):
------------------------------------------

	sbt | java -jar scalac-cosmetics/target/scala-2.11/scalac-cosmetics-assembly-0.1.jar
