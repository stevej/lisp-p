all:
	scalac -d classes `find . -name \*.scala`

run:
	scala -classpath classes
