build:
	elm-make src/exercise-puzzle.elm

dist: build
	rm -rf dist
	mkdir dist
	cp index.html dist/
	cp css.css dist/

deploy: dist
	firebase deploy