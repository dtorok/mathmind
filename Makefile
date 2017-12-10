build:
	elm-make src/exercise-puzzle.elm

dist: build
	rm -rf dist
	mkdir dist
	cp index.html dist/
	cp -r resources dist/

deploy: dist
	firebase deploy