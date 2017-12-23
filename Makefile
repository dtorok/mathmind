clean:
	rm -rf dist
	rm resources/exercise-puzzle.js

build:
	elm-make src/exercise-puzzle.elm --output resources/exercise-puzzle.js

open: build
	open index.html

dist: build download clean
	mkdir dist
	cp index.html dist/
	cp -r resources dist/

deploy: dist
	firebase deploy

download:
	scripts/download-images.sh
