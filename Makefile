clean:
	rm -rf dist

build:
	elm-make src/exercise-puzzle.elm

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
