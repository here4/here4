
GHUSER=kfish
PROJECT=dreambuggy

REMOTE=${GHUSER}-${PROJECT}

SOURCE=dreambuggy/Main.elm
TARGET=build/Main.js

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)

.PHONY: update build pull push clean realclean

all: build

update:
	elm-install

build:
	elm-make ${SOURCE} --output ${TARGET}

rebuild: clean build

publish: build
	git add -f build/Main.js
	git commit -m 'Update Main.js'
	git push --force kfish-${PROJECT} HEAD:gh-pages
	git reset --hard HEAD^

pull:
	git fetch ${REMOTE}
	git merge ${REMOTE}/${BRANCH}

push:
	git push ${REMOTE} ${BRANCH}

clean:
	rm -rf elm-stuff/build-artifacts

realclean:
	rm -rf elm-stuff/

