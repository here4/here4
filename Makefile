
GHUSER=kfish
PROJECT=dreambuggy

REMOTE=${GHUSER}-${PROJECT}

SOURCE=dreambuggy/Main.elm
SCRIPT=dreambuggy.js
TARGET=build/${SCRIPT}
DEPLOY=scripts/${SCRIPT}


BRANCH=$(shell git rev-parse --abbrev-ref HEAD)

.PHONY: update build pull push clean realclean

all: build

update:
	elm-install

build:
	elm-make ${SOURCE} --output ${TARGET}
	mkdir -p scripts
	cp ${TARGET} ${DEPLOY}

publish: build
	git add -f ${TARGET}
	git commit -m "Update ${TARGET}"
	git push --force ${REMOTE} HEAD:gh-pages
	git reset --hard HEAD^

pull:
	git fetch ${REMOTE}
	git merge ${REMOTE}/${BRANCH}

push:
	git push ${REMOTE} ${BRANCH}

clean:
	rm -rf build
	rm -rf elm-stuff/build-artifacts

realclean:
	rm -rf elm-stuff/

