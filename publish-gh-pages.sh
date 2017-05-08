#!/bin/sh

PROJECT=dreambuggy

git push kfish-${PROJECT} ${PROJECT}-master:master
if [ -e build/Main.js ]; then
	git add -f build/Main.js
	git commit -m 'Update Main.js'
	git push --force kfish-${PROJECT} HEAD:gh-pages
	git reset --hard HEAD^
else
	echo "Please build build/Main.js"
fi
