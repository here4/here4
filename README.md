# Dreambuggy

Drive dreambuggy on mountains, snow and sand. Fly dreambird.

Plug in a gamepad, or use mouse and keyboard arrows or WASD keys to move.
Press X to switch between driving and flying.

Supports up to 2 USB gamepads.

## Play in your browser

[![PLAY](resources/beach1.png)][demo]
[![PLAY](resources/snow1.png)][demo]

[demo]: http://kfish.github.io/dreambuggy/

## Build Locally

This package depends on kfish/elm-gamepad and kfish/dynamic,
which contain experimental Native code. They are not yet
whiteliested for inclusion in the Elm package archive.
You will need to use an unofficial installer like
[elm-install](https://github.com/gdotdesign/elm-github-install)

Clone this repository and run a local HTTP file server,
such as the included ./server.py:

```bash
git clone https://github.com/kfish/dreambuggy.git
cd dreambuggy
elm-install
make
./server.py
```

And then open [http://localhost:8000/index.html](http://localhost:8000/index.html) to see it in action!

## Credits

Built with [Elm](http://elm-lang.org/).

This demo includes the following shaders:

  * [Fire](https://www.shadertoy.com/view/Xsl3zN) by 301
