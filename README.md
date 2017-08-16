# Here4

Here4 is a framework for building 3D apps and the worlds they live in.

[www.here4.io](http://www.here4.io/)

## Dreambuggy

Dreambuggy is a driving game.

 * [kfish/dreambuggy](https://github.com/kfish/dreambuggy)

 * [Play](http://kfish.github.io/dreambuggy/)

## Shufflepuck

Shufflepuck is an app, which you can customize place into any world:

 * [kfish/shufflepuck](https://github.com/kfish/shufflepuck)

## Apps

Everything in Here4 is an independent app. For example, apps in dreambuggy include:

 * the terrain
 * the sky
 * the player's avatar
 * cars
 * airplanes
 * a flock of birds (boids)
 * random statues / objects
 * portals
 * a shufflepuck table

To enter an app (ie. start driving a car, controlling a game or go through a
portal), go near it and press space on the keyboard, or X on your gamepad.
Press the same again to return to walking around.

## Worlds

Worlds are named, and have a list of apps.

## Portals

Portals have a destination location, which can be local (within this world) or remote (into another world).
You can specify where the portal takes you:

 * by coordinate
 * by naming an app, and 

Portals in dreambuggy include:

```elm
    , portal <| Remote "world2" (Facing "fire-cube")

...

    , portal <| Remote "world2" (Behind "shufflepuck")

...

    , portal <| Local (Become "boids")
```

## Hacking on Here4

The easiest way to get started is to clone the dreambuggy repo and modify the world.

