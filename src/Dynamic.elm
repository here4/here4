module Dynamic exposing (Dynamic, pack, unpack)

import Native.Dynamic

type Dynamic = Dynamic

pack : a -> Dynamic
pack = Native.Dynamic.pack

unpack : Dynamic -> a
unpack = Native.Dynamic.unpack
