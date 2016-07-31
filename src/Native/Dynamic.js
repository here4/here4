var _kfish$dreambuggy$Native_Dynamic = function()
{

function pack(x) {
	var origCtor = x.ctor;
	return { ctor: "Dynamic", payload: x };
}

function unpack(x) {
	var payload = x.payload;
	return payload;
}

return {
	pack: pack,
	unpack: unpack
}

}();
