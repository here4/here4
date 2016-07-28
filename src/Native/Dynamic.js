var _kfish$dreambuggy$Native_Dynamic = function()
{

function pack(x) {
	var origCtor = x.ctor;
	x.ctor = "Payload";
	return { ctor: "Dynamic", origCtor: origCtor, payload: x };
}

function unpack(x) {
	var payload = x.payload;
	payload.ctor = x.origCtor;
	return payload;
}

return {
	pack: pack,
	unpack: unpack
}

}();
