dynamic intrinsic class Object
{
	function watch(name:String, callback:Function, userData:Object):Boolean;
	function unwatch(name:String):Boolean;
	function addProperty(name:String, getter:Function, setter:Function):Boolean;
	function hasOwnProperty(name:String):Boolean;
	function toString():String;


	var __proto__;
	var constructor : Function;

	static function registerClass(name:String, theClass:Object):Boolean;
	static var prototype:Object;

}

