intrinsic class Object
{
	function watch(name:String, callback:Function, userData:Object):Boolean;
	function unwatch(name:String):Boolean;
	function addProperty(name:String, getter:Function, setter:Function):Boolean;
	function toString():String;

	static function registerClass(name:String, theClass:Object):Boolean;
}

