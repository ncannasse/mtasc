
intrinsic class TopLevel {

	static var _global:Object;
	static var _root:Object;

	static var Infinity:Number;
	static var NaN:Number;

	static function escape(value:String):String;
	static function unescape(value:String):String;
	static function parseInt(value:String,radix:Number):Number;
	static function parseFloat(value:String):Number;
	static function trace(value:String):Void;
	static function updateAfterEvent():Void;
	static function isNaN(value:Object):Boolean;
	static function isFinite(value:Object):Boolean;
	static function getURL(url:String,target:String,vars:String):Void;

	static function setInterval():Number;
	static function clearInterval(id:Number):Void;

	static function MMExecute(expr:String);

}
