
intrinsic class TopLevel {

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

	static function setInterval():Number;
	static function clearInterval(id:Number):Void;

	static function MMExecute(expr:String);

	//
	static function getURL(url:String,target:String,vars:String):Void;
	static function getTimer():Number;
	static function random( n : Number ) : Number;
	static function int( o : Object ) : Number;
	static function string( o : Object ) : String;
	static function chr( o : Number ) : String;
	static function ord( s : String ) : Number;
	static function delete( o ) : Void;
	static function loadMovie( url : String, target : MovieClip, method : String ) : Void;
	

}
