
intrinsic class TopLevel {

	static var Infinity:Number;
	static var NaN:Number;

	static function escape(value:String):String;
	static function unescape(value:String):String;
	static function parseInt(value:String,radix:Number):Number;
	static function parseFloat(value:String):Number;
	static function trace(value):Void;
	static function updateAfterEvent():Void;
	static function isNaN(value:Object):Boolean;
	static function isFinite(value:Object):Boolean;

	static function setInterval():Number;
	static function clearInterval(id:Number):Void;

	static function MMExecute(expr:String);

	// private members are not stored into _global, they need special compilation opcodes

	private static function eval( e : String );
	private static function getURL(url:String,target:String,vars:String):Void;
	private static function getTimer():Number;
	private static function random( n : Number ) : Number;
	private static function int( o : Object ) : Number;
	private static function string( o : Object ) : String;
	private static function chr( o : Number ) : String;
	private static function ord( s : String ) : Number;
	private static function delete( o ) : Void;
	private static function loadMovie( url : String, target : MovieClip, method : String ) : Void;
	private static function loadVariables( url : String, target : MovieClip, method : String ) : Void;
	private static function typeof( o ) : String;
	private static function instanceof( o : Object, cl : Object ) : Boolean;
	private static function targetPath( o : MovieClip ) : String;
	private static var arguments : Array;

}
