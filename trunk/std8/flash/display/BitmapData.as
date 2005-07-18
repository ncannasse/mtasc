intrinsic class flash.display.BitmapData {

	static function loadBitmap();

	var width : Number;
	var height : Number;
	var rectangle : flash.geom.Rectangle;
	var transparent : Boolean;

	function BitmapData( width : Number, height : Number, unk : Boolean, color : Number );

	function getPixel( x : Number, y : Number ) : Number;
	function setPixel( x : Number, y : Number, color : Number ) : Void;
	function getPixel32( x : Number, y : Number ) : Number;
	function setPixel32( x : Number, y : Number, color : Number ) : Void;

	function fillRect( r : flash.geom.Rectangle, color : Number ) : Void;
	function copyPixels();
	function applyFilter();
	function scroll( dx : Number, dy : Number ) : Void;
	function threshold();
	function draw();
	function pixelDissolve();
	function floodFill( x : Number, y : Number, color : Number ) : Void;
	function getColorBoundsRect();
	function perlinNoise();
	function colorTransform();
	function hitTest();
	function paletteMap();
	function merge();
	function noise();
	function copyChannel();
	function clone();
	function dispose();
	function generateFilterRect();

}