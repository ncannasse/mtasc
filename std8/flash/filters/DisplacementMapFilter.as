intrinsic class flash.filters.DisplacementMapFilter {

	var alpha : Number;
	var color : Number;
	var mode : String;
	var scaleX : Number;
	var scaleY : Number;
	var componentX : Number;
	var componentY : Number;
	var mapPoint : flash.geom.Point;
	var mapBitmap;

	function DisplacementMapFilter(mapBitmap : BitmapData, mapPoint : Point, componentX : Number, componentY : Number, scaleX : Number, scaleY : Number, mode : String, color : Number, alpha : Number);
	function clone() : DisplacementMapFilter;

}