intrinsic class flash.filters.DisplacementMapFilter {

	static var Mode : flash.filters.__Mode;

	var alpha : Number;
	var color : Number;
	var mode : String;
	var scaleX : Number;
	var scaleY : Number;
	var componentX : Number;
	var componentY : Number;
	var mapPoint : flash.geom.Point;
	var mapBitmap;

	function DisplacementMapFilter();
	function clone() : DisplacementMapFilter;

}