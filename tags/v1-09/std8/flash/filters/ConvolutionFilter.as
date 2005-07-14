intrinsic class flash.filters.ConvolutionFilter {

	var alpha : Number;
	var color : Number;
	var clamp : Boolean;
	var preserveAlpha : Boolean;
	var bias : Number;
	var divisor : Number;
	var matrix : Array;
	var matrixX : Number;
	var matrixY : Number;

	function ConvolutionFilter();
	function clone() : ConvolutionFilter;

}