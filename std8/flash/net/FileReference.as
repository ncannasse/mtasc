intrinsic class flash.net.FileReference {

	var creator;
	var creationDate;
	var modificationDate;
	var size : Number;
	var type;
	var name : String;

	function FileReference();

	function browse();
	function upload();
	function download();
	function cancel();

	var _listeners : Array;
	function addListener();
	function removeListener();

}