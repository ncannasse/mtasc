intrinsic class MovieClipLoader
{
	function addListener(listener:Object):Boolean;
	function getProgress(target:Object):Object;
	function loadClip(url:String, target:Object):Boolean;
	function removeListener(listener:Object):Boolean;
	function unloadClip(target:Object):Boolean;

	function onLoadComplete(target:Object):Void;
	function onLoadError(target:Object, errorCode:String):Void;
	function onLoadProgress(target:Object, bytesLoaded:Number, bytesTotal:Number):Void;
	function onLoadStart(target:Object):Void;
}
