dynamic intrinsic class MovieClip
{
	var useHandCursor:Boolean;
	var enabled:Boolean;
	var focusEnabled:Boolean;
	var tabChildren:Boolean;
	var tabEnabled:Boolean;
	var tabIndex:Number;
	var hitArea:Object;
	var trackAsMenu:Boolean;

	var _x:Number;
	var _y:Number;
	var _xmouse:Number;
	var _ymouse:Number;
	var _xscale:Number;
	var _yscale:Number;
	var _width:Number;
	var _height:Number;
	var _alpha:Number;
	var _lockroot:Boolean;
	var _visible:Boolean;
	var _target:String;
	var _rotation:Number;
	var _name:String;
	var _framesloaded:Number;
	var _droptarget:String;
	var _currentframe:Number;
	var _totalframes:Number;
	var _quality:String;
	var _focusrect:Boolean;
	var _soundbuftime:Number;
	var _url:String;
	var _parent:MovieClip;
	var menu:ContextMenu;

	function getURL(url:String,window:String,method:String):Void;
	function unloadMovie():Void;
	function loadVariables(url:String,method:String):Void;
	function loadMovie(url:String,method:String):Void;
	function attachMovie(id:String,name:String,depth:Number,initObject:Object):MovieClip;
	function swapDepths(mc:Object):Void;
	function localToGlobal(pt:Object):Void;
	function globalToLocal(pt:Object):Void;
	function hitTest():Boolean;
	function getBounds(bounds):Object;
	function getBytesLoaded():Number;
	function getBytesTotal():Number;
	function attachAudio(id:Object):Void;
	function attachVideo(id:Object):Void;
	function getDepth():Number;
	function getInstanceAtDepth(depth:Number):MovieClip;
	function getNextHighestDepth():Number;
	function setMask(mc:Object):Void;
	function play():Void;
	function stop():Void;
	function nextFrame():Void;
	function prevFrame():Void;
	function gotoAndPlay(frame:Object):Void;
	function gotoAndStop(frame:Object):Void;
	function duplicateMovieClip(name:String,depth:Number,initObject:Object):MovieClip;
	function removeMovieClip():Void;
	function startDrag(lockCenter:Boolean,left:Number,top:Number,right:Number,bottom:Number):Void;
	function stopDrag():Void;
	function createEmptyMovieClip(name:String,depth:Number):MovieClip;
	function beginFill(rgb:Number,alpha:Number):Void;
	function beginGradientFill(fillType:String,colors:Array,alphas:Array,ratios:Array,matrix:Object):Void;
	function moveTo(x:Number,y:Number):Void;
	function lineTo(x:Number,y:Number):Void;
	function curveTo(controlX:Number,controlY:Number,anchorX:Number,anchorY:Number):Void;
	function lineStyle(thickness:Number,rgb:Number,alpha:Number):Void;
	function endFill():Void;
	function clear():Void;
	function createTextField(instanceName:String,depth:Number,x:Number,y:Number,width:Number,height:Number):Void;
	function getTextSnapshot():TextSnapshot;

	function onData():Void;
	function onDragOut():Void;
	function onDragOver():Void;
	function onEnterFrame():Void;
	function onKeyDown():Void;
	function onKeyUp():Void;
	function onKillFocus(newFocus:Object):Void;
	function onLoad():Void;
	function onMouseDown():Void;
	function onMouseMove():Void;
	function onMouseUp():Void;
	function onPress():Void;
	function onRelease():Void;
	function onReleaseOutside():Void;
	function onRollOut():Void;
	function onRollOver():Void;
	function onSetFocus(oldFocus:Object):Void;
	function onUnload():Void;

}


