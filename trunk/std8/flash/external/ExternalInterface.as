intrinsic class flash.external.ExternalInterface {

	static function _initJS() : Void;
	static function _objectID();
	static function _addCallback();
	static function _evalJS();
	static function _callOut();
	static function _escapeXML();
	static function _unescapeXML();
	static function _jsQuoteString();

	static var available : Boolean;

	static function addCallback( funname, inst, method );
	static function call( funname );
	static function _callIn( inst, method, request );

	static function _arrayToXML( a : Array ) : String;
	static function _argumentsToXML( a : Array ) : String;
	static function _objectToXML( o : Object ) : String;
	static function _toXML( v ) : String;

	static function _objectToAS( obj : XMLNode );
	static function _arrayToAS( obj : XMLNode ) : Array;
	static function _argumentsToAS( obj : XMLNode ) : Array;
	static function _toAS( obj : XMLNode );

	static function _arrayToJS( a : Array ) : String;
	static function _objectToJS( o : Object ) : String;
	static function _toJS( v ) : String;


}