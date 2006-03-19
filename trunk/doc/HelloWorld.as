// Run: mtasc -swf helloworld.swf -main -header 640:480:20 HelloWorld.as
// Your basic Hello World app that every language needs
class HelloWorld {

	static var app : HelloWorld;

	function HelloWorld() {
		_root.createTextField("tf",0,0,0,640,480);
		_root.tf.text = "Hello world !";
	}

	static function main(mc) {
		app = new HelloWorld();
	}
}
