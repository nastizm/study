package jp.example

class Cat {
	val dangerous = false
}

class Tiger (
	override val dangerous: Boolean,
	private var age: Int
) extends Cat

class Tiger2(param1: Boolean, param2: Int) extends Cat {
	override val dangerous = param1
	private var age = param2
}
