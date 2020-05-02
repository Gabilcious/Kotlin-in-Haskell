fun f(x: Int) : Unit {}

fun main() : Unit {
    var y: Int?;

    f(x!!);     // NullPointerException
}
