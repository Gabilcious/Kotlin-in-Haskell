fun f(x: Int) : Unit {}

fun main() : Unit {
    var x: Int? = 10;
    var y: Int?;

    x?.let {    // x: Int
        f(x);
        println ("Reachable");
    }
    y?.let {
        f(y);
        println ("Unreachable");
    }

    f(x!!);
}
