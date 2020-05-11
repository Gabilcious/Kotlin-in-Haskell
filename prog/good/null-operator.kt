fun f(x: Int) : Unit {}

fun main() : Unit {
    var x: Int? = 10;
    var y: Int?;

    x?.let {    // x: Int
        f(it);
        println ("Reachable");
    }
    y?.let {
        f(it);
        println ("Unreachable");
    }

    f(x!!);
}
