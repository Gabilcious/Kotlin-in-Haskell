fun f(x: Int) : Unit {}

fun main() : Unit {
    var x: Int? = 10;
    var y: Int?;

    x?.let {    // x: Int
        f(x);
    }
    y?.let {
        f(y);
        // Unreachable code because y is null.
    }

    f(x!!);
}
