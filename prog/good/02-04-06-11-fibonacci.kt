fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    print ( fib (10) );
}
