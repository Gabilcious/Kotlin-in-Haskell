fun fib(d: Int) : Int {
    if (d == 1 || d == 2) {
        return 1;
    }
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    print ( fib (9) ); // 34
}
