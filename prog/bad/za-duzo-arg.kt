fun fibb(d: Int) : Int {
    return fibb (d - 1) + fibb (d - 2);
}

fun main() : Unit {
    print ( fib (10, 2) );
}
