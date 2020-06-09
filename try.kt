var w :Int =1;

fun f(d:Int) : Unit {
    if (d>1) {w =w *d; f(d-1);}
}


fun fib(d: Int) : Int {
    if (d == 1 || d == 2) {
        return 1;
    }
    return fib (d - 1) + fib (d - 2);
}
fun main(): Unit {
    println(fib(4));
    f(4);
    println(w);
    w = 1;
    f(3);
    println(w);
    w = 1;
    f(2);
    println(w);
    w = 1;
    f(1);
    println(w);
}