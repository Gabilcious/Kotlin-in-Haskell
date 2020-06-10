/* Temat:  Mozna nazwac parametr funkcji tak samo, jak w poprzedniej funkcji */

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

}