/* Temat:  Próba ponownego zadeklarowania zmiennej
 * Result: Ident "x" was previously declared in this scope
 */

fun main() : Unit {
    var x: Int = 10;
    var x: Int = 20;
    println(x);
}
