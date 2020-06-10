/* Temat:  Proba ponownego zadeklarowania zmiennej
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in declaration x:
 *      x was previously declared in this scope
 */

fun main() : Unit {
    var x: Int = 10;
    var x: Int = 20;
    println(x);
}
