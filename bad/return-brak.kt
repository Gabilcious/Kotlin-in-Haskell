/* Temat:  Brak return lub return tylko w części branch
 * Result: Function should return something
 */

var b: Bool = true;

fun f() : Int {
    if (b) {
        return 10;
    }
}


fun main() : Unit { }