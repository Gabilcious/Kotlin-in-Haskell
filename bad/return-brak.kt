/* Temat:  Brak return lub return tylko w czesci branch
 * Result:
 * interpreter: Error has occured:
 *      in function f:
 *      Function should return something
 */

var b: Bool = true;

fun f() : Int {
    if (b) {
        return 10;
    }
}


fun main() : Unit { }