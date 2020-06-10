/* Temat:  Proba przypisania zlego typu do zmiennej
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in statement ((x="V1")):
 *      Got String when Int is expected
 */

fun main() : Unit {
    var x: Int = 10;
    x = "V1";
	println(x);
}
