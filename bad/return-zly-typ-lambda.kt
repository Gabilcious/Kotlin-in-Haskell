/* Temat:  Zly typ zwracanej wartosci w lambdzie
 *         (zauwazmy, że w lambdzie typ wykrywany jest automatycznie, więc to przypisanie sie nie uda)
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in declaration foo:
 *      Got (Int,String) -> Int? when (Int,String) -> Int is expected
 */

fun main() : Unit {
    val foo: (Int, String) -> Int = { y: Int, w: String ->
        val x: Int? = y;
        x;
    };
}
