/* Temat:  Mozna modyfikowac tablice */

val n: Int = 5;

val b: Array<Array<Int?>> = Array(
        n,
        { x: Int ->
            Array(5, {y: Int -> 0; });
        }
);

fun main(): Unit {
    println(b);
    b[1][2] = 100;
    println(b);
}