val n: Int = -5;

val b: Array<Array<Int?>> = Array(
        n,
        { x: Int ->
            Array(5, {y: Int ->
                var ret : Int?;
                if (x + y != 4) { ret = y; }
                ret;
            }
            );
        }
);

val c: Array<Int> = Array(5, { y: Int -> y+10; });

fun foo(): Int {
    return 10/0;
}

fun main(): Unit {
    if (true) {
        b[1][2] = 100;
    }
}