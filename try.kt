val n: Int = 5;

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

fun foo(): Int {
    assert(10==19);
    return 0;
}
// TODO: println(b[4]/10);

fun main(): Unit {
    for (i in [1..10]) {
        if (true) {
            val a: Int = 10/0; // TODO return 10/0;
            foo();
        }
    }
}