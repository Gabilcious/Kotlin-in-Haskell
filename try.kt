val n: Int = -1;

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
//    assert(10==19);
    return 10/0;
}
// TODO: println(b[4]/10);
// TODO: return 10/0 lub var a = 10/0

fun main(): Unit {
    for (i in [1..10]) {
        if (true) {
//            val a: Int = (i/0);
            println(b[4]/10);
            //foo();
//            foo();
//            assert(10==0);
            //a++;
        }
    }
}