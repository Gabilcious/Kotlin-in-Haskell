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


fun main(): Unit {
   //b[10]=15; // TODO
    //val x: Int = b[10];
    println(b[-10]);
}