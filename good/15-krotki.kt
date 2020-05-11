fun main() : Unit {
    var k: Tupla<Int, Tupla<String, Int>>? =
        Tupla(3, Tupla ("mama", 5));
    println (k);

    k = null;
    println (k);

    k = Tupla (0, Tupla ("tata", 0));
    println (k);
}
