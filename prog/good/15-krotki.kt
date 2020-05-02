fun main() : Unit {
    var k: Tupla<Int, Tupla<String, Int>>? =
        Tupla(3, Tupla ("mama", 5));
    k = null;
    k = Tupla (0, Tupla ("tata", 0));
}
