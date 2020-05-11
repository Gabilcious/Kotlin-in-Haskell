fun main() : Unit {
    var k: Int = 10;

    if ((k = 30) < 100) { }
    assert(k == 30);

    if (true && (k = 40) < 100) { }
    assert(k == 40);

    if (false || (k = 50) < 100) { }
    assert(k == 50);

    if (true || (k = 60) < 100) { }
    assert(k == 50);
}