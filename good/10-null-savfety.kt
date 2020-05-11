fun main() : Unit {
    var str_null: String?;
    assert(str_null == null);

    str_null = "mama";
    assert(str_null == "mama");

    str_null = null;
    assert(str_null == null);
}
