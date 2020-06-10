# NewKotlin-in-Haskell
Implementation of interpreter of some functionality of Kotlin language in Haskell

# NewKotlin
Kotlin is a compiled, statically typed language which was born from Java. NewKotlin is language I created from some of Kotlin's functionalities.

# Hello world
NewKotlin demands to have <i>main</i> function in interpreted file. This is function which is called at the beginning. Below is an example of basic function in NewKotlin. 
```
fun main(): Unit {
    println("Hello World!");
}
```
will result
```
Hello World!
```

# Declaring variables
In NewKotlin all variables must be declared and their type should be specified.
```
var number: Int = 42;
var message: String = "Hello World!";
```
When you want to change value of this variables, you cannot change the type. Possible assign operators are: <i>= += -= *= /= </i> and <i>%=</i>.
```
number = 10;
number -= 20;
message = "Goodbye World!";
```

## Read-only variables
There also exists read-only variables. They cannot be change during program runtime. You can declare them with <i>val</i> (for <i>"value"</i>) instead:
```
val number: Int = 42;
val message: String = "Hello World!";
```

## Functions
Functions can be declared in two way: using <i>var/val</i> and using <i>fun</i>. The second one is not allowed inside body of other function. Type of the function is (list_of_args_types)->return_type. Fun cannot be redeclared.
```
val foo: (Int, String)->Int = {num: Int, msg: String -> num; };
fun bar(num: Int, msg: String): Int {
    return x;
}
foo(number, message);
```
Both declaration above are the same. To understand how right side of first declaration works, read <i>Lambda</i>

# Primitive data types
In NewKotlin exists <i>Int</i>, <i>Bool</i>. This data types works exactly as in other programming languages:
```
var number: Int = 42;
var flag: Bool = true;
```

# String
In NewKotlin string is set of characters that cannot be divided into chars.
```
var message: String = "Hello "; // Hello 
message += "World!"     // Hello World!
```

# Tupla
This type is similar to Python tuplas, but cannot be unpacked. <i>Tupla</i>. is an pack of two variables, where one of them can also be <i>Tupla</i>.
```
var tupla: Tupla<Int, String> = Tupla(number, message);
var anotherTupla: Tupla<Tupla<Int, String>, Int> = Tupla(tupla, number);
```

# Array
Array are build of some number of values same type. To create Array you need to specify it size and function to determine how their elements will look like. This is an example of Array size 10, filled with even numbers from 2 to 20:
```
var array: Array<Int> = Array(10, {i: Int -> 2*i; }
```
To understand how second parameter in the constructor works, read <i>Lambda</i>

# Conditionals
You can compare variables with == and !=. Boolean expressions are formed with && for logical AND, || for logical OR, and ! for logical NOT.

## If/else
If/else works the same way in other languages. The conditions are enclosed in parentheses and after if statement can be else statement.
```
val number: Int = 42;
if (number %2 == 0) {
    println("This number is even");
} else {
    println("This number is odd");
}
```

## Ternaty operator
You can also use ternary operator:
```
val number: Int = 42;
val message: String = number %2 == 0 ? "This number is even" : "This number is odd";
```

# Loops
## for
For loop in NewKotlin is similar to python's one. For iterates over anything that is iterable.
```
val names: Array<String> = (0, {i: Int -> "Jan Kowalski"});
for (name in names) {
    println(name)
}
```
Iterable can be both array and range.

### Range
You can create range in .. operator:
```
for (x in [0..10]) { println(x); } // Prints 0 through 10 (inclusive)
```
If you want to exclude last value, you can use until:
```
for (x in [0 until 10]) { println(x); } // Prints 0 through 10 (exclusive)
```
You can change an increment with step (must be positive).
```
for (x in [0 until 10 step 3]) { println(x); } // Prints 0 3 6 9
```
And if you need to count down you can use downTo.
```
for (x in [9 downto 0 step 3]) { println(x); } // Prints 9 6 3
```

## while
```
var i = 0
while (i < 10) {
    println(i);
    i++;
}
```

## continue and break
Continue inside the loop skips to the next iteration of the most inner loop, while break stops it. They can be used only inside of some loop.
```
var i = 0
while (true) {
    println(i);
    i++;
    if (i>10) {
        break;
    }
}
```

# Functions
As was previously said functions can be declared both using fun and val/var. Every function which return types is different than Unit MUST return something.
```
fun fib(d: Int) : Int {
    if (d == 1 || d == 2) {
        return 1;
    } else {
        return fib (d - 1) + fib (d - 2);
    }
}
```

## Lambda
Lambda is a function which has no name. It's body is inside bracelets. It should have specified all arguments with their types.
```
val array: Array<Array<Int?>> = Array(
        n,
        { x: Int ->               // Lambda
            Array(5, {y: Int ->.  // Another lambda
                                var ret : Int?;
                                if (x + y != 4) { ret = y; }
                                ret;
                            }
            );
        }
);
val foo: (Int, String)->Int = {num: Int, msg: String -> num; };
```
It does not use return statement. The return value is automaticaly detected based on last line in the lambda.

# Null safety
## Nullable types
Null safety is main difference between NewKotlin and other programming languages. Every value can be Nullable or Not-nullable. Nullable (where null can be assigned) type is specified by ? after typename.
```
val number: Int? = null;
val number: Int?;
if (number != null) {
    println(number);
}
```
Both declaration above are the same. When no initial value is specified in nullable type in declaration, null is automaticly assigned.

## Calling functions with nullable types
Calling functions where args is nullable or not-nullable with nullable types are both allowed.
```
var notnull: String = "Hello";
var nullable: String? = "Hello";
fun foo(a: String?) {}
foo(notnull);
foo(nullable);
```
But calling function where args are not-nullable with nullable types are prohibited.
```
var nullable: String? = "Hello";
fun foo(a: String) {}
foo(nullable);  // ERROR
```

## Safe call operator: ?.let {..}
This is save cast to not-nullable value. If value is null statements are not called and if value is not null it's automaticly casted to not-null type and statements are called.
```
var nullable: String? = "Hello";
nullable?.let {
    println(nullable); // type String not String?
}
var nullable: String?;
nullable?.let {
    println(nullable); // Never called
}
```

## Not-null assertion operator: !!
When you are sure that your variable is not null (even when type is nullable) you can use !! operator to force cast form nullable type to not-nullable.
```
val number: Int? = 42;
fun f(x: Int) : Unit {}
f(number!!); // This is allowed
```
But be carefull. It can couse NullPointer Exception when value is null.
