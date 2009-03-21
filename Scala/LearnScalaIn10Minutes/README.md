Learn Scala in 10 Minutes
=========================

Introduction
------------

This is an adaptation of [Learn Haskell in 10 Minutes][learn-haskell] for [Scala][scala].


Overview
--------

Scala is a general purpose programming language designed to express common programming patterns in a concise, elegant, and type-safe way. It smoothly integrates features of object-oriented and functional languages, enabling Java and other programmers to be more productive. Code sizes are typically reduced by a factor of two to three when compared to an equivalent Java application.

Scala runs on the [Java Virtual Machine][JVM].  You can download the Scala compiler from [http://www.scala-lang.org/downloads][scala-download].  Once you've installed Scala, you get two programs you're interested in right now: scalac, and scala. The first compiles Scala libraries or applications to Java byte code. The second is an interactive shell that lets you write Scala code and get feedback right away.  It is also capable of compiling Scala scripts into Scala applications.

Simple Expressions
------------------

To start the scala interactive shell, you run the command `scala`:

  $ scala

You can type most math expressions directly into the scala interactive shell and get an answer. scala> is the default prompt for the scala interactive shell.

    scala> 3 * 5
    res0: Int = 15

    scala> Math.pow(4, 2 - 1)
    res1: Double = 4.0

    scala> Math.pow(1 - 5, 3 * 2 - 4)
    res2: Double = 16.0

Strings are in "double quotes." You can concatenate them with `+`.

    scala> "Hello"
    res3: java.lang.String = Hello

    scala> "Hello" + ", Scala"
    res4: java.lang.String = Hello, Scala

Calling functions is done by putting the arguments after the function in parentheses:

    scala> Math.floor(6.59)
    res5: Double = 6.0

    scala> Math.round(6.59)
    res6: Long = 7

    scala> Math.sqrt(2.0)
    res7: Double = 1.4142135623730951

    scala> ! (5 < 3)
    res8: Boolean = true

The Console
-----------

I/O actions can be used to read from and write to the console. Some common ones include:

    scala> println("Hello, Scala")
    Hello, Scala

    scala> print("No newline")
    No newline
    scala> print(1 < 2)
    true

The `println` and `print` functions output strings to the terminal.

If you need multiple I/O actions in one expression, you can separate them with semicolons.

    scala> print("2 + 2 = "); print(2 + 2)
    2 + 2 = 4
    scala> println("ABCDE"); println("12345")
    ABCDE
    12345

Reading can be done with `readLine` (which gives back a String) or `readInt` (which gives back an Int).

    scala> var n = readInt(); print(Math.pow(n, 2))
    16.0n: Int = 4

(The 4 was input. The 16.0 was a result.)

There is actually another way to write this.  Try putting the file in a source file (say, Test.scala) and run it.

    println("What is 2 + 2?")
    var x = readInt()
    if(x == 4)
      println("You're right!")
    else
      println("You're wrong!")

You can run it with `scala Test.scala`.  You get an if expression as a bonus. 

Simple Types
------------

So far, not a single type declaration has been mentioned. That's because Scala does type inference. You generally don't have to declare types unless you want to. If you do want to declare types, you use : to do it.



[learn-haskell]: http://haskell.org/haskellwiki/Learn_Haskell_in_10_minutes
[scala]: http://www.scala-lang.org
[JVM]: http://java.sun.com
[scala-download]: http://www.scala-lang.org/downloads