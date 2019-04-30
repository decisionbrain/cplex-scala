A scala library for IBM ILOG CPLEX. 

This library combines functional programming, mathematical programming and constraint programming allowing to 
model optimization problems with a syntax that is close to the standard presentation of these problems in textbooks and 
scientific papers. For instance, a constraint such as:
 

![](equation.gif)

can be written as:

```scala
    model.add(sum (for (i <- 1 to n) yield a(i) * x(i)) <= c(j))
```

To get up to speed, the easiest way to start with this library is to study the examples:
 
 * src/examples/mp: examples of optimization models based on mathematical programming
 * src/examples/cp: examples of optimization models based on constraint programming

This library has been tested using IBM ILOG CPLEX 12.9, Scala 2.12.8 and Java JDK 1.8.0_202 on Windows 10 
64 bits. Port to 32 bits or Linux should be fairly easy, see file `build.gradle`. Last revision compatible with 
IBM ILOG Decision Optimization Center 3.9 is release 1.2.0 (Java 6, Scala 2.11.8)

If you want to play with this library and do not have a license of CPLEX, you can download the  
[Community Edition of IBM ILOG CPLEX Optimization Studio](https://www-01.ibm.com/software/websphere/products/optimization/cplex-studio-community-edition/)

To build this library use the Gradle wrapper provided or install [Gradle 2.10](https://gradle.org) and set the environment variable `CPLEX_STUDIO_HOME` (e.g. 
on windows `C:\IBM\ILOG\CPLEX_Studio129`).  

Then do:

```
$ ./gradlew build
```

This will create the scala library in directory `build/libs`.


To run the tests, do:

```
$ ./gradlew test
```

Reports are generated in directory `build/reports/tests`.

To generate the scala docs, do:

```
$ ./gradlew scaladoc
```

The scala documentation is generated in directory `build/docs/scaladoc`.

To clean and rebuild the library, do:

```
$ ./gradlew clean build
```

For a complete list of Gradle tasks, do:

```
$ ./gradlew tasks
```


