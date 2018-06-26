# Release Notes of cplex-scala: A Scala library for IBM ILOG CPLEX

## cplex-scala v1.2.1-SNAPSHOT

  * Functional programming: add iterators for cumul function expression, state function, step function and piecewise 
  linear function. Furthermore, these functions are now iterables and this allow to iterate like 
  this:
  ```scala
     val f = model.numToNumStepFunction()
     ...
     for (s <- f) {
        println("[" + s.start + " .. " + s.end + ") -> " + s.value)
     }
  ```
  * Add type `IntSet`, factory methods and iterator.
  * Add scala API for inverse constraint and add example `Talent.scala`.
  * port to CPLEX 12.8
  
  
## cplex-scala v1.2.0

### README

 * Add a link to IBM ILOG CPLEX Studio Community Edition download page in the README

### Constraint Programming

 * Add state function
 * and constraints `alwaysIn`, `alwaysEqual`, `alwaysConstant` and `alwaysNoState` on state functions
 * Add example `SchedState.scala`
 * Add missing precedence constraints in companion object of CpModel
 * Add constraints `alwaysEqual` on cumul functions
 * Add integer division on integer expression

## cplex-scala v1.1.0

### README

  * Use syntax highlighting for scala code blocks
  * Update README: specify version of Java JDK used for testing

### Constraint Programming

  * Add constraint forbidStart, forbidEnd, forbidExtent
  * Replace type NumToNumStepFunction with a class, define operators (+=, -=, *=, +, -...) and a proper toString method that prints the step function in a human readable format.
  * Add example SchedCalendar.
  
## cplex-scala v1.0.1

### README

  * Update math equation
  * Add support to IBM ILOG CPLEX 12.7. 
  * Update build.gradle to optionally configure maven repository.
  
## cplex-scala v1.0.0

Initial release of the scala library for IBM ILOG CPLEX.