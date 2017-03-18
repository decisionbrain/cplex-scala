## cplex-scala v1.1.1

 * Add state function
 * and constraints `alwaysIn`, `alwaysEqual`, `alwaysConstant` and `alwaysNoState` on state functions
 * Add example `SchedState.scala`
 * Add missing precedence constraints in companion object of CpModel
 * Add constraints `alwaysEqual` on cumul functions
 * Add integer division on integer expression
 * Add a link to IBM ILOG CPLEX Studio Community Edition download page in the README

## cplex-scala v1.1.0

###README

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