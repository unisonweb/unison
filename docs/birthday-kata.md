# Practical Example - Birthday Kata

## The Kata
[Source: Matteo Vaccari](http://matteo.vaccari.name/blog/archives/154)

### Goals
This kata is normally used to practice TDD, dependency injection, and ports and adapters architectures in an Object Oriented style. Instead, we will tackle the same problem in Unison and explore TDD, dependency rejection, and how pure FP languages naturally encourage ports and adapters like architectures (through seperation of pure and impure functions).

At a more technical level, we will explore how to do domain modeling with the Unison type system, how to use abilities, and how to write tests.

### The Prompt

As the HR department, I want to send an email to every employee on their birthday so that they feel appreciated and company morale is kept high.

Technical Notes:
1) Loads a set of employees from somewhere (we haven't decided yet if its a database, or a flatfile, or just in memory)
2) Sends an email to all employees whose birthday is today.

Platform Goals:
1) Testable; we should be able to test the internal application logic with no need to ever send a real email.
2) Flexible: we anticipate that the data source in the future could change from a flat file to a relational database, or perhaps a web service. We also anticipate that the email service could soon be replaced with a service that sends greetings through Facebook or some other social network.
3) Well-designed: separate clearly the business logic from the infrastructure.

For further information about this kata please checkout the original post [here](http://matteo.vaccari.name/blog/archives/154).

### Our Approach
First, we will come up with a rich domain model and discuss different ways we can model the system to take advantage of the compiler nd what the tradeoffs are.

Second, we will talk about Abilities, dependency injection / rejection, and how we can decouple possibly impure infrastructure from pure decision making logic.

Third, we will apply TDD to implement our behavior.

## Modeling
Unison provides us with all the algebraic data types we need to build and compose types. In lieu of discussing them up front, we will just jump into modeling and discuss syntax and decisions as we go!

From skimming over the prompt, it seems pretty clear that top level types we need are `Employee` and `Email`. Let's start with `Employee`.

### Employee
So what is an employee to us? For one, they are an entity and therefore should have a unique `id`. Some other fields we probably care about include the employee's `name`, and importantly, their `birthday`.

Obviously, there a millions of other fields we could choose to include in our model, but we would also like to only include what we care about. In a larger more complex system, the Employee model may have many other fields we don't care about but make sense to include in the broader modelling context (see "bounded context", Domain Driven Design).

So lets start with:
```unison
unique type Employee  = 
  { id: EmployeeId
  , name: Name
  , birthday: Date 
  }

unique type EmployeeId = EmployeeId Text
unique type FirstName = FirstName Text
unique type LastName = LastName Text
unique type Name = 
  { first: FirstName
  , last: LastName
  }
unique type Date = 
  { day: Nat
  , month: Nat
  , year: Nat
  }
```

Our employee is a record type consisting of an `id`, `name`, and a `birthday`. In Unison, the record type syntax is actually equivalent to a normal data type like
```unison
type Employee = Employee EmployeeId Name Date
```
except the compiler will automatically generate a collection of functions (lens) that allow for easy access and updates of this immutable data structure, which will be a big help later. You can see this in the output in `ucm`:
```ucm
  I found and typechecked these definitions in ~/projects/scratch.u. If you do an `add` or
  `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Date
      unique type Employee
      unique type EmployeeId
      unique type FirstName
      unique type LastName
      unique type Name
      Date.day                 : Date -> Nat
      Date.day.modify          : (Nat ->{g} Nat) -> Date ->{g} Date
      Date.day.set             : Nat -> Date -> Date
      Date.month               : Date -> Nat
      Date.month.modify        : (Nat ->{g} Nat) -> Date ->{g} Date
      Date.month.set           : Nat -> Date -> Date
      Date.year                : Date -> Nat
      Date.year.modify         : (Nat ->{g} Nat) -> Date ->{g} Date
      Date.year.set            : Nat -> Date -> Date
      Employee.birthday        : Employee -> Date
      Employee.birthday.modify : (Date ->{g} Date) -> Employee ->{g} Employee
      Employee.birthday.set    : Date -> Employee -> Employee
      Employee.id              : Employee -> EmployeeId
      Employee.id.modify       : (EmployeeId ->{g} EmployeeId) -> Employee ->{g} Employee
      Employee.id.set          : EmployeeId -> Employee -> Employee
      Employee.name            : Employee -> Name
      Employee.name.modify     : (Name ->{g} Name) -> Employee ->{g} Employee
      Employee.name.set        : Name -> Employee -> Employee
      Name.first               : Name -> FirstName
      Name.first.modify        : (FirstName ->{g} FirstName) -> Name ->{g} Name
      Name.first.set           : FirstName -> Name -> Name
      Name.last                : Name -> LastName
      Name.last.modify         : (LastName ->{g} LastName) -> Name ->{g} Name
      Name.last.set            : LastName -> Name -> Name
```

`Name` and `Date` are record types as well.

Also note that we have opted in to using nominal typing (with the `unique` keyword) instead of structural typing. If we had left out `unique` the compiler would consider `EmployeeId`, `FirstName`, and `LastName` all to be the same type. For our current use case, we want to make sure that the compiler considers all of these different types, so we can prevent mistakes like accidentally passing in a `LastName` where an `EmployeeId` should go.