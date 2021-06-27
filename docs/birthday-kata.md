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
#### Decision 1 - Record Types
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

#### Decision 2 - Nominal vs Structural Typing
Also note that we have opted in to using nominal typing (with the `unique` keyword) instead of structural typing. If we had left out `unique` the compiler would consider `EmployeeId`, `FirstName`, and `LastName` all to be the same type. For our current use case, we want to make sure that the compiler considers all of these different types, so we can prevent mistakes like accidentally passing in a `LastName` where an `EmployeeId` should go.

#### Decision 3 - Avoiding Primitives
We could have also modeled the Employee like this (or any other similar way)
```unison
unique type Employee  = 
  { id: Text
  , name: Name
  , birthday: Date 
  }

unique type Name = 
  { first: Text
  , last: Text
  }

unique type Date = 
  { day: Nat
  , month: Nat
  , year: Nat
  }
```

When deciding whether to use primitives or to wrap them in more rich domain types we should consider some tradeoffs
1) Using primitive types gives the compiler less context on what you are attempting to model. For example, using `Text` for the id, the first name, and the last name, means the compiler wouldn't catch an easy mistake like
```unison
foo = 
    id = "12345"
    firstName = "John"
    lastName = "Lennon"
    employee = Employee id (Name lastName firstName) (Date 1 1 1991)
```
(We've transposed the employees name!)
Or even worse, maybe we accidentally swap the id and last name! However, the compiler would catch this problem.

```unison
foo = 
    id = EmployeeId "12345"
    firstName = FirstName "John"
    lastName = LastName "Lennon"
    employee = Employee id (Name lastName firstName) (Date 1 1 1991)
    ()
```
```ucm
  The 1st argument to `Name`
  
            has type:  LastName
      but I expected:  FirstName
  
      9 | unique type LastName = LastName Text
     10 | unique type Name = 
     11 |   { first: FirstName
      .
     23 |     employee = Employee id (Name lastName firstName) (Date 1 1 1991)
```
Of course, you can still make a mistake like
```unison
firstName = LastName "Lennon"
```
But it's much harder to make this mistake. If we are ok with the extra types and the overhead of deconstructing them, we can catch many more typos/bugs at compile time, which means fewer tests are necessary to ensure correctness!

2) In the future, Unison may introduce the ability to make to make data constructors private and/or refined types. Doing so would allow us to to avoid another huge source of bugs when using primitive types directly, which is that the set of possible values is much larger than what we are attempting to model. You can see this in our model for `Date`

```unison
unique type Date = 
  { day: Nat
  , month: Nat
  , year: Nat
  }
```

By modelling day/month/year all as `Nat` we have introduced the possibility of an illegal state into our system. For example, in this model, we could have illegal dates like `0/500/99999999999999`. In fact, lets try to restrict the possible values a bit, to reduce the surface area for future bugs. This means more tests and more runtime validation (and runtime validation failures).

Lets try to contrain the possible values a bit.
```unison
unique type Date = 
  { day: Day
  , month: Month
  , year: Year
  }
unique type Month 
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
unique type Day = Day Nat
unique type Year = Year Nat
```

It is really easy for us to just enumerate all of the possible months. Now we have eliminated the possiblity of an invalid month appearing in our system! Now we have to decide how far we want to go to make illegal state unrepresentable. Maybe `Nat` is ok for year if we don't care about `BCE` and we are ok with possibly talking about birthdays millions of years in the future (not ideal). But it would be nice to disallow days of months that don't exist, and this is where refined types, dependent types, or private constructors could be leveraged. Hopefully, we will have those one day!

### Email
Let's apply some similar decision making to get an Email.
```unison
unique type Email = 
  { to: EmailAddress
  , subject: Subject
  , body: Body
  }

unique type EmailAddress = EmailAddress Text
unique type Subject = Subject Text
unique type Body = Body Text
```

It would be nice to add some constraints to the email address (like a regex to ensure only valid email addresses can exist) but this is pretty good so far!

But maybe we can build a little bit toward the future here. The prompt told us the future will likely include sending birthday messages via other platforms, like SMS or robocalls.

Maybe our model should actually be about a `Message` of which there is some relationship with an `Email`. One possible way to model this would be
```unison
unique type Message 
  = Email EmailAddress Subject Body
```
which would give us the option of extending it in the future as needed
```unison
unique type Message 
  = Email EmailAddress Subject Body
  | SMS PhoneNumber Body
  | TwitterDM TwitterHandle Body
```

### Summary
There isn't one "best" way to model this, but this is one possible way:
```unison
unique type Employee = 
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
  { day: Day
  , month: Month
  , year: Year
  }
unique type Month 
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
unique type Day = Day Nat
unique type Year = Year Nat

unique type Message 
  = Email EmailAddress Subject Body

unique type EmailAddress = EmailAddress Text
unique type Subject = Subject Text
unique type Body = Body Text
```