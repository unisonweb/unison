# Practical Example - Modeling a Birthday Message Service

## The Kata
[Source: Matteo Vaccari](http://matteo.vaccari.name/blog/archives/154)

### Goals
This kata is normally used to practice TDD, dependency injection, and ports and adapters architectures in an Object Oriented style. Instead, we will tackle the same problem in Unison and explore TDD, dependency rejection, and how pure FP languages naturally encourage ports and adapters like architectures (through seperation of pure and impure functions).

If you aren't familiar with "ports and adapters" (also called "Hexagonal Architecture", "The Clean Architecture", and "The Onion Architecture") the idea is to invert dependencies between pure business logic and infrastructure code, usually using interfaces, so that implementation details like the DB, transfer protocols, etc. is fully decoupled from the actual business logic -- making it easier to extend, update, maintain, and test!

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
First, we will come up with a rich domain model and discuss different ways we can model the system to take advantage of the compiler and what the resulting tradeoffs are.

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
unique type Employee  = 
  { id: EmployeeId
  , name: Name
  , birthday: Date 
  }
```
#### Decision 1 - Record Types
Our employee is a record type consisting of an `id`, `name`, and a `birthday`. In Unison, the record type syntax is actually equivalent to a normal data type like
```unison
type Employee = Employee EmployeeId Name Date
```
except the compiler will automatically generate a collection of functions that allow for easy access and updates of this immutable data structure, which will be a big help later. You can see this in the output in `ucm`:
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
Also note that we have opted in to using nominal typing (with the `unique` keyword) instead of structural typing. If we had left out `unique` the compiler would consider `EmployeeId`, `FirstName`, and `LastName` all to be the same type (with all names being interchangeable). For our current use case, we want to make sure that the compiler considers all of these different types, so we can prevent mistakes like accidentally passing in a `LastName` where an `EmployeeId` should go.

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

Let's also update `Employee` to have an email address
```unison
unique type Employee  = 
  { id: EmployeeId
  , name: Name
  , birthday: Date 
  , email: EmailAddress
  }
```

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

## Abilities
Now that we have our model, let's quickly talk about the effects/infrastructure we know we will need.

One of the goals of this exercise (and generally working as a software engineer) is to decouple infrastructure from our business logic (which is often approximately the same thing as spearating pure and impure code). In OO we would use interfaces to define contracts and dependency inject in whatever implementations we need (be it postgres, aws, in memory, mocks, etc).

In Unison, we leverage Abilities (also called Algebraic Effects) to accomplish something similar. See [the abilities documentation](ability-typechecking.markdown) for a more detailed overview of Abilities.

In our case, let's limit ourselves to the following effects: sending a message, fetching employees from somewhere (a flatfile, a db, an api, anything), and fetching the current date. For the sake of simplicity let's ignore an exception throwing ability (and failures in general) or any other abilities that may make sense to add in a real production system.

```unison
ability MessageService where
  send: Message -> Message

ability EmployeeRepository where
  fetchAllByBirthday: (Day, Month) -> [Employee]

ability Calendar where
  today: Date
```

Let's walk through what these abilities are doing:
1) `MessageService` is an ability that provides the interface for a single command `send`. `send`'s type signature can be read as "a function that takes a message and returns a (same) message, requiring the `MessageService` ability."
2) `EmployeeRepository` provides the interface `fetchAllByBirthday` which takes a `Day` and a `Month` pair and returns a list of `Employees`, requiring the `EmployeeRepository` ability.
3) `Calendar` provides the interface `today` which is a function that takes no arguments and returns a `Date`, requiring the `Calendar` ability.

Note that abilities 
1) can provide multiple interfaces if we wanted
```unison
ability EmployeeRepository where
  fetchAllByBirthday: (Day, Month) -> [Employee]
  fetchAll: [Employee]
  findById: EmployeeId -> {Employee}
```
2) have their behavior defined elsewhere (using handler syntax we will get to later)
3) are composable (hence "algebraic effects")
4) require that any calling function provide the ability in their type signature, which bubbles all the way to the top of the call stack (until it finds a handler).

So let's scaffold our main function for this exercise and see this stuff in action.

```unison
sendBirthdayEmails : [Message]
sendBirthdayEmails = 
  todo "get the current date"
  todo "fetch all the employees with birthdays today"
  todo "create happy birthday messages"
  todo "send and return the messages"
```
Let's say we want a function we can call that will find all the employees whose birthday is today, create emails for them, send those emails, and then return those emails so we can see what was created and sent.

As an aside, let's avoid modeling failures simply because it will clutter this exercise a bit. In real life, many of our effects would have type signatures with `Either` so we could model possible failures, but let's just naively pretend everything always works.

So let's do some more scaffolding!
```unison
sendBirthdayEmails : [Message]
sendBirthdayEmails = 
  today' = today
  employees = fetchAllByBirthday (Date.day today', Date.month today')
  messages = map (Employee.toBirthdayMessage) employees
  map (send) messages

Employee.toBirthdayMessage : Employee -> Message
Employee.toBirthdayMessage _ = todo "toBirthdayMessage"
```
So we will grab the date, use that fetch all the employees with birthdays today, map those employees to messages, and then send out (and return) those messages.

Uh oh! The compiler is mad!
```ucm
The expression in red needs the {Calendar} ability, but this location does not have access to any abilities.
  
    136 |   today' = today)
```

Ah, turns out that any function with an ability in its signature can only be called by another function with that ability available in its signature. So we need to add all our abilities to the function signature. So let's just go ahead and add all three.

```unison
sendBirthdayEmails : {MessageService, EmployeeRepository, Calendar} [Message]
```

But we are stil getting the same error :(

Well, it turns out that these effects can actually only be run in the context of a handler. Until we wire that up, we can instead just defer the computation (so all effects become deferred).

```unison
sendBirthdayEmails : '{MessageService, EmployeeRepository, Calendar} [Message]
sendBirthdayEmails =  'let
  today' = today
  employees = fetchAllByBirthday (Date.day today', Date.month today')
  messages = map (Employee.toBirthdayMessage) employees
  map (send) messages

Employee.toBirthdayMessage : Employee -> Message
Employee.toBirthdayMessage _ = todo "toBirthdayMessage"
```

And the compiler seems happy so far! Note that we just deferred the entire computation by putting the `'let` at the beginning.

Let's move on applying TDD in order to implement the remaining code.

## Implementation!
Let's write a first failing test!
```unison
test> sendBirthdayEmails.tests.noBirthdaysToday = 
  check let
    actual = !sendBirthdayEmails
    expected = []
    if expected === actual
    then true
    else bug (exected, actual)
```

```ucm
  The expression in red  needs these abilities: {Calendar, EmployeeRepository, 𝕖457}, but this location does not have access to any abilities.
  
     70 |     actual = !sendBirthdayEmails
```

Well we can't defer this call any more, this is the boundary of the system! So it's time to implement handlers for our abilities!

```unison
test> sendBirthdayEmails.tests.noBirthdaysToday = 
  check let
    actual = 
      handle 
        handle 
          handle 
            !sendBirthdayEmails 
          with (Calendar.handler.mock (Date (Day 1) January (Year 1991)))
        with (EmployeeRepository.handler.mock [])
      with MessageService.handler.mock
    expected = []
    if expected === actual
    then true
    else bug (expected, actual)

Calendar.handler.mock : Date -> {Calendar} k -> k
Calendar.handler.mock date k =
  h : Request {Calendar} k -> k
  h = cases
    {today -> resume} -> handle resume date with h
    { resume } -> resume
  handle k with h

EmployeeRepository.handler.mock : [Employee] -> {EmployeeRepository} k -> k
EmployeeRepository.handler.mock employees k =
  h : Request {EmployeeRepository} k -> k
  h = cases
    { fetchAllByBirthday birthday -> resume } ->
      birthdays = filter (employee -> (Date.day (Employee.birthday employee), Date.month (Employee.birthday employee)) === birthday) employees
      handle resume birthdays with h
    { resume } -> resume
  handle k with h

MessageService.handler.mock : {MessageService} k -> k
MessageService.handler.mock k =
  h : Request {MessageService} k -> k
  h = cases
    { send message -> resume } -> handle resume message with h
    { resume } -> resume
  handle k with h
```

This is a lot so lets dig into it a bit!

The syntax for a handler is `handle <fn> with <handler>`. I've wrapped our call to `!sendBirthdayEmails` in three nested handle blocks (we can make this cleaner later).

When you write a handler, your goal is to pattern match on the possible ability function signatures, and then "resume" the computation (or not) along with providing the expected value for the ability. Let's dig in to our mock Calendar handler:

```unison
Calendar.handler.mock : Date -> {Calendar} k -> k
Calendar.handler.mock date k =
  h : Request {Calendar} k -> k
  h = cases
    {today -> resume} -> handle resume date with h
    { resume } -> resume
  handle k with h
```
1) our mock calendar handler dates a `Date` as input. This is so we can specify the return value of `today`, so we can "mock" it in our tests.
2) the `k` is the "continuation", our handler is being consulted to try to handle an ability, and then we call k to "continue" the existing computation.
3) we define some handler `h` locally which does the actual bulk of the work. It pattern matches on any ability case we care about. Note that its signature has access to the `Calendar` ability.
4) we pattern match on two cases. The first is the `today` case. If we are handling the `today` method, all we are doing is handling the continuation (by calling resume) and providing the value for `today` which is the provided `date` since are just "mocking" the date in this handler. The continuation is called and handled recursively.
5) we have a base `resume` pattern where we are basically saying "if this is any other behavior, just bubble this request up in case another handler knows how to handle it."

In summary, `Calendar.handler.mock` takes a `Date`, and any function it handles that calls `today` will return that date.

Now let's look at the `EmployeeRepository`:
```unison
EmployeeRepository.handler.mock : [Employee] -> {EmployeeRepository} k -> k
EmployeeRepository.handler.mock employees k =
  h : Request {EmployeeRepository} k -> k
  h = cases
    { fetchAllByBirthday birthday -> resume } ->
      birthdays = filter (employee -> (Date.day (Employee.birthday employee), Date.month (Employee.birthday employee)) === birthday) employees
      handle resume birthdays with h
    { resume } -> resume
  handle k with h
```

This is another mock to make it easy to test.
1) This handler takes a list of employees, which it uses to generate the return value for `fetchAllByBirthday`.
2) Our `h` take a request with the `EmployeeRepository` ability.
3) We match on the `fetchAllByBirthday` method and it's argument `birthday`.
4) When we match on that method, we filter the provided list of employees based on the `birthday` arg, and resume the computation with those employees as the return value.
5) In real life, we might read from a DB here.
6) Otherwise we bubble up the function in case other handlers know how to handle it.


And finally
```unison
MessageService.handler.mock : {MessageService} k -> k
MessageService.handler.mock k =
  h : Request {MessageService} k -> k
  h = cases
    { send message -> resume } -> handle resume message with h
    { resume } -> resume
  handle k with h
```

1) This handler does not take any args because it doesn't need any outside info to handle the `send` function.
2) We match on the `send` function and its message argument and resume the computation with that same message as a return value
3) A real version of this might instead reach out to Twilio or some external service to perform an effect before continuing.
4) And we bubble up any functions we don't care about.

Great! And then we just wrap all these handlers around our birthday function.
```unison
test> sendBirthdayEmails.tests.noBirthdaysToday = 
  check let
    actual = 
      handle 
        handle 
          handle 
            !sendBirthdayEmails 
          with (Calendar.handler.mock (Date (Day 1) January (Year 1991)))
        with (EmployeeRepository.handler.mock [])
      with MessageService.handler.mock
    expected = []
    if expected === actual
    then true
    else bug (expected, actual)
```

And thats how we do "dependency injection" in unison! We can easily swap out these handlers with other behaviors at the boundary of our system!

Now that we have a passing test, let's take a minute to refactor a bit before we add some more interesting test cases.

All these nested handlers are kind of gross. Let's combine them into a single "multihandler".
```unison
sendBirthdayEmails.handlers.mock : Date -> [Employee] -> '{Calendar, EmployeeRepository, MessageService} k -> k
sendBirthdayEmails.handlers.mock date employees k =
  h : Request {Calendar, EmployeeRepository, MessageService} k -> k
  h = cases
    {today -> resume} -> handle resume date with h
    { send message -> resume } -> handle resume message with h
    { fetchAllByBirthday birthday -> resume } ->
      birthdays = filter (employee -> (Date.day (Employee.birthday employee), Date.month (Employee.birthday employee)) === birthday) employees
      handle resume birthdays with h
    { resume } -> resume
  handle !k with h

test> sendBirthdayEmails.tests.noBirthdaysToday = 
  check let
    today = (Date (Day 1) January (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today []
    expected = []
    if expected === actual
    then true
    else bug (expected, actual)
```

Note that we have changed how we use the handlers to something slightly more idiomatic: a normal function call! Though I've chosen to pipe in the function as the last argument because I think it puts the "important" bit at the beginning of the line.

Note (again) that we updated the handlers signature to defer the first `k` so we no longer need to add `!` to the top level test since it is done inside the handler.

Alright lets test with the next test case, a single birthday today!
```unison
test> sendBirthdayEmails.tests.oneBirthdayToday = 
  check let
    employees = 
      [
        Employee 
        (EmployeeId "1") 
        (Name (FirstName "John") (LastName "Smith")) 
        (Date (Day 1) January (Year 1991))
        (EmailAddress "john@smith.com")
      , Employee 
        (EmployeeId "2") 
        (Name (FirstName "Fred") (LastName "Smith")) 
        (Date (Day 2) March (Year 1991))
        (EmailAddress "fred@smith.com")
      ]
    today = (Date (Day 1) January (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today employees

    expected =
      [
        Email 
        (EmailAddress "john@smith.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      ]
    if expected === actual
    then true
    else bug (expected, actual)
```

```ucm
  💔💥
  
  I've encountered a call to builtin.todo with the following value:
  
    "toBirthdayMessage"
```

That's a good failure! Let's fix it!
```unison
Employee.toBirthdayMessage : Employee -> Message
Employee.toBirthdayMessage employee =
  Email (Employee.email employee) (Subject "Happy Birthday!") (Body "Congrats! Love, HR.")
```

```ucm
    70 |   check let
    
    ✅ Passed : Proved.
  
    81 |   check let
    
    ✅ Passed : Proved.
```

It's working! Let's do one more test case just to be sure.

```unison
test> sendBirthdayEmails.tests.twoBirthdaysToday = 
  check let
    employees = 
      [
        Employee 
        (EmployeeId "1") 
        (Name (FirstName "John") (LastName "Smith")) 
        (Date (Day 1) January (Year 1991))
        (EmailAddress "john@smith.com")
      , Employee 
        (EmployeeId "2") 
        (Name (FirstName "Fred") (LastName "Smith")) 
        (Date (Day 2) March (Year 1991))
        (EmailAddress "fred@smith.com")
      , Employee 
        (EmployeeId "3") 
        (Name (FirstName "Abe") (LastName "Lincoln")) 
        (Date (Day 2) March (Year 1776))
        (EmailAddress "abe@lincoln.com")
      ]
    today = (Date (Day 2) March (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today employees

    expected =
      [
        Email 
        (EmailAddress "fred@smith.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      , Email 
        (EmailAddress "abe@lincoln.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      ]
    if expected === actual
    then true
    else bug (expected, actual)
```

```ucm
    70 |   check let
    
    ✅ Passed : Proved.
  
    81 |   check let
    
    ✅ Passed : Proved.
  
    113 |   check let
    
    ✅ Passed : Proved.
```

Great! And of course there a ton more tests you can add to make sure this is working for various corner cases, but this seems pretty good for now!

So let's go back and refactor our main function a bit now that we have the stability of some tests.
```unison
sendBirthdayEmails : '{Calendar, EmployeeRepository, MessageService} [Message]
sendBirthdayEmails =  'let
  today
    |> toDayMonthPair
    |> fetchAllByBirthday
    |> toBDayMessages
    |> sendAll

toDayMonthPair : Date -> (Day, Month)
toDayMonthPair = cases
  Date day month _ -> (day, month)

toBDayMessages : [Employee] -> [Message]
toBDayMessages employees = 
  map (Employee.toBirthdayMessage) employees

sendAll : [Message] ->{MessageService} [Message]
sendAll messages =
  map (send) messages
```

Looks a lot cleaner, its easier to read, and its composed of a bunch of small modular easy to understand functions!

## Conclusion
Let's see it all together!
```unison
unique type Employee  = 
  { id: EmployeeId
  , name: Name
  , birthday: Date 
  , email: EmailAddress
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

ability MessageService where
  send: Message -> Message

ability EmployeeRepository where
  fetchAllByBirthday: (Day, Month) -> [Employee]

ability Calendar where
  today: Date

sendBirthdayEmails : '{Calendar, EmployeeRepository, MessageService} [Message]
sendBirthdayEmails =  'let
  today
    |> toDayMonthPair
    |> fetchAllByBirthday
    |> toBDayMessages
    |> sendAll

toDayMonthPair : Date -> (Day, Month)
toDayMonthPair = cases
  Date day month _ -> (day, month)

toBDayMessages : [Employee] -> [Message]
toBDayMessages employees = 
  map (Employee.toBirthdayMessage) employees

sendAll : [Message] ->{MessageService} [Message]
sendAll messages =
  map (send) messages

Employee.toBirthdayMessage : Employee -> Message
Employee.toBirthdayMessage employee =
  Email (Employee.email employee) (Subject "Happy Birthday!") (Body "Congrats! Love, HR.")

sendBirthdayEmails.handlers.mock : Date -> [Employee] -> '{Calendar, EmployeeRepository, MessageService} k -> k
sendBirthdayEmails.handlers.mock date employees k =
  h : Request {Calendar, EmployeeRepository, MessageService} k -> k
  h = cases
    {today -> resume} -> handle resume date with h
    { send message -> resume } -> handle resume message with h
    { fetchAllByBirthday birthday -> resume } ->
      birthdays = filter (employee -> (Date.day (Employee.birthday employee), Date.month (Employee.birthday employee)) === birthday) employees
      handle resume birthdays with h
    { resume } -> resume
  handle !k with h

test> sendBirthdayEmails.tests.noBirthdaysToday = 
  check let
    today = (Date (Day 1) January (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today []
    expected = []
    if expected === actual
    then true
    else bug (expected, actual)

test> sendBirthdayEmails.tests.oneBirthdayToday = 
  check let
    employees = 
      [
        Employee 
        (EmployeeId "1") 
        (Name (FirstName "John") (LastName "Smith")) 
        (Date (Day 1) January (Year 1991))
        (EmailAddress "john@smith.com")
      , Employee 
        (EmployeeId "2") 
        (Name (FirstName "Fred") (LastName "Smith")) 
        (Date (Day 2) March (Year 1991))
        (EmailAddress "fred@smith.com")
      ]
    today = (Date (Day 1) January (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today employees

    expected =
      [
        Email 
        (EmailAddress "john@smith.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      ]
    if expected === actual
    then true
    else bug (expected, actual)

test> sendBirthdayEmails.tests.twoBirthdaysToday = 
  check let
    employees = 
      [
        Employee 
        (EmployeeId "1") 
        (Name (FirstName "John") (LastName "Smith")) 
        (Date (Day 1) January (Year 1991))
        (EmailAddress "john@smith.com")
      , Employee 
        (EmployeeId "2") 
        (Name (FirstName "Fred") (LastName "Smith")) 
        (Date (Day 2) March (Year 1991))
        (EmailAddress "fred@smith.com")
      , Employee 
        (EmployeeId "3") 
        (Name (FirstName "Abe") (LastName "Lincoln")) 
        (Date (Day 2) March (Year 1776))
        (EmailAddress "abe@lincoln.com")
      ]
    today = (Date (Day 2) March (Year 2021)) 
    actual = sendBirthdayEmails |> sendBirthdayEmails.handlers.mock today employees

    expected =
      [
        Email 
        (EmailAddress "fred@smith.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      , Email 
        (EmailAddress "abe@lincoln.com")
        (Subject "Happy Birthday!")
        (Body "Congrats! Love, HR.")
      ]
    if expected === actual
    then true
    else bug (expected, actual)
```

## References
[Scott Wlaschin - Domain Modeling Made Functional](https://www.youtube.com/watch?v=Up7LcbGZFuo&ab_channel=NDCConferences)

[Mark Seemann - Dependency Injection to Dependency Rejection](https://www.youtube.com/watch?v=cxs7oLGrxQ4)

[Mark Seemann - Pits of Success](https://www.youtube.com/watch?v=US8QG9I1XW0&ab_channel=NDCConferences)

[Unison slack thread that contributed a lot of code examples](https://unisonlanguage.slack.com/archives/CLKV43YE4/p1624391625329100?thread_ts=1624385627.328200&cid=CLKV43YE4)