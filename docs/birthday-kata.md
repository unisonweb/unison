# Practical Example - Birthday Kata

## The Kata
[Source: Matteo Vaccari](http://matteo.vaccari.name/blog/archives/154)

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