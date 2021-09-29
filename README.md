# repo-hwang241

# Course Description and Objectives

 This is a course to be taken by computer science majors around the end of the sophomore year. It will use a functional language to introduce a high-level approach to programming over complex data. It will emphasize a view of such data that abstracts away from their representation, using types as a vehicle for organizing them as values and for structuring computations over them. Advanced programming techniques that use ideas such as recursion, higher-order functions, lazy and eager forms of evaluation and infinite data objects will be explored. The possibility of exploiting parallelism arising from pure forms of expression evaluation will be examined. Other techniques and principles to be studied include search-based programming, modularity and concurrency. Programming projects that focus on symbolic computation will be used to impart the core ideas in the course; such projects may include writing parsers, type-checkers and interpreters for suitably circumscribed programming languages, and applications of search-based techniques.

# Course Content

### Listed below are the topics that I plan to cover during the term. Don't read this as a weekly or even a linear schedule: the topics are interrelated and so we may often cover them in parallel.


* Types as a mechanism for organizing programs: Structuring data around types and structuring programs around data. Richness in typing through recursive, higher-order and polymorphic types.

* Computation as evaluation of expressions free of side-effects: Principles for structuring expressions such as binding of names, scopes, and environments. Richness in expressions through recursion, varieties of recursion. Implicit parallelism.

* Treating functions as first class objects: Embedding functions in data, passing them as arguments to other functions, returning them as results. Benefits in programming, explored via common higher-order functions such as map, filter, fold. Applications of higher-order functions in realizing parametric polymorphism and exposing control in evaluation.

* Proving properties of recursive programs: Designing functions around invariants, reasoning about invariants using induction. Types as a variety of invariants.

* Variations in evaluation strategies and their use in programming: Eager and lazy evaluation and their justification via strict and non-strict interpretation of functions. Programming techniques exploiting lazy evaluation, infinite data structures such as streams. Simulating laziness in eager languages.

* Introducing side-effects into computation: Type safe references, assignments, other side-effecting constructs, iterative control structures. Modelling effectful computation via state transforming functions, introducing side-effects into lazy languages. Object-oriented programming as combining environments with state. Circular data structures and their realization through references.

* Analyzing the complexity of programs: Estimating execution time via recurrence relations associated with recursive functions. Mutable and immutable data and their impact on programming techniques. Functional data structures.

* A glimpse of the implementation of high-level value-based programming: Mapping conceptual data objects to memory, memory usage, copying versus pointing. Garbage creation and automatic collection, memory management.
* Search-based computation: Search as a computational paradigm and its applications. Programming techniques for realizing search.
* Role of modularity in programming-in-the-large: Interface specifications, abstract data types. Language support for modular programming, interface checking as type checking. Module composition as function application.
* Concurrency. Asynchronous computation as a paradigm, coordination through communication; language mechanisms for organizing and controlling communication.
* Translation of principles into programming in mainstream, non-functional languages.

To ground our discussions, we will need to write programs in a real programming language. The language we will used is called OCaml.

Something to realize with regard to the list of topics above: these are topics that several of my colleagues and I concluded a few years ago would be great to provide you exposure to at this stage of your learning but not all of them have to be covered to make this a successful course. We will make some decisions as the semester moves along about what to focus on more sharply and, correspondingly, what to leave out to create more time for the selected topics. In making these decisions, I will take into account what absolutely must be covered and also where I sense the interests of the class to lie.