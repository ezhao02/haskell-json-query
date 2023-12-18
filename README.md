# project-cis5520

## Authors
- Henghak Kun, kunhk
- Eric Zhao, zhaoer

## Brief description

This application takes in a JSON file and an HJQL file (containing potentially
multiple queries) and runs the queries sequentially on the JSON file. The app
prints the results of each query to the terminal. There are three types of
instructions: read, write, and delete. Write and delete instructions update
the JSON file as they are run.

## Module organization

The files/main components of this project should be read in the following order:

  - The [JSONObject module][src/JSONObject.hs] defines the JSON type and a show
    instance so that JSONs can be represented in Haskell.
  - The [HJQL module][src/HJQL.hs] defines our query language, HJQL, as a type
    in Haskell. Additionally, it includes functions that run queries on JSON
    objects.

  - The [Parser module][src/Parser.hs] is based on the module used in previous
    homeworks, with additional useful parsers defined as well.
  - The [JSONParser module][src/JSONParser.hs] implements functions that parse
    JSONs from strings or from files.
  - Similarly, the [HJQLParser][src/HJQLParser.hs] module allows parsing of
    queries or lists of queries.
  
  - The entry point for the executable is in [Main.hs](app/Main.hs).
  
  - All of the test cases are in [the test directory](test/Spec.hs). These are
    split into modules based on what they test: [JSON parsing][test/JSONParserTest.hs],
    [HJQL parsing][test/HJQLParserTest.hs], and [running queries][test/HJQLTest.hs].

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`. Specifically, the usage is
`stack run JSON_FILE_NAME HJQL_FILE_NAME`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.


