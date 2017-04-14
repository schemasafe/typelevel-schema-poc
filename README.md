Toy-troy is simplified version of [Troy](https://github.com/cassandra-scala/troy/), the schema-safe Cassandra driver.
The main purpose of this repo to demonstrate the underlying concept of Troy by implementing only a small subset of it.

# What Troy allows you to do
You write your code using CQL strings
![developer code](read-me-pic-1.png)

# What Troy does under the hood
Using Macros, the CQL strings above gets expanded into:
![generated code](read-me-pic-2.png)

# Error messages
At compile time, you get error messages if you query is wrong
```
[error] <macro>:5: Table "lol" does not exist.
```
```
[error] <macro>:5: Column "foobar" does not exist in table "posts"
```
```
[error] <macro>:5: Row is not compatible with Output structure
```
```
[error] <macro>:5: Bind Markers are not compatible with Input structure
```

# Demo
[See it working here](demo/src/main/scala/Example.scala), you'll need running Cassandra to run the demo, but it compiles without connecting it.
