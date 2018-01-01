import schemasafe.{TypedQuery, TypedQueryMaterializer}
import schemasafe.api._

object SchemasafeExample extends App {

  case class ByFoo(foo: Int)
  case class FooBar(foo: Int, bar: String)

  type testQuery1 = "select foo, bar from table x where foo = ?"
  // Dummy instance, should be derived instead
  implicit val successInstance = TypedQueryMaterializer.instance[
    testQuery1,
    ByFoo,
    FooBar,
    Right[Nothing, TypedQuery[testQuery1, ByFoo, FooBar, None.type, Array[Byte], String]]
   ](Right[Nothing, TypedQuery[testQuery1, ByFoo, FooBar, None.type, Array[Byte], String]](new TypedQuery[testQuery1, ByFoo, FooBar, None.type, Array[Byte], String] {
    override def rawQuery = "select foo, bar from table x where foo = ?"
    override def resultSetMaxSize = None
    override def inputEncoder: (ByFoo) => Array[Byte] = _ => Array.empty
    override def outputDecoder: (Array[Byte]) => Either[String, FooBar] = _ => Right(new FooBar(1, "bar"))
  }))

  type testQuery2 = "select foo, bar from table x where foo = ? limit 1;"
  // Dummy instance, should be derived instead
  implicit val successInstance2 = TypedQueryMaterializer.instance[
    testQuery2,
    ByFoo,
    FooBar,
    Right[Nothing, TypedQuery[testQuery2, ByFoo, FooBar, Some[1], Array[Byte], String]]
    ](Right[Nothing, TypedQuery[testQuery2, ByFoo, FooBar, Some[1], Array[Byte], String]](new TypedQuery[testQuery2, ByFoo, FooBar, Some[1], Array[Byte], String] {
    override def rawQuery = "select foo, bar from table x where foo = ? limit 1;"
    override def resultSetMaxSize = Some(1)
    override def inputEncoder: (ByFoo) => Array[Byte] = _ => Array.empty
    override def outputDecoder: (Array[Byte]) => Either[String, FooBar] = _ => Right(new FooBar(1, "bar"))
  }))

  // Dummy instance, should be derived instead
  implicit val failureInstance = TypedQueryMaterializer.instance[
    "select oops from table x where foo = ?",
    ByFoo,
    FooBar,
    Left["Column oops not found in table x", Nothing]
    ](Left["Column oops not found in table x", Nothing]("Column oops not found in table x"))

  val query0 = q("select foo, bar from table x where foo = ?")[ByFoo, FooBar].materialize
  println(query0.inputEncoder(ByFoo(4)))
  val out: FooBar = query0.outputDecoder(Array.empty).right.get
  println(out)


  val selection = q("select foo, bar from table x")
  val clause = q(" where foo = ")
  val query1 = (selection ++ clause :+ "?").apply[ByFoo, FooBar].materialize
  println(query1.inputEncoder(ByFoo(4)))
  val out1: FooBar = query1.outputDecoder(Array.empty).right.get
  println(out1)

  val query2 = q("select foo, bar from table x where foo = ? limit 1;")[ByFoo, FooBar].materialize
  println(query2.resultSetMaxSize.x) // x is member of class Some, we can access it because we statically know it is a Some at compile time
  val size1: 1 = query2.resultSetMaxSize.x // we also know it is exactly one at compile time
  // val size2: 2 = query2.resultSetMaxSize.x // this doesn't compile


//    val prepared = preparedSync(query0)
//    val result: Future[Seq[Row]] = executeAsync(prepared) // the monad depends on the underlying driver


  // Fails with: SchemaSafeExample.scala:47: "Column oops not found in table x"
//  val query2 = q["select oops from table x where foo = ?"][ByFoo, FooBar].materialize
}