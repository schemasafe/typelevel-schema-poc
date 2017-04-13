package troy.parsers
import scala.meta._

object ParserSpec extends App {

  val query = QueryParser.parseQuery("SELECT x, y, z from test where x = ? AND y = ? AND z CONTAINS ?")
  val typsTree = query.right.get

  assert(typsTree.children(0).syntax == t"SelectStatement".syntax)

  val cols = typsTree.children(1)

  val firstCol = cols.children(0)
  assert(firstCol.syntax == t"${"x"}".syntax)
  assert(cols.children(1).syntax == t"::".syntax)

  val secondCol = cols.children(2).children(0)
  assert(secondCol.syntax == t"${"y"}".syntax)
  assert(cols.children(2).children(1).syntax == t"::".syntax)

  val thirdCol = cols.children(2).children(2).children(0)
  assert(thirdCol.syntax == t"${"z"}".syntax)
  assert(cols.children(2).children(2).children(1).syntax == t"::".syntax)

  val endOfCols = cols.children(2).children(2).children.last
  assert(endOfCols.syntax == t"HNil".syntax)

  assert(typsTree.children(2).syntax == t"${"test"}".syntax)


  val relations = typsTree.children(3).children
  val firstRelation = relations(0).children
  assert(firstRelation(0).syntax == t"Relation".syntax)
  assert(firstRelation(1).syntax == t"${"x"}".syntax)
  assert(firstRelation(2).syntax == t"Equals".syntax)

  assert(relations(1).syntax == t"::".syntax)

  val secondRelation = relations(2).children(0).children
  assert(secondRelation(0).syntax == t"Relation".syntax)
  assert(secondRelation(1).syntax == t"${"y"}".syntax)
  assert(secondRelation(2).syntax == t"Equals".syntax)

  assert(relations(2).children(1).syntax == t"::".syntax)


  val thirdRelation = relations(2).children(2).children(0).children
  assert(thirdRelation(0).syntax == t"Relation".syntax)
  assert(thirdRelation(1).syntax == t"${"z"}".syntax)
  assert(thirdRelation(2).syntax == t"Contains".syntax)

  assert(relations(2).children(2).children(1).syntax == t"::".syntax)


  val endOfRelations = relations(2).children(2).children.last
  assert(endOfRelations.syntax == t"HNil".syntax)

}
