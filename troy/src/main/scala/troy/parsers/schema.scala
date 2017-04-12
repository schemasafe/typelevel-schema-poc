/*
 * Copyright 2016 Tamer AbdulRadi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package troy.parsers

import scala.meta._
import troy.typelevel.ColumnType

case class Schema(tables: Map[String, Table]) extends AnyVal
case class Table(columns: Map[String, ColumnType]) extends AnyVal

object SchemaParser {
  type Parser[T] = fastparse.core.Parser[T,Char,String]

  def columnType: Parser[ColumnType] = ???
  def column: Parser[(String, ColumnType)] = ???
  def columns: Parser[Seq[(String, ColumnType)]] = ???
  def table: Parser[Table] = ???
  def tables: Parser[Seq[Table]] = ???
  def schema: Parser[Schema] = ???

  def parse(schema: String): Either[String, Schema] = ???
}

sealed trait Fact
final case class TableExistsFact(table: String) extends Fact
final case class ColumnHasTypeFact(table: String, column: String, ctype: ColumnType) extends Fact

object SchemaFactsGenerator {
  def generate(schema: Schema): Seq[Fact] = ???
}

object Schema {
  // Output similar to
  // Seq(
  //   q"""implicit val fact1 = TableExists.instance["test"]""",
  //   q"""implicit val fact2 = ColumnHasType.instance["test", "x", ColumnType.Text]""",
  //   q"""implicit val fact3 = ColumnHasType.instance["test", "y", ColumnType.Int]""",
  //   q"""implicit val fact4 = ColumnHasType.instance["test", "z", ColumnType.List[ColumnType.Text]]"""
  // )
  def parseToTypelevel(schema: String): Either[String, Seq[Stat]] =
    SchemaParser.parse(schema).map(SchemaFactsGenerator.generate).map(_
      .zipWithIndex
      .map {
        case (TableExistsFact(table), i) =>
          constructFact(i, q"TableExists.instance[${literal(table)}]")
        case (ColumnHasTypeFact(table, column, ctype), i) =>
          constructFact(i, q"ColumnHasType.instance[${literal(table)}, ${literal(column)}, ${ctname(ctype)}]")
      }
    )

    def constructFact(i: Int, value: Term) = {
      val termName = Pat.Var.Term(Term.Name(s"fact$i"))
      q"implicit val $termName = $value"
    }

    def literal(str: String): Type.Name = tname(quoted(str))

    def quoted(str: String): String = s"""\"$str\""""

    def tname(str: String): Type.Name = Type.Name(str)

    def ctname(ctype: ColumnType): Type.Name =
      tname("ColumnType." + (ctype match {
        case ColumnType.Text => "Text"
        case ColumnType.Int => "Int"
        case ColumnType.List(inner) => s"List[${ctname(inner)}]"
      }))


}
