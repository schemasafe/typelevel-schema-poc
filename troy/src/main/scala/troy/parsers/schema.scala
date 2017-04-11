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

object SchemaFactsGenerator {
  type Fact = scala.meta.Type // Containing code similar to q"implicit val fact1 = ..."

  def generate(schema: Schema): Seq[Fact] = ???
    // Output similar to
    // Seq(
    //   q"""implicit val fact1 = TableExists.instance["test"]""",
    //   q"""implicit val fact2 = ColumnHasType.instance["test", "x", ColumnType.Text]""",
    //   q"""implicit val fact3 = ColumnHasType.instance["test", "y", ColumnType.Int]""",
    //   q"""implicit val fact4 = ColumnHasType.instance["test", "z", ColumnType.List[ColumnType.Text]]"""
    // )

}

object Schema {
  type Type = scala.meta.Type

  def parseToTypelevel(schema: String): Either[String, Seq[Type]] =
    SchemaParser.parse(schema).map(SchemaFactsGenerator.generate)
}
