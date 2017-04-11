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

object QueryParser {
  import fastparse.all._

  def parseQuery(input: String): Either[String, Type] = {
    val identifier = P(CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_").rep.!).map(TypeHelpers.literal)

    val eqRelation = P(identifier.! ~ " = " ~ "?").map(TypeHelpers.eqRelationType)
    val containReltation = P(identifier.! ~ IgnoreCase(" CONTAINS ") ~ "?").map(TypeHelpers.containReltationType)
    val relation = P(eqRelation | containReltation)

    val columns = P(identifier.rep(sep=", "))
    val from = P(IgnoreCase(" FROM ") ~ identifier)
    val where = P(IgnoreCase(" WHERE ") ~ relation.rep(sep=IgnoreCase(" AND ")))

    val selectParser = P( IgnoreCase("SELECT ") ~ columns ~ from ~ where).map((TypeHelpers.selectStatmentType _).tupled)

    val parseInput = selectParser.parse(input)
    parseInput match {
      case Parsed.Success(r, _) => Right(r)
      case Parsed.Failure(_, _, extra) => Left(extra.traced.trace)
    }
  }
}

object TypeHelpers {
  def quoted(str: String): String = s"""\"$str\""""

  def literal(str: String): Type.Name = Type.Name(quoted(str))

  def relation(identifier:String, op: Type): Type = t"Relation[${literal(identifier)}, $op]"
  def eqRelationType(identifier: String): Type = relation(identifier, t"Equals")
  def containReltationType(identifier: String): Type = relation(identifier, t"Contains")

  def foldAsHList(ts: Seq[Type]): Type = ts.reverse.foldLeft[Type](t"HNil") {
    case (acc: Type, s: Type) => t"$s :: $acc"
  }

  def selectStatmentType(columns: Seq[Type], table: Type, relations: Seq[Type]):Type = {
    t"SelectStatement[${foldAsHList(columns)}, $table, ${foldAsHList(relations)}]"
  }
}
