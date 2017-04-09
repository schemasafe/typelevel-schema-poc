package troy.typelevel

import scala.meta._

object Parser {
  import fastparse.all._

  def parseQuery(input: String): Either[Exception, Type] = {
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
      case Parsed.Failure(_, _, extra) => Left(throw new Exception(extra.traced.trace))
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