package troy.parsers

import fastparse.core.Parsed
import troy.typelevel.ColumnType

object SchemaParserSpec extends App {
  {
    val createTable =
      """
      CREATE TABLE posts (
        author_name text PRIMARY KEY,
        post_id int,
        post_rating int,
        post_tags list<text>,
        post_title text,
        reviewer_name text
      );
      """

    val schemaParser = SchemaParser.parse(createTable)
    val schema = schemaParser.right.get

    assert(schema.tables.size == 1)

    val columns = schema.tables("posts").columns
    assert(columns.size == 6)
    assert(columns("author_name") == ColumnType.Text)
    assert(columns("post_id") == ColumnType.Int)
    assert(columns("reviewer_name") == ColumnType.Text)
    assert(columns("post_title") == ColumnType.Text)
    assert(columns("post_rating") == ColumnType.Int)
    assert(columns("post_tags") == ColumnType.List(ColumnType.Text))
  }


  {
    val createTable =
      """
      CREATE TABLE posts (
        author_name text,
        post_id int,
        post_rating int,
        post_tags list<text>,
        post_title text,
        reviewer_name text,
        PRIMARY KEY (author_name, post_id)
      );
      """

    val schemaParser = SchemaParser.parse(createTable)
    val schema = schemaParser.right.get

    assert(schema.tables.size == 1)

    val columns = schema.tables("posts").columns
    assert(columns.size == 6)
    assert(columns("author_name") == ColumnType.Text)
    assert(columns("post_id") == ColumnType.Int)
    assert(columns("reviewer_name") == ColumnType.Text)
    assert(columns("post_title") == ColumnType.Text)
    assert(columns("post_rating") == ColumnType.Int)
    assert(columns("post_tags") == ColumnType.List(ColumnType.Text))
  }
}
