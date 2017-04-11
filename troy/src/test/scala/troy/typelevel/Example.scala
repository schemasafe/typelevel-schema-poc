package troy.typelevel

import com.datastax.driver.core.{Cluster, Session}
import shapeless._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

case class Post(
  authorName: String,
  postId: Int,
  reviewerName: String,
  title: String,
  rating: Int,
  tags: Seq[String]
)

object PostService {
  object Queries {
    final case class Get(authorName: String, postId: Int)
  }
}

// TODO: This should use the macro based Schema parser
object Schema {
  implicit val fact1 = TableExists.instance["posts"]
  implicit val fact2 = ColumnHasType.instance["posts", "author_name", ColumnType.Text]
  implicit val fact3 = ColumnHasType.instance["posts", "post_id", ColumnType.Int]
  implicit val fact4 = ColumnHasType.instance["posts", "reviewer_name", ColumnType.Text]
  implicit val fact5 = ColumnHasType.instance["posts", "post_title", ColumnType.Text]
  implicit val fact6 = ColumnHasType.instance["posts", "post_rating", ColumnType.Int]
  implicit val fact7 = ColumnHasType.instance["posts", "post_tags", ColumnType.List[ColumnType.Text]]
}


class PostService(implicit session: Session) {
  import Schema._
  import Operator._

  // TODO: This should use the macro based Query parser
  val listByAuthor = Query.select[PostService.Queries.Get, Post, SelectStatement[
    "author_name" :: "post_id" :: "reviewer_name" :: "post_title" :: "post_rating" :: "post_tags" :: HNil,
    "posts",
    Relation["author_name", Equals] :: Relation["post_id", Equals] :: HNil
  ]]("SELECT author_name, post_id, reviewer_name, post_title, post_rating, post_tags from posts where author_name = ? AND post_id = ?")
}

object Example extends App {
  val port: Int = 9042
  val host: String = "127.0.0.1"

  private val cluster =
    new Cluster.Builder().addContactPoints(host).withPort(port).build()

  implicit val session: Session = cluster.connect("test")

  val postService = new PostService()


  val posts = Await.result(postService.listByAuthor(PostService.Queries.Get("Tam", 1)), Duration(1, "second"))
  println(posts)

  session.close()
  cluster.close()
}
