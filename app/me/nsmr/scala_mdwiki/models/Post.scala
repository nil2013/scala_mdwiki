package me.nsmr.scala_mdwiki.models

import anorm._
import java.util.Date
import play.api.Logger
import me.nsmr.parser.GFDefaultDiscounter._
import com.tristanhunt.knockoff.Block

object Post {
  import anorm.SqlParser._
  import DatabaseConnectionHelper._
  val id_column: String = "id"
  val tableName = "post"
  def toMap(post: Post) = {
    println(post)
    Map(
        'id        -> post.id,
        'title      -> post.title,
        'content    -> post.content,
        'createDate -> post.createDate,
        'updateDate -> post.updateDate
        )
  }
  implicit def map2params(map: Map[Symbol, Any]): Seq[(Any, ParameterValue[_])] = {
    println("mapping...")
    val result =
      (map.map { case ((sym, value)) => any2ArrowAssoc(sym) -> toParameterValue(value) }).toList
    println("map finish")
    result
  }
  val parser = (long("id")~str("title")~str("content")~date("create_date")~date("update_date"))
  def load(id: Long): Option[Post] = {
    readAs(SQL(s"""select * from $tableName where id={id}""").on('id -> id), (parser.map {
      case id~title~content~createDate~updateDate =>
        Post(id, title, content, createDate, updateDate)
    })) match {
      case list if list.size>0 => Some(list(0))
      case _ => None
    }
  }
  def apply(id: Long) = load(id)
  def apply(title: String, content: String, createDate: Date, updateDate: Date): Post =
    apply(-1, title, content, createDate, updateDate)
  def apply(title: String, content: String): Post =
    apply(-1, title, content, new Date, new Date)
}
case class Post (id: Long, title: String, content: String, createDate: Date, updateDate: Date) {
  import Post._
  import DatabaseConnectionHelper._
  def getId: Long = id
  def toMap = Post.toMap(this)
  def update(id: Long=this.id, title: String=this.title, content: String=this.content, createDate: Date=this.createDate, updateDate: Date=this.updateDate) = {
    
  }
  def save = {
    Logger.debug("save...")
    val sql = SQL(s"""insert into $tableName (title, content, create_date, update_date)
            values ({title}, {content}, {createDate}, {updateDate})""")
            .on(toMap:_*)
    Logger.debug(sql.toString)
    val id = executeInsert(sql)
    id match {
      case Some(id) => Post(id, this.title, this.content, this.createDate, this.updateDate)
      case None => throw new Exception("save error!")
    }
  }
  def update = {
    executeUpdate(SQL(s"""update $tableName set title={title}, content={content}, create_date={createDate}, update_date={updateDate}
            where id={id}""")
            .on(toMap:_*))
  }
  def delete = {
    execute(SQL(s"""delete from $tableName where id={id}""").on('id -> this.id))
  }
  lazy val blocks: Seq[Block] = knockoff(content)
  lazy val getContentXHTML = toXHTML(blocks)
}
