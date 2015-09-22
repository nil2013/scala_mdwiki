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
    Map(
        'id         -> post.id,
        'title      -> post.title,
        'content    -> post.content,
        'createDate -> post.createDate,
        'updateDate -> post.updateDate
        )
  }
  implicit def map2params(map: Map[Symbol, Any]): Seq[(Any, ParameterValue[_])] =
    (map.map { case ((sym, value)) => any2ArrowAssoc(sym) -> toParameterValue(value) }).toList

  val parser = (long("id")~str("title")~str("content")~date("create_date")~date("update_date") map {
      case id~title~content~createDate~updateDate =>
        Post(id, title, content, createDate, updateDate)
    })

  def getList(option: String = "") = {
    readAs(SQL(s"""select * from $tableName $option"""), parser) match {
      case list if list.size>0 => Some(list)
      case _ => None
    }
  }

  def load(id: Long): Option[Post] = {
    readAs(SQL(s"""select * from $tableName where id={id}""").on('id -> id), parser) match {
      case list if list.size>0 => Some(list(0))
      case _ => None
    }
  }

  def getTitle(content: String): String =
    getTitle(knockoff(content))
  def getTitle(blocks: Seq[Block]): String = 
    toXHTML(blocks).find { x => x.label=="h1" } match {
      case Some(title) => title.text
      case None => "no title"
    }

  def apply(id: Long) = load(id)
  def apply(content: String): Post =
    apply(getTitle(content), content, new Date)
  def apply(title: String, content: String, createDate: Date, updateDate: Date): Post =
    apply(-1, title, content, createDate, updateDate)
  def apply(title: String, content: String, createDate: Date): Post =
    apply(-1, title, content, createDate, createDate)
  def apply(title: String, content: String): Post =
    apply(-1, title, content, new Date, new Date)
}
case class Post (id: Long, title: String, content: String, createDate: Date, updateDate: Date) {
  import Post._
  import DatabaseConnectionHelper._
  def getId: Long = id
  def toMap = Post.toMap(this)

  def update(content: String=this.content): Post =
    Post(id, getTitle(content), content, createDate, new Date).update

  def save = {
    val sql = SQL(s"""insert into $tableName (title, content, create_date, update_date)
            values ({title}, {content}, {createDate}, {updateDate})""")
            .on(this.toMap:_*)
    val id = executeInsert(sql)
    id match {
      case Some(id) => Post(id, this.title, this.content, this.createDate, this.updateDate)
      case None => throw new Exception("save error!")
    }
  }
  def update = {
    executeUpdate(SQL(s"""update $tableName set title={title}, content={content}, create_date={createDate}, update_date={updateDate}
            where id={id}""")
            .on(this.toMap:_*))
    this
  }
  def delete = {
    val result = execute(SQL(s"""delete from $tableName where id={id}""").on('id -> this.id))
    println(result)
    if(result)
      Post(-1, this.title, this.content, this.createDate, this.updateDate)
    else this
  }
  lazy val blocks: Seq[Block] = knockoff(content)
  lazy val getContentXHTML = toXHTML(blocks)
}
