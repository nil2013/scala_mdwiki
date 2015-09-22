package me.nsmr.scala_mdwiki.models

import anorm._
import play.api.db.DB
import play.api.Play.current
import play.api.Logger

object DatabaseConnectionHelper {
  def executeInsert(sql: Sql) = DB.withConnection { implicit c => sql.executeInsert() }
  def executeUpdate(sql: Sql) = DB.withConnection { implicit c => sql.executeUpdate() }
  def execute(sql: Sql) = DB.withConnection { implicit c => sql.execute() }
  def readAs[T](sql: Sql, parser: RowParser[T]): List[T] =
    DB.withConnection{ implicit c => sql.as(parser *) }
}
