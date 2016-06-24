package me.nsmr.scala_mdwiki.controllers

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import me.nsmr.parser.GFDefaultDiscounter._
import me.nsmr.scala_mdwiki.models.Post
import me.nsmr.utils.FileUtil

object Application extends Controller {

  def index = Action {
    Ok(me.nsmr.scala_mdwiki.views.html.index(Post.getList().getOrElse(List())))
  }

  def post = Action { implicit req =>
    val form = Form("source" -> nonEmptyText).bindFromRequest
    if(form.hasErrors) {
      Ok(me.nsmr.scala_mdwiki.views.html.edit(None))
    } else {
      Redirect(routes.Application.view(Post(form.get).save.id))
    }
  }

  def add = Action { Ok(me.nsmr.scala_mdwiki.views.html.edit(None)) }

  def view(id: Long) = Action{
    val list = Post.getList() match {
      case Some(list) => list
      case None => List()
    }
    Post(id) match {
      case None => NotFound("Page not found...")
      case Some(post) =>
        Ok(me.nsmr.scala_mdwiki.views.html.view.render(list, post))
    }
  }

  def edit(id: Long) = Action{
    Post(id) match {
      case None => NotFound("Page not found...")
      case post =>
        Ok(me.nsmr.scala_mdwiki.views.html.edit(post))
    }
  }

  def update(id: Long) = Action{ implicit req =>
    Post(id) match {
      case None => NotFound("Page not found...")
      case Some(post) =>
        val form = Form("source" -> nonEmptyText).bindFromRequest
        if(form.hasErrors) {
          Ok(me.nsmr.scala_mdwiki.views.html.edit(None))
        } else {
          Redirect(routes.Application.view(post.update(form.get).id))
        }
    }
  }

  def remove(id: Long) = Action {
    Post(id) match {
      case None => NotFound("Page not found...")
      case Some(post) =>
        val result = post.delete
        println(result)
//        if(post.delete.id < 0) {
        Redirect(routes.Application.index())
//        } else {
//          throw new Exception("operation failed: Remove")
//        }
    }
  }

  def upload = Action(parse.multipartFormData) { req =>
    req.body.file("mdfile").map { picture =>
      Redirect(routes.Application.view(
          Post(FileUtil.readFile(picture.ref.file))
            .save.id))
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "Missing file")
    }
  }

  def search = Action { implicit req =>
    val form = Form("keyword" -> nonEmptyText).bindFromRequest
    if(form.hasErrors) {
      Ok(me.nsmr.scala_mdwiki.views.html.index(Post.getList().getOrElse(List())))
    } else {
      Ok(me.nsmr.scala_mdwiki.views.html.index(Post.getList(s"where content like '%${form.get}%'").getOrElse(List())))
    }
  }

  def download(id: Long) = Action {
    Post(id) match {
      case None => NotFound("Page not found...")
      case Some(post) =>
        Ok(post.content).withHeaders(
            CONTENT_DISPOSITION -> s"""attachment; filename="${post.title}.md""""
            ).as("text/markdown; charset=UTF-8")
    }
  }
}
