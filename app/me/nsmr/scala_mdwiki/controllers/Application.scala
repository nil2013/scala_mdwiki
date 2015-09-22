package me.nsmr.scala_mdwiki.controllers

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import me.nsmr.parser.GFDefaultDiscounter._

object Application extends Controller {

  def index = Action {
    Ok(me.nsmr.scala_mdwiki.views.html.index("Your new application is ready."))
  }
  
  def mdtest = Action { implicit req =>
    val form = Form("source" -> nonEmptyText).bindFromRequest
    if(form.hasErrors) {
      Ok(me.nsmr.scala_mdwiki.views.html.index("Your new application is ready."))
    } else {
      val source = form.get
      val xhtml = toXHTML(knockoff(source))
      val title = xhtml.find { x => x.label=="h1" } match {
        case Some(title) => title.text
        case None => "no title"
      }
      Ok(me.nsmr.scala_mdwiki.views.html.view.render(title, xhtml))
    }
  }
}