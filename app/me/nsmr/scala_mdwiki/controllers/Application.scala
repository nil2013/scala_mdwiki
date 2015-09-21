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
      val md = knockoff(source)
      val xhtml = toXHTML(md)
      Ok(me.nsmr.scala_mdwiki.views.html.view.render(xhtml))
    }
  }
}