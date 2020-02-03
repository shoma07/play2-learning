package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject.Inject
import scalikejdbc._
import models._
import UsersController._

class UsersController @Inject()(components: MessagesControllerComponents)
  extends MessagesAbstractController(components) {

  private val u = Users.syntax("u")
  private val c = Companies.syntax("c")

  /**
   * GET /users
   */
  def index = Action { implicit request =>
    DB.readOnly { implicit session =>
      val users = withSQL {
        select.from(Users as u)
              .leftJoin(Companies as c)
              .on(u.companyId, c.id)
              .orderBy(u.id.asc)
      }.map { rs =>
        (
          Users(u)(rs),
          rs.intOpt(c.resultName.id).map(_ => Companies(c)(rs))
        )
      }.list.apply()

      Ok(views.html.users.index(users))
    }
  }

  /**
   * GET /users/:id
   */
  def show(id: Long) = Action { implicit request =>
    DB.readOnly { implicit session =>
      withSQL {
        select.from(Users as u).leftJoin(Companies as c).on(u.companyId, c.id)
          .where.eq(u.id, id)
      }.map { rs =>
        (Users(u)(rs), rs.intOpt(c.resultName.id).map(_ => Companies(c)(rs)))
      }.single.apply() match {
        case Some((user, company)) => Ok(views.html.users.show(user, company))
        case None => NotFound
      }
    }
  }

  /**
   * GET /users/new
   */
  def news = Action { implicit request =>
    DB.readOnly { implicit session =>
      Ok(views.html.users.news(userForm, Companies.findAll()))
    }
  }

  /**
   * POST /users
   */
  def create = Action { implicit request =>
    DB.localTx { implicit session =>
      userForm.bindFromRequest.fold(
        error => {
          BadRequest(views.html.users.news(error, Companies.findAll()))
        },
        form => {
          Users.create(form.name, form.companyId)
          Redirect(routes.UsersController.index)
        }
      )
    }
  }

  /**
   * GET /users/:id/edit
   */
  def edit(id: Long) = Action { implicit request =>
    DB.readOnly { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          val form = userForm.fill(UserForm(user.name, user.companyId))
          val companies = Companies.findAll()
          Ok(views.html.users.edit(id, form, companies))
        }
        case None => NotFound
      }
    }
  }

  /**
   * POST /users/:id/update
   */
  def update(id: Long) = Action { implicit request =>
    DB.localTx { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          userForm.bindFromRequest.fold(
            error => {
              BadRequest(views.html.users.edit(id, error, Companies.findAll()))
            },
            form => {
              Users.save(user.copy(name = form.name, companyId = form.companyId))
              Redirect(routes.UsersController.index)
            }
          )
        }
        case None => NotFound
      }
    }
  }

  /**
   * POST /usres/:id/destroy
   */
  def destroy(id: Long) = Action { implicit request =>
    DB.localTx { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          Users.destroy(user)
          Redirect(routes.UsersController.index)
        }
        case None => NotFound
      }
    }
  }
}

object UsersController {
  case class UserForm(name: String, companyId: Option[Int])

  val userForm = Form(
    mapping(
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}
