package controllers.api.v1

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import javax.inject.Inject
import scalikejdbc._
import models._
import UsersController._

object UsersController {
  case class UserParams(name: String, companyId: Option[Int])

  implicit val usersWritesFormat = new Writes[Users]{
    def writes(user: Users): JsValue = {
      Json.obj(
        "id"        -> user.id,
        "name"      -> user.name,
        "companyId" -> user.companyId
      )
    }
  }

  implicit val userReads  = Json.reads[UserParams]
}

class UsersController @Inject()(components: ControllerComponents)
  extends AbstractController(components) {

  private val u = Users.syntax("u")
  private val c = Companies.syntax("c")

  /**
   * GET /api/v1/users
   */
  def index = Action { implicit request =>
    DB.readOnly { implicit session =>
      val users = withSQL {
        select.from(Users as u).orderBy(u.id.asc)
      }.map(Users(u.resultName)).list.apply()

      Ok(Json.obj("data" -> users))
    }
  }

  /**
   * GET /api/v1/users/:id
   */
  def show(id: Long) = Action { implicit request =>
    DB.readOnly { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          Ok(Json.obj("data" -> user))
        }
        case None => NotFound
      }
    }
  }

  /**
   * POST /api/v1/users
   */
  def create = Action(parse.json) { implicit request =>
    request.body.validate[UserParams].map { form =>
      DB.localTx { implicit session =>
        val user = Users.create(form.name, form.companyId)
        Ok(Json.obj("data" -> user))
      }
    }.recoverTotal{ e =>
      BadRequest(Json.obj("error" -> JsError.toJson(e)))
    }
  }

  /**
   * PUT   /api/v1/users/:id
   * PATCH /api/v1/users/:id
   */
  def update(id: Long) = Action(parse.json) { implicit request =>
    DB.localTx { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          request.body.validate[UserParams].map { form =>
            val savedUser = Users.save(user.copy(name = form.name, companyId = form.companyId))
             Ok(Json.obj("data" -> savedUser))
          }.recoverTotal { e =>
            BadRequest(Json.obj("error" -> JsError.toJson(e)))
          }
        }
        case None => NotFound
      }
    }
  }

  /**
   * DELETE /api/v1/users/:id
   */
  def destroy(id: Long) = Action{ implicit request =>
    DB.localTx { implicit session =>
      Users.find(id) match {
        case Some(user) => {
          Users.destroy(user)
          Ok
        }
        case None => NotFound
      }
    }
  }
}
