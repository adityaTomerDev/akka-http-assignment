package com.example

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import com.example.StudentService._
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import com.example.StudentService._

//#import-json-formats
//#Student-routes-class
class StudentRoutes(StudentRegistry: ActorRef[StudentService.Command])(implicit val system: ActorSystem[_]) {

  //#Student-routes-class
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._
  //#import-json-formats

  // If ask takes more time than this to complete the request is failed
  private implicit val timeout = Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

  def getStudents(): Future[Students] =
    StudentRegistry.ask(GetStudents)
  def getStudentById(name: String): Future[GetStudentResponse] =
    StudentRegistry.ask(GetStudentById(name, _))
  def getStudentsByDepartment(department: String): Future[Students] =
    StudentRegistry.ask(GetStudentsByDepartment(department, _))
  def getStudentsByCity(city: String): Future[Students] =
    StudentRegistry.ask(GetStudentsByCity(city, _))
  def getStudentsByState(state: String): Future[Students] =
    StudentRegistry.ask(GetStudentsByState(state, _))
  def updateStudent(student: Student): Future[ActionPerformed] =
    StudentRegistry.ask(UpdateStudent(student, _))
  def createStudent(newStudent: NewStudent): Future[ActionPerformed] =
    StudentRegistry.ask(CreateStudent(newStudent, _))
  def deleteStudent(name: String): Future[ActionPerformed] =
    StudentRegistry.ask(DeleteStudent(name, _))

  //#all-routes
  //#Students-get-post
  //#Students-get-delete
  val studentRoutes: Route =
    pathPrefix("students") {
      concat(
        //#Students-get-delete
        pathEnd {
          concat(
            get {
              complete(getStudents())
            },
            post {
              entity(as[NewStudent]) { newStudent =>
                onSuccess(createStudent(newStudent)) { performed =>
                  complete((StatusCodes.Created, performed))
                }
              }
            })
        },
        //#Students-get-delete
        //#Students-get-post
        path(Segment) { id =>
          concat(
            get {
              //#retrieve-Student-info
              rejectEmptyResponse {
                onSuccess(getStudentById(id)) { response =>
                  complete(response.maybeStudent)
                }
              }
              //#retrieve-Student-info
            },
            delete {
              //#Students-delete-logic
              onSuccess(deleteStudent(id)) { performed =>
                complete((StatusCodes.OK, performed))
              }
              //#Students-delete-logic
            },
            put {
              entity(as[Student]) { student =>
                println(" iin update call")
                onSuccess(updateStudent(student)) { response =>
                  complete(response)

                }
              }
            }
          )
        },
        pathPrefix("by-city") {
          path(Segment) { city =>
            get {
              complete(getStudentsByCity(city))
            }

          }
        },
        pathPrefix("by-state") {
          path(Segment) { state =>
            get {
              complete(getStudentsByState(state))
            }

          }
        },
        pathPrefix("by-department") {
          path(Segment) { department =>
            get {
              complete(getStudentsByDepartment(department))
            }

          }
        },

      )
      //#Students-get-delete
    }
  //#all-routes
}
