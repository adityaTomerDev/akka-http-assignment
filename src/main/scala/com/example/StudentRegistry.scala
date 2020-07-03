package com.example

//#user-service-actor
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.collection.immutable
import scala.util.Random

case class NewStudent(firstName: String, lastName: String, age: Int, department: String, dob: String, city: String, state: String, email: String )
case class Student(id: String, firstName: String, lastName: String, age: Int, department: String, dob: String, city: String, state: String, email: String )
case class UpdateStudentInfo(id: String,
                             firstName: Option[String] = None,
                             lastName: Option[String] = None,
                             age: Option[Int] = None,
                             department: Option[String] = None,
                             dob: Option[String] = None,
                             city: Option[String] = None,
                             state: Option[String] = None,
                             email: Option[String] = None ) {

  def to(student: Student) : Student = {
    student.copy(id = id,
      firstName= this.firstName.getOrElse(student.firstName),
      lastName = this.lastName.getOrElse(student.lastName),
      age = this.age.getOrElse(student.age),
      department = this.department.getOrElse(student.department),
      dob = this.dob.getOrElse(student.dob),
      city = this.city.getOrElse(student.city),
      state = this.state.getOrElse(student.state),
      email = this.email.getOrElse(student.email)
      )

  }
}
//#user-case-classes
case class Students(students: Seq[Student])
//#user-case-classes

object StudentService {
  // actor protocol
  sealed trait Command
  final case class GetStudents(replyTo: ActorRef[Students]) extends Command
  final case class CreateStudent(newStudent: NewStudent, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class GetStudentById(id: String, replyTo: ActorRef[GetStudentResponse] ) extends Command
  final case class GetStudentsByState(state: String, replyTo: ActorRef[Students]) extends Command
  final case class GetStudentsByCity(city: String, replyTo: ActorRef[Students]) extends Command
  final case class GetStudentsByDepartment(department: String, replyTo: ActorRef[Students]) extends Command
  final case class UpdateStudent(updateDetails: UpdateStudentInfo, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class DeleteStudent(id: String, replyTo: ActorRef[ActionPerformed]) extends Command


  final case class GetStudentResponse(maybeStudent: Option[Student])
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = service(Set.empty)

  private def service(students: Set[Student]): Behavior[Command] =
    Behaviors.receiveMessage {
      case GetStudents(replyTo) =>
        replyTo ! Students(students.toSeq)
        Behaviors.same
      case CreateStudent(student, replyTo) =>
        val id = Random.alphanumeric.take(30).mkString("")
        replyTo ! ActionPerformed(s"Student ${student.firstName} created.")
        service(students + Student(id, student.firstName, student.lastName, student.age, student.department, student.dob, student.city, student.state, student.email))
      case GetStudentById(id, replyTo) =>
        replyTo ! GetStudentResponse(students.find(_.id.equals(id)))
        Behaviors.same
      case GetStudentsByDepartment(department, replyTo) =>
        replyTo ! Students(students.filter(_.department.equals(department)).toSeq)
        Behaviors.same
      case GetStudentsByCity(city, replyTo) =>
        replyTo ! Students(students.filter(_.city.equals(city)).toSeq)
        Behaviors.same
      case GetStudentsByState(state, replyTo) =>
        replyTo ! Students(students.filter(_.state.equals(state)).toSeq)
        Behaviors.same
      case UpdateStudent(updateDetails, replyTo) =>
        students.find(_.id.equals(updateDetails.id)) match {
          case Some(value) =>
            replyTo ! ActionPerformed(s"Student ${updateDetails.id} updated.")
            val student = updateDetails.to(value)
            service(students.filterNot(_.id.equals(value.id)) + student)
          case None =>
            replyTo ! ActionPerformed(s"Student with ${updateDetails.id} not found . Updating the id is not allowed.")
            Behaviors.same

        }
      case DeleteStudent(id, replyTo) =>
        replyTo ! ActionPerformed(s"Student $id deleted.")
        service(students.filterNot(_.id.equals(id)))
    }
}
//#user-service-actor
