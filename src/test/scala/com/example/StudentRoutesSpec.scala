package com.example

//#user-routes-spec
//#test-top
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Matchers, WordSpec }
import akka.actor.typed.scaladsl.adapter._

//#set-up
class
StudentRoutesSpec extends WordSpec with Matchers with ScalaFutures with ScalatestRouteTest {
  //#test-top

  // the Akka HTTP route testkit does not yet support a typed actor system (https://github.com/akka/akka-http/issues/2036)
  // so we have to adapt for now

  lazy val testKit = ActorTestKit()
  implicit def typedSystem = testKit.system
  override def createActorSystem(): akka.actor.ActorSystem =
    testKit.system.toClassic

  // Here we need to implement all the abstract members of UserRoutes.
  // We use the real UserRegistryActor to test it while we hit the Routes,
  // but we could "mock" it by implementing it in-place or by using a TestProbe
  // created with testKit.createTestProbe()
  val studentService = testKit.spawn(StudentService())
  lazy val routes = new StudentRoutes(studentService).studentRoutes

  // use the json formats to marshal and unmarshall objects in the test
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._
  //#set-up

  //#actual-test
  "studentRoutes" should {
    "return no users if no present (GET /students)" in {
      // note that there's no need for the host part in the uri:
      val request = HttpRequest(uri = "/students")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        // we expect the response to be json:
        contentType should ===(ContentTypes.`application/json`)

        // and no entries should be in the list:
        entityAs[String] should ===("""{"students":[]}""")
      }
    }
    //#actual-test

    //#testing-post
    "be able to add users (POST /students)" in {
      val newStudent1 = NewStudent("aditya", "tomer", 26, "software", "23/07/1994", "Kolkata", "West Bengal", "a@t.com")
      val newStudent2 = NewStudent("sunny", "singh", 26, "software", "23/07/1994", "Bengaluru", "Karnataka", "s@s.com")
      val newStudent3 = NewStudent("Vaky", "Kaushik", 26, "marketing", "22/11/1994", "Gurgaon", "Haryana", "v@k.com")
      postCheck(newStudent1)
      postCheck(newStudent2)
      postCheck(newStudent3)
         }
    //#testing-post

    "be able to get users by department" in {
      val request = Get("/students/by-department/software")
      request ~> routes ~> check {
        status should === (StatusCodes.OK)
        contentType should === (ContentTypes.`application/json`)
        val students = entityAs[Students]
        students.students.size should === (2)
        students.students.count(_.department.equals("software")) should === (2)
      }
    }

    "be able to get users by city" in {
      val request = Get("/students/by-city/Bengaluru")
      request ~> routes ~> check {
        status should === (StatusCodes.OK)
        contentType should === (ContentTypes.`application/json`)
        val students = entityAs[Students]
        students.students.size should === (1)
        students.students.count(_.city.equals("Bengaluru")) should === (1)
      }
    }

    "be able to get users by state" in {
      val request = Get("/students/by-state/Haryana")
      request ~> routes ~> check {
        status should === (StatusCodes.OK)
        contentType should === (ContentTypes.`application/json`)
        val students = entityAs[Students]
        students.students.size should === (1)
        students.students.count(_.state.equals("Haryana")) should === (1)
      }
    }

    "be able to update student" in {
      getByState() match {
        case Some(value) =>
          val updateInfo = UpdateStudentInfo( id = value.id, city = Some("Gurugram"))
          val entity = Marshal(updateInfo).to[MessageEntity].futureValue
          val request = Put(s"/students/${value.id}").withEntity(entity)
          request ~> routes ~> check {
            println(" status in put"+ status)
            status should === (StatusCodes.OK)
            contentType should === (ContentTypes.`application/json`)
            entityAs[String] should === (s"""{"description":"Student ${value.id} updated."}""")
          }
        case None =>
          fail(" students should not have been empty")
      }
    }

    "be able to remove users (DELETE /students)" in {
      getByState() match {
          case Some(value) =>
            val request = Delete(uri = s"/students/${value.id}")

            request ~> routes ~> check {
              status should ===(StatusCodes.OK)

              // we expect the response to be json:
              contentType should ===(ContentTypes.`application/json`)

              // and no entries should be in the list:
              entityAs[String] should ===(s"""{"description":"Student ${value.id} deleted."}""")
            }
          case None =>
            fail(" students should not have been empty")
        }

      }
      // user the RequestBuilding DSL provided by ScalatestRouteSpec:

    }
    //#actual-test


  def postCheck(newStudent: NewStudent) = {

    val newStudentEntity = Marshal(newStudent).to[MessageEntity].futureValue // futureValue is from ScalaFutures

    // using the RequestBuilding DSL:
    val request = Post("/students").withEntity(newStudentEntity)

    request ~> routes ~> check {
      status should ===(StatusCodes.Created)

      // we expect the response to be json:
      contentType should ===(ContentTypes.`application/json`)

      // and we know what message we're expecting back:
      entityAs[String] should ===(s"""{"description":"Student ${newStudent.firstName} created."}""")
    }
    //#actual-test


  }

  def getByState(): Option[Student] = {
    val getRequest = Get("/students/by-state/Haryana")
    getRequest ~> routes ~> check {
      status should ===(StatusCodes.OK)
      contentType should ===(ContentTypes.`application/json`)
      val students = entityAs[Students]
      println("got students "+ students)
      students.students.headOption
    }
  }
  //#set-up
}
//#set-up
//#user-routes-spec
