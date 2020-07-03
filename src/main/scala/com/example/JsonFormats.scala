package com.example

import com.example.StudentService.ActionPerformed


//#json-formats
import spray.json.DefaultJsonProtocol

object JsonFormats  {
  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._

  implicit val studentJsonFormat = jsonFormat9(Student)
  implicit val newStudentJsonFormat = jsonFormat8(NewStudent)
  implicit val studentssJsonFormat = jsonFormat1(Students)

  implicit val actionPerformedJsonFormat = jsonFormat1(ActionPerformed)
}
//#json-formats
