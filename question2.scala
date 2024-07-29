import scala.io.StdIn._

object demo {

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks should be positive and cannot exceed total possible marks."))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks should be a positive integer."))
    } else {
      (true, None)
    }
  }


  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student name: ")
    val name = readLine()
    println("Enter marks obtained: ")
    val marks = readInt()
    println("Enter total possible marks: ")
    val totalMarks = readInt()


    val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
    if (!isValid) {
      println(s"Error: ${errorMessage.get}")
      getStudentInfo()
    } else {
      val percentage = (marks.toDouble / totalMarks) * 100
      val grade = percentage match {
        case p if p >= 90 => 'A'
        case p if p >= 75 => 'B'
        case p if p >= 50 => 'C'
        case _ => 'D'
      }
      (name, marks, totalMarks, percentage, grade)
    }
  }


  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks/$totalMarks")
    println(s"Percentage: ${percentage}%.2f".format(percentage))
    println(s"Grade: $grade")
  }


  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!isValid) {
      println("Enter student name: ")
      val name = readLine()
      println("Enter marks obtained: ")
      val marks = readInt()
      println("Enter total possible marks: ")
      val totalMarks = readInt()


      val validationResult = validateInput(name, marks, totalMarks)
      if (validationResult._1) {
        isValid = true
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        studentInfo = (name, marks, totalMarks, percentage, grade)
      } else {
        println(s"Error: ${validationResult._2.get}")
      }
    }

    studentInfo
  }

  def main(args: Array[String]): Unit = {

    val studentRecord = getStudentInfoWithRetry()
    printStudentRecord(studentRecord)
  }
}
