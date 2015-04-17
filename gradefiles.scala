import scala.util._
import scala.io._
import scala.math._

object gradefiles extends App {

  def parseCSVHeader(line : String) : Array[String] = {
    val tokens = line.split(",")
    for (i <- 0 until tokens.length)
       tokens(i) = tokens(i).trim
    tokens
  }

  def parseCSVRowOfDoubles(line : String, failValue : Double) : Array[Double] = {

    val tokens = line.split(",")
    val doubles = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      doubles(i) = Try(tokens(i).trim.toDouble) getOrElse(failValue)
    } 
    doubles
  }

  def parseCSVRowOfInts(line : String, failValue : Int) : Array[Int] = {

    val tokens = line.split(",")
    val integers = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      integers(i) = Try(tokens(i).trim.toInt) getOrElse(failValue)
    } 
    integers
  }

  def readCategoryFile(courseName : String) : (Int, Array[String], Array[Int], Array[Int]) = {
    val courseFileName = s"categories_$courseName.txt"
    val file = Source.fromFile(courseFileName)
    val lines = file.getLines
    val header = lines.next
    val headerNames = parseCSVHeader(header)
    val quantities = lines.next
    val quantitiesArray = parseCSVRowOfInts(quantities, -1)
    val weights = lines.next
    val weightsArray = parseCSVRowOfInts(weights, -1)

    val columns = min(min(headerNames.length, quantitiesArray.length), weightsArray.length)

    (columns, headerNames, quantitiesArray, weightsArray)
  }

  //val courseName = Try(args(0)) getOrElse("comp150")
  //println(s">> Reading $courseName categories file")
  //val results = readCategoryFile(courseName)
  //results match {
    //case (n, h, q, w) => {
      //println(s"There are $n columns of data")
      //println("Headings")
      //h foreach println
      //println("Quantities")
      //q foreach println
      //println("Weights")
      //w foreach println
   
 

    def readStudentFiles(courseName : String) : (Array[String], Array[String], Array[String]) = {
        val courseFileName = s"students_$courseName.txt"
        val file = Source.fromFile(courseFileName)
        val n = file.getLines.length
        val ids = Array.ofDim[String](n)
        val lastNames = Array.ofDim[String](n)
        val firstNames = Array.ofDim[String](n)
        var i = 0
        for (line <- file.getLines) {
            val parts = parseCSVHeader(line)
            ids(i) = parts(0)
            lastNames(i) = parts(1)
            firstNames(i) = parts(2)
        }
        (ids, lastNames, firstNames)
  }
    def readGradeFiles(courseName : String) : Array[(String, Int, Int)] = {
        val studentids = readStudentFiles(0)
        val students = parseCSVHeader(studentids)
        val courseFileName = s"$students$courseName.data"
        val file = Source.fromFile(courseFileName)
        var gradefiles = Array[(String, Int, Int)]
        for (line <- file.getLines) {
            val parts = parseCSVHeader(line)
            gradefiles = gradefiles :+ (parts(0), parts(1).toInt, parts(2).toInt)
        }
        gradefiles
    }
}