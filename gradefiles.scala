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
    
    val CourseName = "comp170"
    val categories = readCategoryFile(CourseName)
    val students = readStudentFiles(CourseName)
    
    def doGrading(courseName : String, categories : (Int, Array[String], Array[Int], Array[Int]), students : (Array[String], Array[String], Array[String])) : (Array[String], Array[Int], Array[Double]) = {
        val studentid = students._1
        var i = 0
        for (i <- 0 to studentid.length-1)
            studentid(i)
            val courseFileName = s"$studentid" + s"$courseName.data"
            val file = Source.fromFile(courseFileName)
            val n = file.getLines.length
            var k = 0
            val assignmentName = Array.ofDim[String](n)
            val sum = Array.ofDim[Double](categories._1)
            for (line <- file.getLines) {
                val parts = parseCSVHeader(line)
                assignmentName(k) = parts(0)
            }
                val lines = file.getLines
                val quantity = lines.next
                val quantityArray = parseCSVRowOfInts(quantity, -1)
                val score = lines.next
                val scoreArray = parseCSVRowOfDoubles(score, -1)
        (assignmentName, quantityArray, scoreArray)
    }
    

     
    //val results = readCategoryFile("comp170")
    //results match {
        //case (n, h, q, w) => {
          //println(s"There are $n columns of data")
          //println("Headings")
          //h foreach println
          //println("Quantities")
          //q foreach println
          //println("Weights")
          //w foreach println
        //}
    //}
    
    
    
}