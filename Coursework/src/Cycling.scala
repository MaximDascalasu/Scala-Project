
import Cycling.total
import javafx.stage.Stage

import scala.collection.{SortedIterableFactory, SortedMap}
import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap
import scala.math.Ordering.Float.TotalOrdering


object Cycling extends App {

  //read data from textfile
  val mapdata = readFile("cyclingdata.txt")
  println(mapdata)

  ////Menu
  val actionMap = Map[Int, () => Boolean](1 -> AllRoutes , 2 -> TotalDistance ,3 -> Average ,4 -> Descending,5 -> SpecifiedRoute , 6 -> quitProgram )

  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))

////***readfile function*********************************

  def readFile(filename: String): Map[String, List[(Int, String, Float)]] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[String, List[(Int, String, Float)]] = Map()

    try for (line <- Source.fromFile(filename).getLines()) {
      val splitline = line.split(",").map(_.trim).toList
      mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map {
        case s"$integer:$string:$float" => (integer.toInt, string, float.toFloat)
      })
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }


  // FUNCTIONS FOR MENU


  def readOption: Int = {
    println(
      """|Please select one of the following:
         |   1 - AllRoutes
         |   2 - Show Total Distance and Stages for each route
         |   3 - Get AVERAGE(distance and stages) of all routes
         |   4 - Descending order
         |   5 - Get specified route
         |   6 - Quit""".stripMargin)
    readInt()
  }

  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  ///////***************** menu options***************************************
 //1
  def AllRoutes(): Boolean = {
    println(showAllRoutes(mapdata))
    true
  }
  //2
  def TotalDistance():Boolean={
    showTotal()
    true
  }
  //3
  def Average():Boolean= {

  println(showAverage(mapdata))
    true
    }

  //4
  def Descending():Boolean={
    showDescending(mapdata)
    true
  }
  //5
  def SpecifiedRoute():Boolean={
    showUserSelectedRoute(mapdata)
    true
  }

//6
  def quitProgram(): Boolean = {
    println("selected quit")
    false
  }

  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER

    //1
  def showAllRoutes(data:   Map[String, List[(Int, String, Float)]]) = {
    var format= "Routes".stripMargin +"\n"
    for((x,y) <- data){
      format = format + showroute((x,y))
    }
    println(format)
  }
    //2
  def showTotal()={
    var routesum=""
    for((a,b)<-mapdata){
      routesum=routesum+ total((a,b))
    }
    println(routesum)
  }

    //3
  def showAverage(b: Map[String,List[(Int,String,Float)]]):String = {

    var sumstage= 0f
    var td = 0f
    for ((k,v) <- b){
      v.map(n => sumstage =sumstage+ n._1 / v.length)
      v.map(n => td = td+ n._3/v.length)
    }

    val avg = f"""The average total distance is ${td}%.1f km and average number of stage of all routes is ${sumstage} .""".stripMargin

    avg
  }

  //4

  def showDescending (routes: Map[String, List[(Int, String, Float)]]) = {
    var distances:List[Float] = List()
    for((k,v) <- routes){
      var total = 0f
      v.map(n => total = total+ n._3)
      distances =  distances::: total:: Nil
    }
    val impreuna = (routes zip distances).toSeq.sortWith(_._2 > _._2).toList
    var longest = ""
    for((k,v) <- impreuna){
      longest = longest + total((k._1,k._2))
    }
    println(longest)
  }

  ///5
  def showUserSelectedRoute(routes: Map[String, List[(Int, String, Float)]]) ={
   val suka = routes.zipWithIndex.map(_.swap).toMap
    var poh: String = s"""select the number of the route
                                       |""".stripMargin
   for((k,v)<- suka){ poh = poh + s"$k - ${v._1} \n"}
    println(poh)
    val userSelection: Int = readInt()
    suka.get(userSelection) match {
     case None => "This number is not an option. Please start over"
     case Some(n) => {

       println(total(n).stripMargin)
       println(showroute(n).stripMargin)
     }
   }

 }

      // OPERATION FUNCTIONS
     ///1
     def showroute(route: (String, List[(Int, String, Float)])): String = {

       val routetail = (a: (Int, String, Float)) => f"\n ${a._1}\t " + f" ${a._2}\t" + f"${a._3} f"
       var routehead = s"""
            | Name - ${route._1}
            |""".stripMargin

  route._2.map(k => routehead = routehead + routetail(k))
       routehead +"\n"
  }

  ////2
  def total(tuple: (String, List[(Int, String, Float)])):String= {
    var totdistance =0f
    tuple._2.foreach(f => totdistance =totdistance + f._3)
    f"\t${tuple._1} has ${tuple._2.length} stages and a total distance of ${totdistance}%2.1f km \n".stripMargin

  }








}
