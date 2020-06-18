
var mapBuffer: Map[String, List[(Int, String, Float)]] = Map()

var key ="Oor Wullie Route (GCU)"
var newList = List((1,"City Chambers",0.75f),(2,"Sir Chris Hoy Velodrome",3.8f),(3,"People's Palace",2.7f),(4,"Riverside Museum",5.4f),(5,"Botanic Gardens",2.4f),(6,"GCU",3.4f))
mapBuffer = mapBuffer ++ Map(key -> newList)

key ="Religious Route (Glasgow Cathedral)"
newList = List((1,"St Andrew's Cathedral",1.8f),(2,"Central Mosque",0.75f),(3,"University Chapel",5.4f),(4,"Om Hindu Mandir",1.3f),(5,"Gurdwara Singh Sabha",0.6f),(6,"Quaker Meeting House",1.2f),(7,"Glasgow Buddhist Centre",0.35f),(8,"Garnethill Synagogue",0.45f),(9,"Glasgow Cathderal",3.3f))
mapBuffer = mapBuffer ++ Map(key -> newList)

key ="Art Route (Kelvingrove Art Gallery and Museum)"
newList = List((1,"Hunterian Art Gallery",1.2f),(2,"MacKintosh Building",2.2f),(3,"Gallery Of Modern Art",1.4f),(4,"St. Mungo Museum Of Religious Life & Art",1.3f),(5,"People's Palace",2.0f),(6,"The Burrell Collection",7.1f),(7,"House For An Art Lover",2.8f),(8,"Kelvingrove Art Gallery and Museum",5.0f))
mapBuffer = mapBuffer ++ Map(key -> newList)

key ="Education Route (GCU)"
newList = List((1,"University Of Strathclyde",0.65f),(2,"City Of Glasgow College - Riverside Campus",1.4f),(3,"School of Simulation and Visualisation",3.9f),(4,"Glasgow Science Centre",0.7f),(5,"University of Glasgow",2.4f),(6,"The Mitchell Library",1.9f),(7,"Glasgow School Of Art",0.9f),(8,"Royal Conservatoire Of Scotland",0.75f),(9,"GCU",0.6f))
mapBuffer = mapBuffer ++ Map(key -> newList)

println("mapBuffer: " + mapBuffer)

////////////////////////////////////
////Functions
def showroute(route: (String, List[(Int, String, Float)])): String = {
  val routetail = (a: (Int, String, Float)) => f"\n ${a._1}\t " + f" ${a._2}\t" + f"${a._3} f"
  var routehead = s"""
                     | Name - ${route._1}
                     |""".stripMargin

  route._2.map(k => routehead = routehead + routetail(k))
  routehead +"\n"
}
def total(tuple: (String, List[(Int, String, Float)])):String= {
  var totaldistance =0f
  tuple._2.foreach(f => totaldistance =totaldistance + f._3)
  f"\t${tuple._1} has ${tuple._2.length} stages and a total distance of ${totaldistance}%2.1f km \n".stripMargin

}
///test1
def showAllRoutes(data:   Map[String, List[(Int, String, Float)]]) = {

  var format = "Routes".stripMargin + "\n"
  for ((x, y) <- data) {
    format = format + showroute((x, y))
  }
}
println(showAllRoutes(mapBuffer))



/////test 2
def showtotal()= {
  var totalroute = ""
  for ((a, b) <- mapBuffer) {
    var totaldistance = 0f

    var route = f"\t${a} has ${b.length} stages and a total distance of ${totaldistance}%2.1f \n".stripMargin
    b.foreach(f => totaldistance = totaldistance + f._3)
    totalroute = totalroute + route
  }
  println(totalroute)
}
showtotal()
// test3
def showAverage(b: Map[String,List[(Int,String,Float)]]):String = {
  var sumstage= 0f //holds the sum of stages of all routes together
  var totalDistance = 0f
  for ((k,v) <- b){
    v.map(n => sumstage =sumstage+ n._1 / v.length)
    v.map(n => totalDistance = totalDistance+ n._3/v.length)
  }
  f"""The average total distance is ${totalDistance}%.1f km and average number of stage of all routes is ${sumstage} .""".stripMargin

}
println(showAverage(mapBuffer))
///test4
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
}
showDescending(mapBuffer)
  ///test 5

  def showUserSelectedRoute(routes: Map[String, List[(Int, String, Float)]],test:Int) = {
    val routeOptions = routes.zipWithIndex.map(_.swap).toMap

    var routeOptionsString: String =
      s"""select the number of the route
         |""".stripMargin
    for ((k, v) <- routeOptions) {
      routeOptionsString = routeOptionsString + s"$k - ${v._1} \n"
    }

    println(routeOptionsString)

    val userSelection: Int = test:Int

    routeOptions.get(userSelection) match {
      case None => "This number is not an option. Please start over"
      case Some(n) => {

        println(total(n).stripMargin)
        println(showroute(n).stripMargin)
      }
    }

  }
showUserSelectedRoute(mapBuffer,test=1)
showUserSelectedRoute(mapBuffer,test=5)
