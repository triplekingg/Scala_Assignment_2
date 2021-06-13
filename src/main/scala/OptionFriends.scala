
object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] = {
    if(xs.isEmpty) None else {
      if(xs.head._1 == key) Some(xs.head._2) else lookup(xs.tail, key)
    }
  }

  def resolve(userIdFromLoginName: String => Option[String],
              majorFromUserId: String => Option[String],
              divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = {
    def first(loginName:String): Option[String] = loginName.{
      if (loginName.isEmpty) None
      else Some(" Received user id from login name")
    }
    def second(majorFromUserId:String): Option[String] = { first(loginName).flatMap(x => Some(x))}
    def third(divisionFromMajor:String): Option[String] = {
      if (divisionFromMajor.isEmpty) None
      else Some(" Received division from major")
    }
    def fourth(averageScoreFromDivision:String): Option[Double] = {
      if (averageScoreFromDivision.isEmpty) Some(0.0)
      else Some(50)
    }
    if(loginName.isEmpty) 0.0
    else resolve(first,second,third,fourth,"hi")
  }



//  val testing = List(("310", "Manav"),("500", "kv"),("310", "Manit"),("544", "Rit"),("891", "Sammy"))
//  println(lookup(testing,"310"))

  val a = resolve("hi",)
}
