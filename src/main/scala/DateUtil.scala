object DateUtil extends App {
  type Date = (Int, Int, Int)

  def isOlder(x: Date, y: Date): Boolean = {
    if(x==y) false // If dates are equal
    else{
      if(x._3<y._3) true
      else if(x._3==y._3){ // If years are equal check month
        if(x._2<y._2) true
        else if(x._2==y._2){ // If months are equal check days
          if(x._1<y._1) true
          else false
        }
        else false
      }
      else false
    }
  }

  def numberInMonth(xs: List[Date], month: Int): Int = {
    def count(xs: List[Date],index:Int, counter:Int): Int = {
      if(index==xs.length) counter
      else{
        if(xs(index)._2==month) count(xs,index+1,counter+1)
        else count(xs,index+1,counter)
      }
    }
    count(xs,0,0)
  }

  def numberInMonths(xs: List[Date], months: List[Int]): Int = {
    def helper(counter:Int,xs: List[Date], months: List[Int], month_index:Int):Int = {
      if(month_index==months.length) counter
      else{
        helper(counter+numberInMonth(xs,months(month_index)),xs,months,month_index+1)
      }
    }
    helper(0,xs,months,0)
  }

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    def count(xs: List[Date],index:Int, ys:List[Date]): List[Date] = {
      if(index==xs.length) ys
      else{
        if(xs(index)._2==month) count(xs,index+1,xs(index)::ys)
        else count(xs,index+1,ys)
      }
    }
    count(xs,0,List()).reverse
  }

  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    def helper(xs: List[Date],index:Int, months: List[Int], ys:List[List[Date]]): List[List[Date]] = {
      if(index==months.length) ys
      else{
        val y = datesInMonth(xs,months(index))
        helper(xs, index+1, months, y::ys)
      }
    }
    helper(xs,0,months,List()).reverse.flatten
  }

  def dateToString(d: Date): String = {
    val months = List("January", "February","March","April","May","June","July","August","September","October","November","December")
    months(d._2-1)+"-"+d._1.toString+"-"+d._3
  }

  def whatMonth(n: Int, yr: Int): Int = ???

  def oldest(dates: List[Date]): Option[Date] = ???

  def isLeapYear(d:Int): Boolean = { //Helper function to check for leap year
    if(d % 4 == 0) //Check for leap year
    {
      if( d % 100 == 0)
      {
        if ( d % 400 == 0)
          true
        else
          false
      }
      else
        true
    }
    else {
      false
    }
  }

  def isReasonableDate(d: Date): Boolean = {
    val isLeap = isLeapYear(d._3)
    if((d._1>0 && d._1<=31)&&(d._2>0 && d._2<=12)&&(d._3>0)){ //Check if date possibly makes sense
      if(d._2==2){ //if month is Februaury
        if(isLeap==true){
          if(d._1<=29){ //day can be from 1 to 29
            true
          }
          else false
        }
        else{ //if not leap year
          if(d._1<=28){ //day can be from 1 to 28
            true
          }
          else false
        }
      }
      else if(d._2<=6 && d._2%2==0){ //check if the month is even till june except for February
        if(d._1<=30){
          true
        }
        else false
      }
      else if(d._2>=6 && d._2%2==0){ //check if the month is even after june
        if(d._1<=31){
          true
        }
        else false
      }
      else if(d._2%2!=0 && d._2<=7){//Check if month is odd till july
        if(d._1<=31){
          true
        }
        else false
      }
      else if(d._2%2!=0 && d._2>=9){//Check if month is odd from September onwards
        if(d._1<=30){
          true
        }
        else false
      }
      else{
        false
      }
    }
    else{
      false
    }
  }

//  println(isReasonableDate((30,9,2000)))
  val a:Date = (2,1,2000)
  val b:Date = (2,1,2001)
  val c:Date = (2,12,2000)
  val d:Date = (3,1,2000)
  val lst1 = List(3,12,1)
  val lst = List(a,b,c,d)
//  println(datesInMonth(lst,1))
//  println(datesInMonths(lst,lst1))
  println(dateToString(c))
//  println(numberInMonth(lst,9))
//  println(numberInMonths(lst,lst1))
//  println(isOlder(d,c))
}
