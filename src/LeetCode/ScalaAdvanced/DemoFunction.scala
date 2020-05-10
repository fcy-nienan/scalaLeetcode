package LeetCode.ScalaAdvanced


import scala.collection.mutable
import java.net.InetAddress


import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DemoFunction {
  def main(args: Array[String]): Unit = {
    printlnDemo()
  }
  def printlnDemo():Unit={//字符串插值
    var str="hello world"
    println(s"str is :$str")
    println(Console.BOLD)
    println(s"str is :$str")
    println(Console.UNDERLINED)
    println(s"str is :$str")
    println(Console.RESET)
    println(s"str is :$str")
    println(Console.RED)
    println(s"str is :$str")

    val glazedDonut = "Glazed Donut"
    val unitPrice = 2.50
    val qtyPurchased = 10
    val totalCost = qtyPurchased * unitPrice
    println(f"""Total cost of $qtyPurchased $glazedDonut${if (qtyPurchased > 1) "s" else ""} = $$$totalCost%1.2f""")

    val codes = Seq("A", "BB", "CCC", "DDDD", "EEEEE")
    val codesWithLength = codes.map { code =>
      s"$code - ${code.length}"
    }

    codesWithLength.foreach(println(_))


    val item1 = Tuple3("T-Shirt", "Medium", 10.99)
    val item2 = Tuple3("Polo-Shirt", "Large", 4.99)
    val item3 = Tuple3("Vest", "Small", 5.99)
    val item4 = Tuple3("T-Shirt", "Small", 4.99)
    val item5 = Tuple3("T-Shirt", "Small", 4.99)

    val shoppingBasket = List(item1, item2, item3, item4, item5)
    shoppingBasket.foreach {
      case item if item._1 == "T-Shirt" =>
        println(s"${item._1.toUpperCase()} is priced at $$${item._3} for the ${item._2} size.")
      case item =>
        println(s"${item._1.toLowerCase()} is priced at $$${item._3} for the ${item._2} size.")
    }

    val vecNumbers = Vector(0, 10, 20, 47, -2, 99, -98)
    println(s"The smallest item in the Vector = ${vecNumbers.min}")
    println(s"The largest item in the Vector = ${vecNumbers.max}")

  }
  def ListDemo():Unit={
    var list=ListBuffer(1,2,3,4,5)
    list+=3
    list :+ 3
    list partition (_<3)
    val tuple = list.partition(_<3)
    println(tuple._1)
    println(tuple._2)
    println(list)
  }
  def ArrayDemo():Unit={
    val x=Array(1,2,3,4,5)
    println(x.indices)
    List(List(1,2,3),List(2,3,4)).flatten
  }

  def spark():Unit={

  }
  def stringOutput():Unit={
    //三个引号会输出其包含的所有内容(空格和table等)
    //在每行前面加一个'|'会左对齐
    //每行前面这个'|'可以定制,通过stripMargin方法
    val str=
      """
        hello world
        new line
        a new line
        fcy
      """.stripMargin
    val str1=
      """
        |hello world
        |new line
        |a new line
        |fcy
      """.stripMargin
    val str2=
      """
        #hello world
        #new line
        #a new line
        #fcy
      """.stripMargin('#')
    println(str)
    println(str1)
    println(str2)
  }
}
