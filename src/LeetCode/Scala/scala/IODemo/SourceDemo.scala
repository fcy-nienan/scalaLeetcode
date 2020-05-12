package LeetCode.Scala.scala.IODemo

import scala.io.Source

object SourceDemo {
  def main(args: Array[String]): Unit = {
//    fromFile得到的结果进行foreach是针对每一个字符的
//    Source.fromFile("F:\\Thread.txt","utf-8").foreach(print)
    val regex="(?<=from )\\btbl_\\w*\\b".r
    val str=Source.fromFile("E:\\test.java").mkString
    (regex findAllIn str).toList.distinct.foreach(println)

  }
}
