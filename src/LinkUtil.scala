import java.util

import scala.util.Random
import scala.util.control.Breaks

object implicitParam{
  implicit val result:List[Int]=List()
}
object linkMethod{
  def random(count:Int,range:Int):List[Int]={
    (for(i:Int<-1 to count) yield Random.nextInt(range)).toList
  }
  def main(args: Array[String]): Unit = {
    import implicitParam._
    val randomList=random(10,100)
    val node = create(randomList)
    disNode(node)
    val sortedNode=insertionSortList(node)
    disNode(sortedNode)
  }
  def disNode(x:ListNode)(implicit tt:List[Int]): Unit ={
    if (x==null){
      println(tt.reverse.mkString("->"))
    }else {
      disNode(x.next)(x.x :: tt)
    }
  }
  def create(x:List[Int]):ListNode={
    if(x.isEmpty)return null
    else{
      return new ListNode(x.head,create(x.drop(1)))
    }
  }
/*  输入：(2 -> 4 -> 3) + (5 -> 6 -> 4)
  输出：7 -> 0 -> 8
  原因：342 + 465 = 807*/
  def addTwoNumbers(l1:ListNode,l2:ListNode):ListNode={
    var p=0
    var s1=l1
    var s2=l2
    val res:ListNode=new ListNode(0,null)
    var cur=res
    while(s1!=null||s2!=null){
      val x=if (s1==null)0 else s1.x
      val y=if(s2==null)0 else s2.x
      cur.next=new ListNode((x+y+p)%10,null)
      p=(x+y+p)/10
      if(s1!=null)s1=s1.next
      if(s2!=null)s2=s2.next
      cur=cur.next
    }
    if (p>0)cur.next=new ListNode(p,null)
    res.next
  }
  def addTwoNumbers2(l1:ListNode,l2:ListNode):ListNode={
    def calculate(l1:ListNode,l2:ListNode,carryOver:Option[Int]):ListNode={
      if(l1==null&&l2==null&&carryOver.isEmpty)return null
      val sum=(if(l1==null)0 else l1.x)+(if (l2==null)0 else l2.x)+carryOver.getOrElse(0)
      val (p,add)={
        if (sum>9) {
          (Some((sum / 10)), (sum % 10))
        }else{
          (None,(sum))
        }
      }
      val x=new ListNode(add);
      val n1=if(l1==null)null else l1.next
      val n2=if(l2==null)null else l2.next
      x.next=calculate(n1,n2,p)
      return x;
    }
    calculate(l1,l2,None)
  }
  def addTwoNumbers3(l1: ListNode, l2: ListNode): ListNode = {
    def calculate(l1: ListNode, l2: ListNode, carryOver: Option[Int]): ListNode={
      if (l1 == null && l2 == null && carryOver.isEmpty){
        null
      } else{
        val x1 = if(l1 == null) 0 else l1.x
        val x2 = if(l2 == null) 0 else l2.x
        val (n, co) = {
          val sum = x1 + x2 + carryOver.getOrElse(0)
          if (sum > 9){
            (sum%10, Some(sum/10))
          } else{
            (sum, None)
          }
        }
        val r1 = new ListNode(n)
        val n1 = if(l1 == null) null else l1.next
        val n2 = if(l2 == null) null else l2.next
        r1.next = calculate(n1, n2, co)
        r1
      }
    }
    calculate(l1,l2,None)
  }
/*反转链表
  输入: 1->2->3->4->5->NULL, m = 2, n = 4
  输出: 1->4->3->2->5->NULL*/
  def reverseBetween(head: ListNode, m: Int, n: Int): ListNode = {
    def getNode(x:Int,head:ListNode):(ListNode)={
      var p:ListNode=new ListNode(0)
      p.next=head
      var index=x
      var q=p
      while(index>0){
        p=p.next
        index=index-1
      }
      (p)
    }
    val (prev)=getNode(m-1,head);
    val p=prev.next
    var dis=n-m
       Breaks.breakable{
         while(dis>0){
           if(p.next==null)Breaks.break
           val q=p.next
           p.next=q.next
           q.next=prev.next
           prev.next=q
           dis=dis-1
         }
       }
    if(m==1)prev.next else head
  }
  def mergeSortRecursive(head:ListNode):ListNode={
    val tuple = halfLink(head)
    if (tuple._1!=tuple._2) {
      val left=mergeSortRecursive(tuple._1)
      val right=mergeSortRecursive(tuple._2)
      val node = merge(left,right)
      return node;
    }else{
      return tuple._1;
    }
  }
  def mergeSort(head:ListNode):ListNode={
    var cur=head;
    val dummy=new ListNode(0,head)
    var p=dummy
    val len=head.length
    var size=1
    while(size<len){
      cur=dummy.next
      p=dummy
      while(cur!=null){
        val left=cur
        val right=left.drop(size)
        cur=if(right==null)null else right.drop(size)
        p.next=merge(left,right)
        while(p.next!=null){
          p=p.next
        }
      }
      size=size<<1
    }
    dummy.next
  }
  def halfLink(head:ListNode):(ListNode,ListNode)={
    val dummy=new ListNode(0,head)
    var faster=head
    var slow=dummy
    while(faster!=null&&faster.next!=null){
      slow=slow.next
      faster=faster.next.next
    }
    val first=dummy.next
    val next=slow.next
    slow.next=null
    (first,next)
  }
  def merge(l1:ListNode,l2:ListNode):ListNode={
    var p1=l1;
    var p2=l2;
    var p3=new ListNode(-1);
    val p4=p3;
    while(p1!=null&&p2!=null){
      if(p1.x>p2.x){
        p3.next=p2
        p2=p2.next
      }else{
        p3.next=p1
        p1=p1.next
      }
      p3=p3.next
    }
    p3.next=if(p1!=null)p1 else p2
    p4.next
  }
  def mergeRecursive(l1:ListNode,l2:ListNode,cur:ListNode):Unit={
    if (l1==null) cur.next=l2;
    else if (l2==null) cur.next=l1;
    else if(l1.x>l2.x){
      cur.next=l2
      mergeRecursive(l1,l2.next,cur.next)
    }else{
      cur.next=l1;
      mergeRecursive(l1.next,l2,cur.next)
    }
  }
  def findMidRecursive(faster:ListNode,slower:ListNode):ListNode={
    if(faster==null||faster.next==null||faster.next.next==null)slower
    else findMidRecursive(faster.next.next,slower.next)
  }
  def detectCycle(head: ListNode): ListNode = {
    var faster=head
    var slower=head
    do{
      if(faster==null||faster.next==null)return null
      faster=faster.next.next
      slower=slower.next
    }while(faster!=slower)
    var p=head
    while(p!=faster){
      p=p.next
      faster=faster.next
    }
    p
  }
  //4->3->2->1->0
  def insertionSortList(head: ListNode): ListNode = {
    if(head==null||head.next==null)return head
    val prev=new ListNode(-1,head)
    var cur=prev.next
    while(cur!=null&&cur.next!=null){
      var p=prev
      while(cur.next.x>p.next.x){
        p=p.next
      }
      if(p.next!=cur.next) {
        val right = p.next
        val inserted=cur.next
        cur.next=inserted.next
        p.next=inserted
        inserted.next=right
      }else{
        cur=cur.next
      }
    }
    prev.next
  }
}
