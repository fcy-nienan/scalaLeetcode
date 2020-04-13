class ListNode(var x:Int,var next:ListNode) {
  def this(x:Int){
    this(x,null)
  }
  def drop(x:Int): ListNode ={
    var p=new ListNode(-1,this)
    var n=x;
    while(n>0){
      if(p==null)return null;
      p=p.next
      n=n-1;
    }
    val res=p.next
    p.next=null
    res
  }
  def take(x:Int):ListNode={
    val h=new ListNode(-1)
    var now=this;
    var p=h
    for(_ <- 1 to x){
      if(now==null)return h.next
      val t=new ListNode(now.x)
      p.next=t
      p=t
      now=now.next
    }
    h.next
  }
  def length():Int={
    var p=this
    var len=0
    while(p!=null){
      len=len+1
      p=p.next
    }
    len
  }
  def lengthRecursive(x:ListNode):Int={
    if(x.next==null)1
    else lengthRecursive(x.next)+1
  }
}
