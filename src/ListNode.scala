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
    p.next
  }

}
