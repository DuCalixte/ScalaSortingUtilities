package mergeSort.Utilities

object MergeSort {
  
  /* 
   * Below is basic generic merge sort written in scala
   */
  def genericMergeSort[T <% Ordered[T]:ClassManifest](list:List[T]):List[T]={
    def genericMerge[T <% Ordered[T]:ClassManifest](left:List[T],right:List[T]):List[T]={
      var res:List[T]=Nil
      var lft:List[T]=left
      var rht:List[T]=right
      
      while(lft != Nil || rht != Nil){
        if(lft!=Nil&&rht!=Nil){
          if (lft.head <= rht.head){
            res = res :+ lft.head
            lft = lft.tail
          }
          else{
            res = res :+ rht.head
            rht = rht.tail
          }
        }
        else if (lft != Nil){
          res = res :+ lft.head
          lft = lft.tail
        }
        else if (rht != Nil){
          res = res :+ rht.head
          rht = rht.tail
        }
      }
      return res
    }
    if (list == Nil) return Nil
    if (list.length <= 1) return list
    
    val length = list.length
    val (left, right) = list.splitAt(length/2)
    
    return genericMerge(genericMergeSort(left), genericMergeSort(right))
    
  }
  
  def standardmergeSort[T <% Ordered[T]:ClassManifest](list:List[T]):List[T]= list match{
    case List() => list
    case single::Nil => list
    case _ =>
      def merge[T <% Ordered[T]:ClassManifest](left:List[T],right:List[T]):List[T]= (left, right) match{
        case (_, Nil)=>left
        case (Nil, _)=>right
        case (l::_left, r::_right)=>
          if (l <= r) l::merge(_left,right)
          else r::merge(left,_right)
      }
      val(left,right) = list.splitAt(list.length/2)
      merge(mergeSort(left), mergeSort(right))
  }
  
  def mergeSort[T <% Ordered[T]:ClassManifest](list:List[T]):List[T]= list match{
    case Nil | List(_) => list
    case _ =>
      def merge[T <% Ordered[T]:ClassManifest](left: List[T], right: List[T]): Stream[T] = (left, right) match {
        case (l :: _left, r :: _right) if l <= r => l #:: merge(_left, right)
        case (left, r :: _right) => r #:: merge(left, _right)
        case (left, Nil)=>left.toStream
        case (Nil,right)=>right.toStream
        case _ => Stream()
      }
      val split = list.length /2
      val (left, right) = list splitAt split
      merge(mergeSort(left), mergeSort(right)).toList
  }

}