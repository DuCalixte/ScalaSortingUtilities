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

}