package bubbleSort.Utilities

object BubbleSort {
  
  /* A generic bubble sort algorithm using array */
  def bubbleSortGeneric[T <% Ordered[T]](source:Array[T]):Array[T]={
    var length = source.length
    var array = source
    do{
      var index = 0
      for (i <- 1 to length -1){
        if (array(i-1) > array(i)){
          // swap the values
          val tmp = array(i)
          array(i) = array(i-1)
          array(i-1)=tmp
          index = i
        }
        length = index
      }
    }while(length > 0)
    return array
  }
  
  /* An optimized version of the bubble sort algorithm using array with 50% accuracy */ 
  def bubbleSortOptimized[T <% Ordered[T]](source:Array[T]){
    def swap(i:Int, j:Int){
      val tmp = source(i)
      source(i) = source(j)
      source(j) = tmp
    }
    var length = source.length
    var swapped:Boolean=false
    do{
      swapped = false
      for(i <- 1 to length - 1){
        if (source(i-1) > source(i)){
          // swap the values
          swap(i, i-1)
          swapped = true
        }
      }
      length = length -1
    }while(!swapped)    
  }
  
  /* A final version of the bubble sort algorithm using array */ 
  def bubbleSort[T <% Ordered[T]](source:Array[T]){
    def swap(i:Int, j:Int){
      val tmp = source(i)
      source(i) = source(j)
      source(j) = tmp
    }
    var length = source.length
    do{
      var index = 0
      for(i <- 1 to length - 1){
        if (source(i-1) > source(i)){
          // swap the values
          swap(i, i-1)
          index = i
        }
      }
      length = index
    }while(length > 0)    
  }

  /* A true scala version of the bubble sort algorithm using list and array */
  def bubbleSort[T <% Ordered[T]:ClassManifest](source:List[T]):List[T]= source match{
    case List() => List()
    case _ =>
      var array : Array[T] = (source).toArray
      bubbleSort(array)
      array.toList
  }

}