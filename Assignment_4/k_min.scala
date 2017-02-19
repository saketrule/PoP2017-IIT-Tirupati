import scala.annotation.tailrec
object k_min{
    def main(args:Array[String]):Unit = {
        var arr:Array[Int] = Array(5,32,12,2,-13,3,2,7,41,53,25,4,23,42,33,53,64,42,454,65,7545,54,54)
        println(s"Kth minimum element in given array is: ${find_min(4,arr,0,arr.length-1)}")    
        }
    //Function to find kth minimum element in arrayy in Linear time complexity
    def find_min(k:Int,arr:Array[Int],beg:Int,fin:Int):Int = {
        //Function to partition the array based on comparison with pivot element, returns position of partition
        def sort_pivot(pivot:Int,arr:Array[Int],beg:Int = 0,fin:Int = 0):Int = {
            var p1 = beg;
            var p2 = fin;
            while(true){
                while(p1<arr.length && arr(p1)<=pivot)
                    p1 = p1+1
                while(p2>=0 && arr(p2)>pivot)
                    p2 = p2-1
                if(p1>p2)
                    return p1-1;
                swap(p1,p2,arr)
            }
                return -1;
        }
        
        def swap(i:Int,j:Int,arr:Array[Int]):Unit = {
            val temp = arr(i);
            arr(i) = arr(j);
            arr(j) = temp;
        }
    
        //println(s"find_min ${k},${beg},${fin}")
        if(k>fin-beg+1)    
            return -1
        if(beg==fin && k==1)    return arr(beg);
        //Make an array of medians
        if(fin-beg>5){
            var medians:Array[Int] = new Array[Int]((fin-beg)/5)  
            for(i <- 0 to (fin-beg)/5 -1)
                medians(i) = arr(beg+i*5);
            val med = find_min(medians.length/2+1,medians,0,medians.length-1)
            var i=0;
            while(arr(i)!=med)
                i = i +1;
            swap(0,i,arr);
        }
        val pivot = arr(beg)
        val s1 = sort_pivot(pivot,arr,beg+1,fin);
        if(s1==k+beg-1)
            pivot;
        else if(s1>=k+beg)
            find_min(k,arr,beg+1,s1)
        else
            find_min(k+beg-s1-1,arr,s1+1,fin)       
    }
}