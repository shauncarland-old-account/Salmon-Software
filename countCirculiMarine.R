countCirculiMarine <-function(dataSet){
#   Name: countCirculiMarine
#   Input: A data frame of marine circuli from a salmon scale dataset
#   Output: A numerical Vector
#   Description: This function takes in a data set of marine circli.  For each fish (row in the data set) it 
#   counts the number of values until it reaches an NA value.  Then, it stores this count.  
#   This procedure is repeated for N fish in the data set.
#   
  
  circ_list <-c()
  for (i in 1: length(dataSet[,1])){
    
    fish <-dataSet[i,]
    j <-1
    while (TRUE){
      curCirc <-fish[j]
      if (is.na(curCirc)){
        break
      }
      else if (j+1 > length(fish)){
        break
      }
      else{
        j<-j+1
      }
    }
    circ_list[i] <-j
    
  }
  return(circ_list)
}
