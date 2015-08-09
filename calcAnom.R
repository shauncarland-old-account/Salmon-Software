calcAnom <-function(data,avgs){
  
  
#   Name: calcAnom
#   Input: A 3D array and a 2D array
#   Output: A 3D array
#   Description: calcAnom takes in a 2D array containing average values for a base period.  
#   It then iterates through all of the arrays in data, a 3D array.  For each array, calcAnom 
#   calculates the difference between each element in the array from the corresponding element 
#   in the base period array.  This information is then put into a 2D array.  All of these 2D arrays
#   are packaged into a 3D array and returned. 
  
  #get the dimensions
  dim <-dim(data)
  grid_count <-dim[3]
  rows <-dim[1]
  cols <-dim[2]
  ret_arr <-array(dim=grid_count)
  ret_grids <-array(dim=c(rows,cols,grid_count))
  
  #iterate over the 2D arrays in the data set
  for (k in 1: grid_count){
    #iterate over the rows
    for (i in 1: rows){
      #iterate over the columns
      for (j in 1: cols){
        #get cur val
        x<-data[i,j,k]
        #in sea surface temperature data, ice is 
        #given a value of -1.8.  comment this part out
        #if you do not want to ignore ice values.
        ice <- -1.8
        z <- abs(x-ice)
        z <-round(z,2) 
        if (identical(z,0)==TRUE){
          x <-0
        }
        #calculate the current anom
        anom <- x - avgs[i,j]
        #put it in its place
        ret_grids[i,j,k]<-anom
      }#end j loop
    }#end i loop
  }#end k loop
  #return and end
  return (ret_grids)   
}

#example
base <- calcMeans(cutSheets(gom_sst,1,10))
calcAnom(cutSheets(gom_sst,11,250),base)