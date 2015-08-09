calcMeans <-function(data_grid){
  
#   Name: calcMeans
#   Input: 3D array (array of 2D arrays)
#   Output: 2D array
#   Description: This function takes in an array of 2D arrays (a 3D array).  It calculates 
#   the mean average for each slot in the 2D arrays over the array of arrays.  It returns 
#   a 2D Array with each element corresponding to the mean of the elements in that 
#   position of the 3D array.

  browser()
  
  #type check
  if (!is.array(data_grid)){
    stop("Input must be an array.")
  }
  
  dim <-dim(data_grid)
  
  #number of grids in 3D array
  grid_count <-dim[3]
  #dimensions of each grid
  rows <-dim[1]
  cols <-dim[2]


  #in sea surface temperature data, values of -1.8 correspond
  #to ice.  Thus, they should not be included.  Comment this out
  #if using other data, or if one wants to include ice.
  data_grid[round(data_grid,1)== -1.80] <-0
  
  #now, calculate the mean.  Return this
  x <-apply(data_grid,MARGIN=c(1,2),FUN=mean,na.rm=TRUE)
  return (x)
}

calcMeans(cutSheets(gom_sst,11,250))