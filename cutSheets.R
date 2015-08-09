cutSheets <-function(data,start,finish){
#   Name: cutSheets
#   Input: A 3D array of data values, a start index, a finish index
#   Output: A 3D array 
#   Description:  cutSheets takes in a 3D array.  It slices the 3D array based off
#   of the start and finish indexes.  For example, in the domain of sea surface temperature,
#   this function would return an array of the Sea Surface Temperature grids from the
#   start index year to the end index year.
  
  #type check
  if (!is.array(data)){
    stop("Input data must be an array.")
    
  }
  #data is a 3D array
  dim <-dim(data)
  #number of 2D arrays in the 3D array
  grid_count <-dim[3]

  #dimensions of each array in 3D array
  rows <-dim[1]
  cols <-dim[2]
  
  if (finish > grid_count || start <1 ){
    #we are trying to get a grid that doesn't exist
    #throw an error
    if (finish > grid_count){
      stop("Ending Index is larger than the number of sheets in the 3D array")
    }
  else{
    stop("Starting index must be positive.")
  }
  }
  
  #the new 3D array will contain this many
  #2D arrays.
  new_grid_count <- finish-start+1
  
  ret_grids <-array(0:0,dim=c(rows,cols,new_grid_count))
  m <-1
  
  #for k grids
  for (k in start: finish){
    #get the grids we want
    ret_grids[,,m] <-data[,,k]
    m<-m+1
  }
  
  return(ret_grids)
}


cutSheets(gom_sst,11,250)