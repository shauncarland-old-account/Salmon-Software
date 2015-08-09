calcDistance <- function(lat1,lon1,lat2,lon2){
  
#   Name: calcDistance
#   Input: Two latitude points, two longitude points (Floats or Integers)
#   Output: A float
#   Description: This function calculates the distance between two lat/lon points.
  
  #Type check
  if (!is.numeric(lat1) || !is.numeric(lon1) || !is.numeric(lat2) || !is.numeric(lon2)){
    stop("All lat/lon points must be numerics.")
  }
  
  rad <- pi/180
  
  a1 <-lat1*rad
  a2 <-lon1*rad
  b1 <-lat2*rad
  b2 <-lon2*rad
  
  dlon <-b2-a2
  dlat <-b1-a1
  
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  
  return (d)
}
