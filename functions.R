
distance_between_points <- function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
}

generate_line <- function(x1,y1,x2,y2){
  output = list("a" = (y2-y1),"b"=(x1-x2),"c" = (y1*x2-y2*x1))
  return(output)
}

is_point_above <- function(a,b,c,x,y){
  return(a*x+b*y+c >= 0)
}
parallel_line_thru_point <- function(a,b,c,x,y){
  
  output = list("a" = a,"b"=b,"c" = (-a*x-b*y))
  return(output)
}

perpendicular_line_thru_point <- function(a,b,c,x,y){
  output = list("a" = -b,"b"=a,"c" = (b*x-a*y))
  return(output)
}

distance_between_parallel_lines <- function(a1,b1,c1,a2,b2,c2){
  if(a1/b1 != a2/b2) {return("Not Parallel")}
  output = abs((c2*a1/a2 - c1))/sqrt(a1^2 + b1^2)
  return(output)
}

perpendicular_distance_from_point_to_line <- function(a,b,c,x,y){
  output <- abs(a*x + b*y +c )/sqrt(a^2 + b^2)
  return(output)
}

