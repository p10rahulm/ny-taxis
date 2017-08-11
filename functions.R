
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

give_col_no <- function(mb,ln,lt,is_ss){
  if(is_ss){return(2*(110*(mb-1)+11*(ln-1)+lt))}
  else{return(2*(110*(mb-1)+11*(ln-1)+lt)-1)}
}


# ---------------------
# Intrabucket transfer
# ---------------------


give_intrabucket_path <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,dropoff_map_buckets,dropoff_longitude,dropoff_latitude){
  output <- c()
  if(pickup_latitude!=dropoff_latitude){
    for(i in (pickup_latitude + (dropoff_latitude >pickup_latitude) - (dropoff_latitude<pickup_latitude)):dropoff_latitude){
      output <- c(output,give_col_no(pickup_map_buckets,pickup_longitude,i,F))
    }
  }
  if(pickup_longitude!=dropoff_longitude){
    for(i in (pickup_longitude + (dropoff_longitude >pickup_longitude) - (dropoff_longitude<pickup_longitude)):dropoff_longitude){
      output <- c(output,give_col_no(pickup_map_buckets,i,dropoff_latitude,F))
    }
  }
  output <- output[-length(output)]
  if(length(output)==0){return(as.numeric(NA))}
  return(output)
}

give_path <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,
                      dropoff_map_buckets,dropoff_longitude,dropoff_latitude,
                      pickup_bucket_coord,dropoff_bucket_coord
                      ){
  if(pickup_map_buckets==dropoff_map_buckets){
    return(give_intrabucket_path(pickup_map_buckets,pickup_longitude,pickup_latitude,
                                 dropoff_map_buckets,dropoff_longitude,dropoff_latitude))
  } 
  pickupbucket_lon <- as.numeric(unlist(strsplit(as.character(pickup_bucket_coord),"")))[2]
  pickupbucket_lat <- as.numeric(unlist(strsplit(as.character(pickup_bucket_coord),"")))[1]
  dropbucket_lon <- as.numeric(unlist(strsplit(as.character(dropoff_bucket_coord),"")))[2]
  dropbucket_lat <- as.numeric(unlist(strsplit(as.character(dropoff_bucket_coord),"")))[1]
  # First the lats
  
  
  if(pickupbucket_lat!=dropbucket_lat){
    start <- min(pickupbucket_lat,dropbucket_lat)
    stop <- max(pickupbucket_lat,dropbucket_lat)
    
    if(start>stop+1){
      give_intrabucket_path(pickup_map_buckets,pickup_longitude,pickup_latitude,
                            dropoff_map_buckets,dropoff_longitude,dropoff_latitude)
    }
  }
  
}

give_intrabucket_path_string <- function(pickup_map_buckets,pickup_longitude,pickup_latitude,dropoff_map_buckets,dropoff_longitude,dropoff_latitude){
  output <- c()
  if(pickup_latitude!=dropoff_latitude){
    for(i in (pickup_latitude + (dropoff_latitude >pickup_latitude) - (dropoff_latitude<pickup_latitude)):dropoff_latitude){
      output <- c(output,give_col_no(pickup_map_buckets,pickup_longitude,i,F))
    }
  }
  if(pickup_longitude!=dropoff_longitude){
    for(i in (pickup_longitude + (dropoff_longitude >pickup_longitude) - (dropoff_longitude<pickup_longitude)):dropoff_longitude){
      output <- c(output,give_col_no(pickup_map_buckets,i,dropoff_latitude,F))
    }
  }
  output <- output[-length(output)]
  if(length(output)==0){return(as.character(NA))}
  return(paste(output,collapse = "|"))
}


return100cols <- function(vector){
  return(c(vector,rep(as.numeric(NA),100-length(vector))))
}
