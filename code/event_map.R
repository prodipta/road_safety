require(ggmap)
require(ggplot2)
require(reshape2)

grid_to_map <- function(df, grids, grid_size=100){
  max_lat <- max(df$lat)
  min_lat <- min(df$lat)
  max_long <- max(df$long)
  min_long <- min(df$long)
  lats <- seq(min_lat,max_lat,length.out=grid_size+1)
  longs <- seq(min_long,max_long,length.out=grid_size+1)
  mapper <- function(g){
    j <- ifelse(floor(g/grid_size) == g/grid_size,g/grid_size,floor(g/grid_size)+1)
    i <- g - (j-1)*grid_size
    x <- 0.5*(lats[i]+lats[i+1])
    y <- 0.5*(longs[j]+longs[j+1])
    if(is.na(x))stop(paste('wrong value for g',g))
    if(is.na(y))stop(paste('wrong value for g',g))
    return(list(x=x,y=y))
  }
  mapped <- data.frame(grid=grids,x=rep(0,NROW(grids)),y=rep(0,NROW(grids)))
  for(i in 1:NROW(grids)){
    loc <- mapper(grids[i])
    mapped$grid[i] <- grids[i]
    mapped$x[i] <- loc$x
    mapped$y[i] <- loc$y
  }
  return(mapped)
}
map_to_grid <- function(df, grid_size=100){
  max_lat <- max(df$lat)
  min_lat <- min(df$lat)
  max_long <- max(df$long)
  min_long <- min(df$long)
  lats <- seq(min_lat,max_lat,length.out=grid_size+1)
  longs <- seq(min_long,max_long,length.out=grid_size+1)
  mapper <- function(x,y, lats, longs){
    if(x < lats[1])stop(paste(x,'greater than',lats[1]))
    i <- findInterval(x,lats,all.inside = TRUE)
    j <- findInterval(y,longs,all.inside = TRUE)
    if(i < 1 | i > grid_size)stop(paste('wrong value of lat',x,i))
    if(j < 1 | j > grid_size)stop(paste('wrong value of lat',y,j))
    idx <- (NROW(lats)-1)*(j-1)+i
    return(idx)
  }
  df$grid <- apply(df[,c('lat','long')], 1, 
        function(x) mapper(x[1],x[2],lats,longs))
  return(df)
}
get_top_locations <- function(df, event_type, percentile){
  df_cut <- df[df$type==event_type,]
  cut <- data.frame(table(df_cut$grid))
  threshold <- quantile(cut$Freq,percentile)
  locations <- as.numeric(as.character(cut$Var1[cut$Freq > threshold]))
  return(locations)
}
create_types_data <- function(df, event_types, percentile = 0.9, grid_size=100){
  bad_locations <- data.frame()
  for(e in event_types){
    out <- get_top_locations(df, e, percentile)
    latlong <-grid_to_map(df,out,grid_size)
    latlong$type <- e
    print(paste('event:',e,'size',NROW(latlong)))
    if(NROW(bad_locations)==0){
      bad_locations <- latlong
    } else{
      bad_locations <- rbind(bad_locations, latlong)
    }
  }
  bad_locations$grid <- NULL
  return(bad_locations)
}

map1 <- get_map(location = 'Ahmedabad', zoom = 12, maptype = 'roadmap')
#map2 <- get_map(location = 'Marathahalli', zoom = 12, maptype = 'roadmap')

file_name <- 'data_set_1.csv'   # 'data_set_1.csv'
map_name <-  map1
ts_format <- '%d.%m.%Y %H:%M:%S'
#ts_format <- '%Y-%m-%dT%H:%M:%OSZ'
grid_size <- 100
percentile <- 0.9

events <- read.csv(file_name)
#events$type[events$type=='UFCW'] <- 'FCW'
#events$type[events$type=='LDWR'] <- 'LDWL'
event_types <- unique(events$type)
events <- map_to_grid(events, grid_size = grid_size)
events$checksum <- paste0(events$type,events$timestamp,events$grid)
events <- events[!duplicated(events$checksum),]
events$timestamp <- as.POSIXct(events$timestamp, format=ts_format)
bad_locations <- create_types_data(events, event_types, percentile, grid_size)

map <- map_name
p <- ggmap(map)
p <- p + geom_point(data=bad_locations, 
                    aes(x=y, y=x, colour=type),size=3, alpha=0.5)
p

