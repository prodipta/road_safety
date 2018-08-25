
get_neighbours <- function(g, grid_size){
  i <- ifelse(floor(g/grid_size) == g/grid_size,g/grid_size,floor(g/grid_size)+1)
  j <- g - (i-1)*grid_size
  left <- ifelse(j==1,0,g-1)
  right <- ifelse(j==grid_size,0,g+1)
  top <- ifelse(i==1,0,(i-2)*grid_size+j)
  bottom <- ifelse(i==grid_size,0,i*grid_size+j)
  topleft <- ifelse(i!=1 & j!=1,(i-2)*grid_size+(j-1),0)
  topright <- ifelse(i!=1 & j!=grid_size,(i-2)*grid_size+(j+1),0)
  bottomleft <- ifelse(i!=grid_size & j!=1,i*grid_size+(j-1),0)
  bottomright <- ifelse(i!=grid_size & j!=grid_size,i*grid_size+(j+1),0)
  neighbours <- c(left,right,top,bottom,topleft,topright,bottomleft,bottomright)
  return(unique(neighbours))
}
recent_events <- function(df,dt, grid,event_types,timespan = 5, grid_size=100){
  type_df <- data.frame(matrix(0,1,NROW(event_types)))
  colnames(type_df) <- event_types
  neighbours <- c(get_neighbours(grid, grid_size),grid)
  df_cut <- df[df$timestamp < dt & df$timestamp > (dt-timespan*60),
               c('type','timestamp','grid')]
  types <- df_cut$type[df_cut$grid %in% neighbours]
  if(NROW(types)>0){
    for(i in 1:NROW(types)){
      i <- match(types[i],event_types)
      type_df[i] <- type_df[i]+1
    }
  }
  return(type_df)
}

n <- NROW(events)
results <- data.frame(case=NA,event=NA,value=NA,unit=NA,grid=NA)

df <- lapply(1:n,FUN=function(i){
  case <- i
  if(case%%1000==0)print(paste('done case', case))
  time_bucket <- events$event_time[i]
  value <- events$value[i]
  unit <- events$unit[i]
  event <- events$type[i]
  grid <- events$grid[i]
  past_events <- recent_events(events,events$timestamp[i],grid, event_types,5,100)
  out <- data.frame(case=case,event=event,value=value,unit=unit,
                    grid=grid, time=time_bucket)
  out <- cbind(out,past_events)
})
df <- data.frame(matrix(unlist(df), nrow=n, byrow=T))
colnames(df) <- c('case','event','value','unit','grid', 'time')

event_labels <- levels(df$event)
levels(df$event) <- seq(1,NROW(event_labels))
df$event <- as.numeric(df$event)
unit_df <- data.frame(model.matrix(~df$unit-1))
df <- cbind(df,unit_df)
df$unit <- NULL

df$grid <- factor(df$grid)
grid_df <- data.frame(model.matrix(~df$grid-1))
df <- cbind(df,grid_df)
df$grid <- NULL
save(df,file = 'data_set_1_df.RData')

train_idx <- sample(1:n,round(n*0.65))
test_idx <- which(!(1:n %in% train_idx))
train_df <- df[train_idx,]
test_df <- df[test_idx,]
save(train_df, file='data_set_1_train_df.RData')
save(test_df, file='data_set_1_test_df.RData')

rm(df); rm(grid_df); rm(unit_df); rm(events)


