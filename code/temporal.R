require(ggmap)
require(ggplot2)
require(reshape2)

get_time_split <- function(df, event_types, percentile){
  results <- data.frame()
  for(e in event_types){
    cut <- df[df$type==e,]
    tab <- data.frame(table(cut$event_time))
    tab$Freq <- 100*tab$Freq/sum(tab$Freq)
    tab$type <- e
    if(NROW(results)==0){
      results <- tab
    } else{
      results <- rbind(results,tab)
    }
  }
  return(results)
}
get_weekday_split <- function(df, event_types, percentile){
  results <- data.frame()
  for(e in event_types){
    cut <- df[df$type==e,]
    tab <- data.frame(table(cut$week_day))
    tab$Freq <- 100*tab$Freq/sum(tab$Freq)
    tab$type <- e
    if(NROW(results)==0){
      results <- tab
    } else{
      results <- rbind(results,tab)
    }
  }
  return(results)
}

# events from event_map.R
# map from event_map.R

events_times <- as.POSIXct(strftime(events$timestamp, format="%H:%M:%S"), 
                           format="%H:%M:%S")
epoch <- as.POSIXct("00:00:00",format="%H:%M:%S")
events_times <- round((as.numeric(events_times) - as.numeric(epoch))/3600)
events_times[events_times==24] <- 0
events$event_time <- events_times

save(events,file='data_set_1_events.RData')

time_df <- get_time_split(events,event_types)
p <- ggplot(time_df, aes(x=Var1, y=Freq, colour=type, group=factor(type)))
p <- p + geom_line() + xlab("hour")
p

events_week_day <- weekdays(as.Date(events$timestamp))
events$week_day <- events_week_day
weekday_df <- get_weekday_split(events,event_types)
weekday_df$Var1 <- factor(weekday_df$Var1, 
                          levels = c("Monday", "Tuesday", "Wednesday", 
                                     "Thursday", "Friday", "Saturday","Sunday"))

p <- ggplot(weekday_df, aes(Var1, Freq, fill = type))
p <- p + geom_bar(stat = "identity",position = "dodge") + xlab("day")
p




