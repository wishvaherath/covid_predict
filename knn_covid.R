
library(tidyverse)
library(tsfknn)

#modifiers
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#this is the oririnal file
#data_file = "../csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# get the latest file from github
data_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
search_location = "_US"
threshold = 100 #cutoof for the trimming of the time series
days_past = 5 #days to go back for the past projection
days_future = 5 #days to go forward for the future projection
min_r2 = 0.7
max_r2 = 0.95






get_query_df <- function(main_df, search_location){
  #load data csv
  df = main_df
  #combine province and country
  df = df %>% unite("location", Province.State, Country.Region, sep="_", remove = F)
  all_locations = df$location %>% unique
  
  data_row = df %>% filter(location == search_location)
  #remove all but the time series
  
  data_vector = as_vector(data_row %>% dplyr::select(starts_with("X")))
  
  
  #finding the index of the first value greater than "Threshold"
  threshold_index = 0
  counter = 1
  for (i in data_vector){
    if (i >= threshold){
      threshold_index = counter 
      break
    }
    counter = counter + 1
  }
  
  total_length = length(data_vector)
  length_after_trim = total_length - threshold_index
  
  #data vector that contains the original dataset used for training
  usable_data_vector = data_vector[threshold_index:total_length]
  
  if (length(usable_data_vector) < 10) {
    print(paste("Warning: small vector for ", search_location))
    next
  }
  
  #future_df is input_df x values plus days_future
  future_day_list = 1:(length(usable_data_vector) + days_future)
  future_df = tibble(x=future_day_list)
  
  
  #past_df cotains the input_df data minus the selected days in days_past
  past_df = tibble(y=usable_data_vector[1:(length(usable_data_vector)-days_past)], x=1:(length(usable_data_vector)-days_past))
  
  
  #input_df contains the original data used for testing
  input_df = tibble(y=usable_data_vector, x=1:length(usable_data_vector)) %>% add_column(class="CSSE_data")
  #return(input_df)
  
  
  
  
  
  #temp_now = carrotify(input_df, future_df, min_r2, max_r2, caret_methods) %>% add_column(location = search_location)
  #temp_past = carrotify(past_df, future_df, min_r2, max_r2, caret_methods) %>% add_column(location = search_location)
  
  return(list(now=input_df, past=past_df, future=future_df)) 
  
}


df = read.csv(data_file)
dx = get_query_df(df, "_US")


pred_now <- knn_forecasting(ts(dx$now$y), h = 7, lags = 1:7, msas = "MIMO")
autoplot(pred_now, highlight = "neighbors", faceting = FALSE)

pred_past <- knn_forecasting(ts(dx$past$y), h = 7, lags = 1:7, msas = "MIMO")
autoplot(pred_past, highlight = "neighbors", faceting = FALSE)

pred$prediction
ro <- rolling_origin(pred, h = 2)
ro$global_accu


nls_model <- nls(y ~ (x ^ b) + c, dx$now,  start=c(b=2, c=0), trace=T)
df_nls <- dx$future
df_nls <- add_column(df_nls, y=(predict(nls_model, dx$future))) %>% add_column(class="predicted_nonlinear_ls")


nls_model_past <- nls(y ~ (x ^ b) + c, dx$past,  start=c(b=2, c=0), trace=T)
df_nls_past <- dx$future
df_nls_past <- add_column(df_nls_past, y=(predict(nls_model_past, dx$future))) %>% add_column(class="predicted_nonlinear_ls_PAST")


all_df <- df_nls %>% bind_rows(dx$now) %>% bind_rows(df_nls_past)

#calcualte R2
alpha = head(df_nls, n=nrow(df_nls)-5)$y
beta = dx$now$y

cor(alpha, beta)^2

now_last = tail(df_nls, n=1)
past_last = tail(df_nls_past, n=1)


now_final = tail(df_nls, n=days_future)
past_final = tail(df_nls_past, n=days_future)



g = ggplot() + geom_point(data=dx$now, aes(x=x, y=y), color="black", alpha=0.5) 
g = g + geom_line(data=now_final, aes(x=x, y=y), color="red", alpha=0.5)  + geom_point(data=now_final, aes(x=x, y=y), color="red", alpha=0.5) 
g = g + geom_line(data=past_final, aes(x=x, y=y), color="blue", alpha=0.5)  + geom_point(data=past_final, aes(x=x, y=y), color="blue", alpha=0.5) 
g = g + geom_curve(aes(x =past_last$x, y = past_last$y, xend = now_last$x, yend = now_last$y),curvature=-0.2, alpha=0.5, arrow = arrow(length = unit(0.05, "npc")))

g



g = ggplot(data=all_df, aes(x=x, y=y, group=class, color=class)) + geom_line( alpha=0.5) + geom_point(data=dx$now, aes(x=x, y=y), color="black", alpha=0.5) 
g = g + geom_curve(aes(x =past_last$x+4, y = past_last$y, xend = now_last$x+4, yend = now_last$y),curvature=0, alpha=0.5, arrow = arrow(length = unit(0.1, "npc")))

g









get_quantile <- function(ddf){
  temp = ddf %>% filter(x==max(ddf$x))
  return(list("Q"=quantile(temp$y), "xx"=max(ddf$x)))
}




q_now = get_quantile(dx$now)
q_past = get_quantile(dx$past)

g = ggplot(data=dx$now, aes(x=x, y=y, group=class, color=class)) + geom_line( alpha=0.5)  # + geom_point(data=input_df, aes(x=x, y=y), color="black") 
g = g + annotate("pointrange", x = q_now$xx, y = q_now$Q["50%"], ymin = q_now$Q["25%"], ymax = q_now$Q["75%"], colour = "red",alpha=0.5)
g = g + annotate("pointrange", x = q_past$xx, y = q_past$Q["50%"], ymin = q_past$Q["25%"], ymax = q_past$Q["75%"], colour = "blue",alpha=0.5)
g = g + geom_curve(aes(x = q_past$xx, y = q_past$Q["50%"], xend = q_now$xx, yend = q_now$Q["50%"]),curvature=0.2, alpha=0.5, arrow = arrow(length = unit(0.03, "npc")))
g





combined_df_now = combined_df_now %>% add_column(time="now")
combined_df_past = combined_df_past %>% add_column(time="past")
combined_df = bind_rows(combined_df_now, combined_df_past)

#ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line()  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="black") + theme_light()
g1 = ggplot(data=combined_df_past, aes(x=x, y=y, group=class)) + geom_line(color="grey")  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="red") 
g2 = ggplot(data=combined_df_now, aes(x=x, y=y, group=class)) + geom_line(color="grey")  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="red") 

library(grid)
grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g2)))
ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line() + facet_grid(time ~ .) + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="black") + theme_light()


ggplot(data=combined_df, aes(x=x, y=y, group=interaction(class, time), color=time, shape=time)) + geom_line()  + geom_point( size=0.5)

ggplot(d, aes(x=x, y=y, colour=treatment, shape = replicate,
              group=interaction(treatment, replicate))) + 
  geom_point() + geom_line()

#ggplot(data=combined_df, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line()  + geom_point( size=0.5) 


#ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line()  + geom_point( size=0.5) + ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class))

#ggplot() + geom_line(data=combined_df, aes(x=x, y=y, group=class), color="grey") 

#ggplot(data=df_main, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line() + geom_point(size=.5) 










combined_df_now_all = tibble() #for predictions using the entire dataset
combined_df_past_all = tibble() #for predictions using current dataset - past_days
counterz = 0

all_locations = c('_US')
for (search_location in all_locations){
  counterz = counterz + 1
  
  #msgBox <- tkmessageBox(title = "Title of message box",
  #                       message = search_location, icon = "info", type = "ok")
  print(paste("Working on ", search_location))
  
  #filter for a given country
  data_row = df %>% dplyr::filter(location == search_location)
  #remove all but the time series
  data_vector = as_vector(data_row %>% select(starts_with("X")))
  
  #finding the index of the first value greater than "Threshold"
  threshold_index = 0
  counter = 1
  for (i in data_vector){
    if (i >= threshold){
      threshold_index = counter 
      break
    }
    counter = counter + 1
  }
  
  total_length = length(data_vector)
  length_after_trim = total_length - threshold_index
  
  #data vector that contains the original dataset used for training
  usable_data_vector = data_vector[threshold_index:total_length]
  
  if (length(usable_data_vector) < 10) {
    print(paste("Warning: small vector for ", search_location))
    next
  }
  
  #future_df is input_df x values plus days_future
  future_day_list = 1:(length(usable_data_vector) + days_future)
  future_df = tibble(x=future_day_list)
  
  
  #past_df cotains the input_df data minus the selected days in days_past
  past_df = tibble(y=usable_data_vector[1:(length(usable_data_vector)-days_past)], x=1:(length(usable_data_vector)-days_past))
  
  
  #input_df contains the original data used for testing
  input_df = tibble(y=usable_data_vector, x=1:length(usable_data_vector)) %>% add_column(class="CSSE_data")
  #return(input_df)
  
  
  
  
  
  temp_now = carrotify(input_df, future_df, min_r2, max_r2, caret_methods) %>% add_column(location = search_location)
  combined_df_now_all = bind_rows(combined_df_now_all, temp_now)
  
  temp_past = carrotify(past_df, future_df, min_r2, max_r2) %>% add_column(location = search_location)
  combined_df_past_all = bind_rows(combined_df_past_all, temp_past)
  
}



get_quantile <- function(ddf){
  temp = ddf %>% dplyr::filter(x==max(ddf$x))
  return(list("Q"=quantile(temp$y), "xx"=max(ddf$x)))
}


s = "_US"
combined_df_now = combined_df_now_all %>% filter(location == s)
combined_df_past = combined_df_past_all %>% filter(location == s)


q_now = get_quantile(combined_df_now)
q_past = get_quantile(combined_df_past)

g = ggplot(data=combined_df_now, aes(x=x, y=y, group=class, color=class)) + geom_line( , alpha=0.5)  # + geom_point(data=input_df, aes(x=x, y=y), color="black") 
g = g + annotate("pointrange", x = q_now$xx, y = q_now$Q["50%"], ymin = q_now$Q["25%"], ymax = q_now$Q["75%"], colour = "red",alpha=0.5)
g = g + annotate("pointrange", x = q_past$xx, y = q_past$Q["50%"], ymin = q_past$Q["25%"], ymax = q_past$Q["75%"], colour = "blue",alpha=0.5)
g = g + geom_curve(aes(x = q_past$xx, y = q_past$Q["50%"], xend = q_now$xx, yend = q_now$Q["50%"]),curvature=0.2, alpha=0.5, arrow = arrow(length = unit(0.03, "npc")))
g


combined_df_now = combined_df_now %>% add_column(time="now")
combined_df_past = combined_df_past %>% add_column(time="past")
combined_df = bind_rows(combined_df_now, combined_df_past)

#ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line()  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="black") + theme_light()
g1 = ggplot(data=combined_df_past, aes(x=x, y=y, group=class)) + geom_line(color="grey")  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="red") 
g2 = ggplot(data=combined_df_now, aes(x=x, y=y, group=class)) + geom_line(color="grey")  + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="red") 

library(grid)
grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g2)))
ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line() + facet_grid(time ~ .) + geom_point( size=0.5) + geom_point(data=input_df, aes(x=x, y=y), color="black") + theme_light()


ggplot(data=combined_df, aes(x=x, y=y, group=interaction(class, time), color=time, shape=time)) + geom_line()  + geom_point( size=0.5)

ggplot(d, aes(x=x, y=y, colour=treatment, shape = replicate,
              group=interaction(treatment, replicate))) + 
  geom_point() + geom_line()

#ggplot(data=combined_df, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line()  + geom_point( size=0.5) 


#ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class)) + geom_line()  + geom_point( size=0.5) + ggplot(data=combined_df, aes(x=x, y=y, group=class, color=class))

#ggplot() + geom_line(data=combined_df, aes(x=x, y=y, group=class), color="grey") 

#ggplot(data=df_main, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line() + geom_point(size=.5) 



#---------------------------------------------------



