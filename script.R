
library(tidyverse)
library(e1071)
library(nnet)
library(nmp)
library(caret)
library(KRLS)
library(randomForest)
library(elasticnet)
library(monomvn)
library(relaxo)
library(frbs)
library(rqPen)
library(penalized)

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



#caret_methods = c( "krlsPoly", "nnet", "avNNet", "brnn", "rvmPoly", "qrf","rf", "enet", "bridge", "lars", "leapSep", "pcr", "relaxo", "ridge", "ANFIS", "cubist", "DENFIS", "FIR.DM","GFS.THRIFT", "GFS.LT.RS", "HYFIS", "icr", "leapBackward", "leapForward", "rqnc", "penalized", "qrnn", "rqlasso")

caret_methods = c( "krlsPoly", "rf", "enet", "bridge", "lars", "leapSep", "relaxo", "ridge", "ANFIS", "DENFIS", "FIR.DM","rqnc", "penalized", "rqlasso")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------


carrotify <- function(input_df, future_df, min_r2, max_r2, caret_methods) {
  # Runs all given models in caret_methods on input_df
  
  #--------------------------------
  #input_df = learning
  #future_df = to predict
  #min/max r2 = r2 filter thresholds
  
  #retuns a df of x, (predicted)y, algo
  
  #--------------------------------
  
  #using the caret package to automate ml functions
  
  tc = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
  
  #estimating goodness of fit
  
  
  #---------------------------------------------------
  
 
  #nnet
  
  combined_df = c() #this will contain all the predicted values of all the algorithms
  caret_model_list = c()
  r2_list = c()
  for (method in caret_methods){
    print(method)
    #set repeated cross validation
    tc = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
    
    #tc = trainControl(method = "repeatedcv", number = 3, repeats = 5)
    #list of available methods https://topepo.github.io/caret/available-models.html
    
    tryCatch({caret_model = caret::train(y ~ x, data=input_df,method=method,trainControl=tc , preProcess = c("scale", "center"))}, error = function(e){skip_to_next <<- TRUE})
    
    if (skip_to_next) {
      
      r2_list = c(r2_list, 0)
      
      next}
    
    
    #try({caret_model = caret::train(y ~ x, data=input_df,method=method,trainControl=tc , preProcess = c("scale", "center"))})
    temp <- future_df
    predicted_y = predict(caret_model, future_df)
    r2 = postResample(pred=predicted_y, obs = future_df$x)['Rsquared']
    r2_list = c(r2_list, r2)
    print(paste(method, r2, sep="__"))
    
    temp <- add_column(temp, y=predicted_y) %>% add_column(class=method)
    #caret_model_list = c(caret_model_list, caret_model)
    
    #combine with combined_df
    combined_df = bind_rows(combined_df, temp)
    
  }
  
  names(r2_list) = caret_methods
  
  #remove NA
  r2_list =r2_list[!is.na(r2_list)]
  
  #filtering for r2 greater than 0.8
  r2_list = r2_list[r2_list > min_r2]
  r2_list = r2_list[r2_list < max_r2]
  
  #filter df with the filtered r2_list 
  combined_df = combined_df %>% filter(class %in% names(r2_list))
  
  return(combined_df)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------


#load data csv
df = read.csv(data_file)
library(tcltk)
#combine province and country
df = df %>% unite("location", Province.State, Country.Region, sep="_", remove = F)

all_locations = df$location %>% unique


combined_df_now_all = tibble()
combined_df_past_all = tibble()

counterz = 0
for (search_location in all_locations){
  counterz = counterz + 1
  
  #msgBox <- tkmessageBox(title = "Title of message box",
  #                       message = search_location, icon = "info", type = "ok")
  print(paste("Working on ", search_location))

  #filter for a given country
  data_row = df %>% filter(location == search_location)
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

  



  temp_now = carrotify(input_df, future_df, min_r2, max_r2) %>% add_column(location = search_location)
  combined_df_now_all = bind_rows(combined_df_now_all, temp_now)

  temp_past = carrotify(past_df, future_df, min_r2, max_r2) %>% add_column(location = search_location)
  combined_df_past_all = bind_rows(combined_df_past_all, temp_past)

}



get_quantile <- function(ddf){
  temp = ddf %>% filter(x==max(ddf$x))
  return(list("Q"=quantile(temp$y), "xx"=max(ddf$x)))
}


s = "Fujian_China"
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



