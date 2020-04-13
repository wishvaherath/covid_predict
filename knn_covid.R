library(cowplot)
library(tidyverse)
library(tsfknn)
options(scipen=10000000)
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
min_r2 = 0.8
max_r2 = 0.95


get_data_vector <- function(main_df, search_location, threshold){
  #load data csv
  df = main_df

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
    usable_data_vector= NULL
  }
  
  return (usable_data_vector)
  
}


get_query_df <- function(usable_data_vector, days_future, days_past){
  
  

  
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


predict_covid <- function(dx, days_future, days_past, search_location){
  
  #dx = get_query_df(df, search_location) #search query here
  
  
  #--------------------------------------------------------------------------------------------------------------------------------
  # try NLS
  
  nls_model <- nls(y ~ (x ^ b) + c, dx$now,  start=c(b=2, c=0), trace=T)
  df_nls <- dx$future
  df_nls <- add_column(df_nls, y=(predict(nls_model, dx$future))) %>% add_column(class="predicted_nonlinear_ls")
  
  
  nls_model_past <- nls(y ~ (x ^ b) + c, dx$past,  start=c(b=2, c=0), trace=T)
  df_nls_past <- dx$future
  df_nls_past <- add_column(df_nls_past, y=(predict(nls_model_past, dx$future))) %>% add_column(class="predicted_nonlinear_ls_PAST")
  
  
  all_df <- df_nls %>% bind_rows(dx$now) %>% bind_rows(df_nls_past)
  
  #calcualte R2
  alpha = head(df_nls, n=nrow(df_nls)-days_future)$y
  beta = dx$now$y # this is the original df
  
  nls_r2 = cor(alpha, beta)^2
  
  
    
    #fit is good
    
    #Plotting for nls 
    
    nls_now_last = tail(df_nls, n=1)
    nls_past_last = tail(df_nls_past, n=1)
    
    nls_gap = nls_past_last$y - nls_now_last$y
   
    nls_string = "" 
    if (nls_gap > 0) {
      #past_last is high, things have improved
      nls_string = paste("The forecast has improved by  ", floor(abs(nls_gap)), " cases over the past ", days_past, " days.")
      
    } else {
      
      nls_string = paste("The forecast has worsened by ", floor(abs(nls_gap)), " cases over the past ", days_past, " days.")
    }
    
    
    nls_now_final = tail(df_nls, n=days_future)
    nls_past_final = tail(df_nls_past, n=days_future+days_past)
    
    
    
    g = ggplot() + geom_point(data=dx$now, aes(x=x, y=y), color="black", alpha=0.5) 
    g = g + geom_line(data=nls_now_final, aes(x=x, y=y), color="red", alpha=0.5)  + geom_point(data=nls_now_final, aes(x=x, y=y), color="red", alpha=0.5) 
    g = g + geom_line(data=nls_past_final, aes(x=x, y=y), color="blue", alpha=0.5)  + geom_point(data=nls_past_final, aes(x=x, y=y), color="blue", alpha=0.5) 
    #g = g + geom_curve(aes(x =nls_past_last$x, y = nls_past_last$y, xend = nls_now_last$x, yend = nls_now_last$y),curvature=-0.2, alpha=0.5, arrow = arrow(length = unit(0.05, "npc")))
    g = g + geom_curve(aes(x = 0, y = nls_past_last$y, xend = 0, yend = nls_now_last$y),curvature=0, alpha=0.8,color="black", arrow = arrow(length = unit(0.05, "npc")))
    g = g + geom_hline(yintercept=nls_now_last$y, linetype="dashed", color = "red", alpha=0.5)
    g = g + geom_hline(yintercept=nls_past_last$y, linetype="dashed", color = "blue", alpha=0.5)
    g = g + annotate("text", x = (nls_now_last$x)/4, y = nls_now_last$y + 10 , label = paste("Forcast for next ", days_future, " days using current data.", sep=""), color="red")
    g = g + annotate("text", x = (nls_past_last$x)/4, y = nls_past_last$y + 10 , label = paste("Forcast for next ", days_future, " days using ", days_past, " days old data.", sep=""), color="blue")
    g = g + annotate("text",  x = (nls_now_last$x)/2, y = 0, label=paste("Expected cases in 5 days\n", floor(nls_now_last$y), sep=""), size=5, color="red")
    
    #g = g + xlab("Days since 100 cases") + ylab("Number of Cases") + ggtitle(paste("COVID-19 case forecast of ", search_location, "for the next ", days_future, " days\n", nls_string), subtitle = paste("Black: Real data \nBlue: Forecast using upto-date data\nRed: Forecast without data from last ", days_past, " days\nmethod: NLS ; R2: ", nls_r2))
    g = g + xlab("Days since 100 cases") + ylab("Number of Cases") + ggtitle(paste("COVID-19 case forecast of ", search_location, " for the next ", days_future, " days.",sep=""), subtitle = nls_string)
    
    g = g + theme(plot.title = element_text(size = 20, face = "bold"), plot.subtitle=element_text( face="italic", color="red"))
   
    nls_g = g 
    
  
    #doing forecasting by knn
    
      
    max_t = dx$now$x %>% max #last timepoint of the original data
    skip_to_next=F
    tryCatch({ 
    
    pred_now <- knn_forecasting(ts(dx$now$y), h = days_future, lags = 1:7, msas = "MIMO")
    #autoplot(pred_now, highlight = "neighbors", faceting = FALSE)
    #create tibble with predicted data
    pred_now_df = tibble(x= (max_t+1):(max_t+days_future), y=pred_now$prediction, class="current prediction")
    
    
    pred_past <- knn_forecasting(ts(dx$past$y), h = days_past + days_future, lags = 1:1, msas = "MIMO")
    #autoplot(pred_past, highlight = "neighbors", faceting = FALSE)
    pred_past_df = tibble(x= (max_t+1-days_past):(max_t+days_future), y=pred_past$prediction, class="past prediction")
   
    }, error = function(e){skip_to_next <<- TRUE})
    
    if (skip_to_next){
      
      return (list(nls_plot=nls_g, nls_gap=nls_gap, nls_r2=nls_r2, knn_plot="ERROR", knn_gap="ERROR"))
      
    }
    #pred_now$prediction
    #ro <- rolling_origin(pred_now, h = days_future)
    #ro$global_accu
    
    now_last = pred_now_df %>% filter(x == max_t + days_future) 
    past_last = pred_past_df %>% filter(x == max_t + days_future)
    knn_gap = past_last$y - now_last$y
    
     
    knn_string = "" 
    if (knn_gap > 0) {
      #past_last is high, things have improved
      knn_string = paste("The forecast has improved by  ", floor(abs(knn_gap)), " cases over the past ", days_past, " days.")
      
    } else {
      
      knn_string = paste("The forecast has worsened by ", floor(abs(knn_gap)), " cases over the past ", days_past, " days.")
    }
   
    
    g = ggplot() + geom_point(data=dx$now, aes(x=x, y=y), color="black", alpha=0.5) 
    g = g + geom_line(data=pred_past_df, aes(x=x, y=y), color="red", alpha=0.3)  + geom_point(data=pred_past_df, aes(x=x, y=y), color="red", alpha=0.5) 
    g = g + geom_line(data=pred_now_df, aes(x=x, y=y), color="blue", alpha=0.3)  + geom_point(data=pred_now_df, aes(x=x, y=y), color="blue", alpha=0.5) 
    #g = g + geom_curve(aes(x =past_last$x, y = past_last$y, xend = now_last$x, yend = now_last$y),curvature=0, alpha=0.8,color="black", arrow = arrow(length = unit(0.05, "npc")))
    g = g + geom_curve(aes(x = 0, y = past_last$y, xend = 0, yend = now_last$y),curvature=0, alpha=0.8,color="black", arrow = arrow(length = unit(0.05, "npc")))
    g = g + geom_hline(yintercept=now_last$y, linetype="dashed", color = "blue", alpha=0.5)
    g = g + geom_hline(yintercept=past_last$y, linetype="dashed", color = "red", alpha=0.5)
    
    g = g + annotate("text", x = (past_last$x)/4, y = past_last$y + 10 , label = paste("Forcast for next ", days_future, " days using ", days_past, " days old data.", sep=""))
    
    g = g + annotate("text", x = (now_last$x)/4, y = now_last$y + 10 , label = paste("Forcast for next ", days_future, " days using current data.", sep=""))
    g = g + annotate("text",  x = (now_last$x)/2, y = 0, label=paste("Expected cases in 5 days\n", floor(now_last$y), sep=""), size=5)
    
    #g = g + annotate("text", x = 0, y = past_last$y , label = "Forcast from lagging data")
    
    
    #g = g + theme(plot.subtitle=element_text(size=10, face="italic", color="grey"))
    g = g + theme(plot.title = element_text(size = 40, face = "bold"), plot.subtitle=element_text( face="italic", color="red"))
    
    #g = g + theme(plot.subtitle=element_text( face="italic", color="red"))
    
    #g = g + xlab("Days since 100 cases") + ylab("Number of Cases") + ggtitle(paste("COVID-19 case forecast of ", search_location, "for the next ", days_future, " days\n", knn_string), subtitle = paste("Black: Real data \nBlue: Forecast using upto-date data\nRed: Forecast without data from last ", days_past, " days\nMethod: KNN"))
    g = g + xlab("Days since 100 cases") + ylab("Number of Cases") + ggtitle(paste("COVID-19 case forecast of ", search_location, " for the next ", days_future, " days.", sep=""), subtitle = knn_string)
    #g = g +  coord_fixed(ratio=10)
    knn_g = g
                                                                             
    
    
    return (list(nls_plot=nls_g, nls_gap=nls_gap, nls_r2=nls_r2, knn_plot=knn_g, knn_gap=knn_gap))
   
  

}


#actual code commented so that the app can source it. 

if (F) {
  
  

df = read.csv(data_file)
#load data csv
#combine province and country
df = df %>% unite("location", Province.State, Country.Region, sep="_", remove = F)
all_locations = df$location %>% unique



usable_data_vector = get_data_vector(df, search_location, threshold)

if (is.null(usable_data_vector)){
  print("Less than sufficent datapoints")
}

dx = get_query_df(usable_data_vector, days_future, days_past)


result = predict_covid(dx, days_future, days_past, search_location)


#result = predict_covid("Queensland_Australia")
#result = predict_covid("_Iran")
plot_grid(result$nls_plot, result$knn_plot,labels = c("nls", "knn"), ncol = 1)

}
