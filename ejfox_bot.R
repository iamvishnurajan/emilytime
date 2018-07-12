#
#  VANITY FAIR SCRAPER
#  THIS WILL FETCH AND POPULATE A DB TABLE FOR
#  ARTICLES BY THE SPECIFIED AUTHORS AND TWEET AS NEW
#  ARTICLES ARE DETECTED
#  

#  START TIME AND LOAD LIBRARIES

library(rvest)
library(rtweet)
library(lubridate)
library(plyr)
library(dplyr)

Sys.setenv(TZ='America/New_York')

#  USER CONFIGURABLE INPUT PARAMETERS

# Edit these  lines for the respective directory locations
# It is suggested to make backups as subdirectories of the main_dir
# i.e., bkup_dir = /main_dir/backups/
# Note - you must include the trailing slash and you must leave the quotation marks
main_dir <- "INSERT A DIRECTORY NAME HERE"
bkup_dir <- "INSERT A BACKUP DIRECTORY NAME HERE"

# For the file that will be saved and used for comparison to check for new articles
# what should the name be. For example, if you wanted to create a bot for Ronan Farrow,
# we could use just "rfarrow" here. The code will take care of adding
# any necessary extensions (i.e., "rfarrow.csv")
filename_header <- "ejfox"

# List the author URL tag. For VF, the full URL is 
# "https://www.vanityfair.com/contributor/emily-jane-fox". We just need to list the 
# "emily-jane-fox" portion here. The code will take care of the rest.
authorstring = "emily-jane-fox"

# What do you want the bot to say when it tweets.
whattimeisit = "#emilytime"

# Replace "INSERT YOUR TWITTER TOKEN FILENAME" with your twitter token file name
# As noted below, it is suggested to place this file in the main_dir
# See http://rtweet.info/articles/auth.html on how to create this twitter token
# You must leave the quotation marks and list your filename within those
twitter_token <- readRDS(paste0(main_dir,"INSERT YOUR TWITTER TOKEN FILENAME"))

# How frequently the loop should check for new articles
# The number here is how many seconds the loop will "sleep"
# in between checks
sleep_loop = 15

###  NOTHING BELOW THIS SHOULD NEED TO GET EDITED ###

#  INITIALIZATIONS

sink_msgs <- file(paste0(main_dir,"std_msgs.txt"), open="at")
sink(sink_msgs,type=c("message"),append = TRUE)
sink(sink_msgs,type=c("output"),append = TRUE)

tweet_max <- 275

#  INFINITE LOOP; WE WRITE THIS TO RUN EFFECTIVELY FOREVER
#  IF LOOP BROKEN HOWEVER DUE TO SYSTEM RESTART, ETC.
#  IT CAN PICKUP FROM WRITTEN .CSV FILE

while (Sys.Date() < "2200-01-01") {
  vf_file <- paste0(main_dir,filename_header,".csv")
  vf_file_bkup <- paste0(bkup_dir,filename_header,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
  vf_file_csv <- read.csv(vf_file, header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE)
  
  #  .CSV AND OTHER INITIALIZATIONS AND LINK SCRAPE LOOP
  
  vf_file_csv$pub_date <- ymd_hms(vf_file_csv$pub_date)
  vf_df0 <- vf_file_csv
  
  vf_diff <- anti_join(vf_df0,vf_df0,by=c("fulllink"))
  loop_count = 0
  
  while (nrow(vf_diff)==0){
    vfstarttime <- proc.time()
    
    vfpage=read_html(paste0("https://www.vanityfair.com/contributor/",authorstring))
    vfmain <- vfpage %>% html_nodes(".river-list") %>% html_nodes(".component-river-item") %>% html_nodes(".details")

    vf_url_base <- sapply(vfmain,function(x) x  %>% html_nodes(".hed") %>% html_nodes("a") %>% html_attr("href"))
    vf_df <- data.frame("url_base" = vf_url_base,stringsAsFactors = FALSE)
    
    vf_df <- vf_df %>% mutate(
      fulllink = paste0("https://www.vanityfair.com",vf_df$url_base),
      headline = trimws(gsub("[\r\n]","",vfmain %>% html_nodes(".hed") %>% html_text)),
      description = sapply(vfmain, function(x) trimws(gsub("[\r\n]","",x %>% html_nodes(".promo-dek") %>% html_text))),
      byline = trimws(gsub("[\r\n]","",vfmain %>% html_nodes(".component-byline") %>% html_nodes(".contributors__list") %>% html_text)),
      pub_date = mdy_hm(trimws(gsub("[\r\n]","",vfmain %>% html_nodes(".publish-date") %>% html_text)),tz="America/New_York"),
      tweet_text = paste0("It's ",whattimeisit,": ",headline,"\n",fulllink),
      num_char = nchar(tweet_text))
    
    vf_df$description <- sapply(vf_df$description,function(x) ifelse(length(x) > 0,x,""))
    
    vf_df$tweet_text <- 
      ifelse(vf_df$num_char > tweet_max,
             paste0("It's ",whattimeisit,": ",strtrim(vf_df$headline,nchar(vf_df$headline)-(vf_df$num_char-tweet_max-3)),"...\n",vf_df$fulllink),
             vf_df$tweet_text)
    
    #  COMPARISONS AND DIFF
    
    vf_df0 <- vf_df0[order(vf_df0$pub_date),]
    vf_df <- vf_df[order(vf_df$pub_date),]
    
    vf_diff <- anti_join(vf_df,vf_df0,by = c("fulllink"))
    
    if (nrow(vf_diff)>0) {
      for(vfidx in 1:nrow(vf_diff)) {
        post_tweet(status = vf_diff$tweet_text[vfidx],token = twitter_token)
      }
    }
    
    if(nrow(vf_diff)>0){
      vf_df <- bind_rows(vf_df0,vf_diff)
      
      vf_df0 <- vf_df0[order(vf_df0$pub_date,decreasing = TRUE),]
      write.csv(vf_df0,file = vf_file_bkup,row.names=FALSE)
      
      vf_df <- vf_df[order(vf_df$pub_date,decreasing = TRUE),]
      write.csv(vf_df,file = vf_file,row.names=FALSE)
    }
    
    # FINAL TIME TO RUN CALCULATION
    
    if (nrow(vf_diff)>0 | loop_count==((5*60)/sleep_loop)){
      vfendtime <- proc.time() - vfstarttime
      vfendsecs <- vfendtime[3]
      print(vfendsecs)
      print(Sys.time())
      cat("\n\n")
      if (loop_count==((5*60)/sleep_loop)){
        loop_count = 0
      }
    }
    
    loop_count = loop_count+1
    Sys.sleep(sleep_loop)
  }
}

sink(type="message")
sink(type="output")
close(sink_msgs)