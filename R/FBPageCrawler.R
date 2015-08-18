
for (package in c('devtools', 'httr', 'rjson', 'httpuv')) {
  if (!require(package, character.only=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

if(!require("Rfacebook")){
  install_github("WUNSG/Rfacebook/Rfacebook")
  require(Rfacebook)
}

FBPageCrawler() <- function(){
  # STEP 1: Get fackebook token to access data. I need a crossplatform version of winDialog and winDialogString otherwise this only works on Windows
  winDialog(type = "ok", "Make sure you have already signed into Facebook.\n\nWhen  browser opens, please click 'Get Access Token' twice. You DO NOT need to select/check any boxes for a public feed.\n\n After pressing OK, swich over to your now open browser.")
  browseURL("http://developers.facebook.com/tools/explorer/?method=GET&path=100002667499585")
  token <- winDialogString("When browser opens, please click 'Get Access Token' twice and copy/paste token below", "")
  
  # STEP 2: Get facebook ID. This can be a fanpage or whatever e.g. https://www.facebook.com/adidasSG
  tID <- winDialogString("Please enter FB name id below:", "https://www.facebook.com/adidasSG")
  ID <- gsub(".*com/", "", tID)
  
  # STEP 3: How far back do you want get data for? Format should be YYYY-MM-DD
  since <- winDialogString("Please enter a START (older) date for how roughly far back to gather data from using this format: yyyy-mm-dd", "2014-01-01")
  until <- winDialogString("Please enter a END (nearer) date using this format: yyyy-mm-dd", "2015-01-01")
  
  max.post <-as.numeric(winDialogString("Enter maximum posts to crawl within the period", "100"))
  
  #Extract information about a public Facebook post
  page <- getPage(page=ID, token=token, n=max.post, 
                  since=since, until=until)
  
  page[, 4] <- as.POSIXct(page[, 4], format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
  attr(page[, 4], "tzone") <- "Singapore"
  
  library(stringr)
  
  fileName <- str_c(c(ID, "-page-", since, "to", until, ".csv"), collapse = "")
  write.csv(page, fileName)
  winDialog(type = "ok", paste(nrow(page), "page posts saved in\n\n ", getwd(), "/", fileName))
  
  if (winDialog(type = "yesno", "Download comments?") == "YES"){
    #extract all comments from extracted page posts
    post.df <- data.frame()
    for (i in 1:length(page$id)){
      if(page$comments_count[i]> 0){
        print(paste("Crawling comments from post", i, "of", length(page$id), "posts..."))    
        post <- getPost(post=page$id[i], n=2000, token=token)
        post.df <- rbind(post.df, post$comments)
      }
    }
    
    #convert post timings to Singapore time (GMT+8)
    page[, 4] <- as.POSIXct(page[, 4], format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    attr(page[, 4], "tzone") <- "Singapore"
    
    fileName <- str_c(c(ID, "-comments-", since, "to", until, ".csv"), collapse = "")
    write.csv(post.df, fileName)  
    
    winDialog(type = "ok", paste("Post comments saved in\n\n ", getwd(), "/", fileName))  
  }
  
  winDialog(type = "ok", "Facebook crawl job completed.")    
}
