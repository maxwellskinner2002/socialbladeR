


top_50 <- function(platform) {
  
  #Check if input is valid
  if (platform %in% c("youtube", "twitch.tv", "twitter", "facebook", "tiktok")) {
    
    #Create the url variable based on the platform input
    if (platform == "youtube") {
      url = "https://socialblade.com/youtube/top/50/mostsubscribed"
    }
    else if (platform == "twitch.tv") {
      url = "https://socialblade.com/twitch/top/50"
    }
    else if (platform == "twitter") {
      url = "https://socialblade.com/twitter/top/50"
    }
    else if (platform == "facebook") {
      url = "https://socialblade.com/facebook/top/50/likes"
    }
    else if (platform == "tiktok") {
      url = "https://socialblade.com/tiktok/top/50/most-followers"
    }
  
    
    
    
    #set up the Selenium server
    # use google chrome drive
    
    remote_driver <- rsDriver(browser = "chrome",
                              chromever = "112.0.5615.28",
                              verbose = F, 
                              port = free_port())
    
    # Set the user agent string to emulate a human user
    user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"
    
    remDr <- remote_driver[["client"]]
    #remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Set the user agent for the browser window
    remDr$executeScript(paste0("window.navigator.userAgent='", user_agent, "';"))
    
    remDr$navigate(url)
    
    
    # Find all div elements with a specific width using the CSS selector
    
    div_test1 <- remDr$findElements(using = "css selector", value = "div[style='width: 860px; background: #fafafa; padding: 10px 20px; color:#444; font-size: 10pt; border-bottom: 1px solid #eee; line-height: 40px;']")
    div_test2 <- remDr$findElements(using = "css selector", value = "div[style='width: 860px; background: #f8f8f8;; padding: 10px 20px; color:#444; font-size: 10pt; border-bottom: 1px solid #eee; line-height: 40px;']")
    div_test3 <- remDr$findElements(using = "css selector", value = "div[style='width: 860px; background: #fafafa; padding: 0px 20px; color:#444; font-size: 10pt; border-bottom: 1px solid #eee; line-height: 30px;']")
    div_test4 <- remDr$findElements(using = "css selector", value = "div[style='width: 860px; background: #f8f8f8;; padding: 0px 20px; color:#444; font-size: 10pt; border-bottom: 1px solid #eee; line-height: 30px;']")
    
    
    #Append the lists together to get all of the rows in the desired table
    big_div <- append(div_test1, div_test2)
    small_div <- append(div_test3, div_test4)
    
    total_div <- append(big_div, small_div)
    
    
    #Split the "\n" delimiter from each string
    div_texts <- lapply(total_div, function(div) div$getElementText())
    
    div_tests_unlist <- unlist(div_texts)
    
    
    split_string <- function(x) {
      unlist(strsplit(x, "\n"))
    }
    
    split_strings <- lapply(div_tests_unlist, function(x) strsplit(x, "\n")[[1]])
    
    
    #Add all strings into a single data frame
    df <- do.call(rbind, split_strings)
    
    #if there is a column with more then 10 spaces than to remove the column. (Special case used for twitter)
    matches <- colSums(sapply(df, grepl, pattern = "--"))
    
    threshold <- 10
    df <- df[, matches < threshold]
    
    #Scrape the headings of the table from Social Blade to be the headings of the created dataframe
    title_div <- remDr$findElement(using = "css selector", value = "div[style='width: 860px; background: #263238; padding: 20px; color:#fff;']")
    table_headings <- title_div$getElementText()
    column_name_text <- table_headings[[1]]
    column_names <- unlist(strsplit(column_name_text, "\n"))
    colnames(df) <- c(column_names)
    
    #Cleaning up the data frame
    # Removing any rows that have an empty values or "--" in them. This removes non-channels from platforms like Youtube Gaming or Youtube Music
    df <- df[!apply(df == "--", 1, any),]
    
    
    #Sort the dataframe in ascending order by rank
    order(df)
    
    #Remove rank and grade column
    final_df <- subset(df, select = -c(Rank, Grade))
    
    #close the server
    remote_driver$server$stop()
    
    return(final_df)
  }
  
  else {
    stop("Invalid platform specified. Please enter one of: youtube, twitch.tv, twitter, instagram, facebook, tiktok")
  }
  
}


#top_50("youtube") WORKS 

#top_50("twitch.tv") WORKS

#top_50("twitter") #error at column names (there are two empty columns with blanks)

#top_50("instagram") displays table but with no numbers (only available for premium members)

#top_50("facebook") WORKS

#top_50("tiktok") error at subset (there is an empty column on site)
