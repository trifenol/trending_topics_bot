# script created by Janderson Toth
# www.data7.blog

# Installing the packages
if(!require("install.load")) {
  install.packages("install.load")
  library(install.load)
}
install_load("dplyr","rtweet","RSQLite", "tidyr","curl","purrr","telegram", 
             "telegram.bot","stringr", "plyr", "lubridate","testit","rlist")

# setting the directory
#setwd("/home/trifenol/trending_topics/")
setwd("/home/ec2-user/trending_topics/")

# loading the keys
# token <- create_token(
#   app = "BUSCADOR DAPP SPAM",
#   consumer_key = "TOKEN HERE",
#   consumer_secret = "TOKEN HERE",
#   access_token = "TOKEN HERE",
#   access_secret = "TOKEN HERE")

#loading the keys if they are already saved
token <- readRDS("keys.rds")

# loading the database
db_trends <- dbConnect(SQLite(), dbname="dados_trendingtopics_bot.sqlite")

# loading the bot token
bot = Bot(token = "TOKEN HERE")

# Searching the top 50 hashtags from each country
list_of_countries <- trends_available() %>% 
  filter(place_type == "Country" | place_type == "Supername" ) %>%
  select(name, countryCode, woeid)

list_of_countries[list_of_countries$name == "Worldwide",2] <- "GLOBAL"

base_country <- list()
for (k in list_of_countries$woeid) {
  busca <- get_trends(woeid = k, token = token)
  busca <- busca %>% select(trend, promoted_content, tweet_volume,as_of,url) %>% as.data.frame()
  base_country <- list.append(base_country, busca)}

names(base_country) <- list_of_countries$countryCode
save(list_of_countries, file = "list_of_countries.RData")


# loading users already registered 
base_transition <- dbReadTable(db_trends, "base_bot")

# Filtering any users not registered 
base_transition <- base_transition %>% 
  group_by(User_ID) %>% 
  filter(Message_ID == max(Message_ID) & Text_type == "START") %>%
  top_n(n=1, wt = Message_ID) 

# loading and filtering the country of each user
base_transition_country <- dbReadTable(db_trends, "base_country")
base_transition_country <- base_transition_country %>% 
  group_by(User_ID) %>% 
  filter(Message_ID == last(Message_ID) & Text_type == "COUNTRY" & User_ID %in% base_transition$User_ID) %>%
  top_n(n=1, wt = Message_ID)
  
  
# Saving the TT of Brazil in a table
brazil <- base_country$BR
dbWriteTable(conn = db_trends, name = "Brazil", value = brazil, row.names = FALSE, append = T)


# Sending the TT for each user
erro <- c()
ct_erro <- 0
# Sending the messages
  for (chat_id in base_transition$User_ID) {
    print(chat_id)
    user_country <-   toupper(gsub("/","",base_transition_country[base_transition_country$User_ID == chat_id,5]))
    country_to_send <- base_country[[user_country]]
    
    # creating the message that will be sent to each user
    country_to_send$tweet_volume <-  replace_na(country_to_send$tweet_volume, "NA")
    country_to_send$message <- paste0(" \n [", country_to_send$trend,"](",country_to_send$url,") - Tweets: ", country_to_send$tweet_volume)
    
    text <- iconv(paste("These are the main themes of the last hour in", list_of_countries[list_of_countries$countryCode == user_country,1]),"UTF-8","latin1")
   # I don't know just yet how to print the time of each timezone for each user, therefor I will not set the time in the messages for now.
    # text <- iconv(paste("These are the main issues in", list_of_countries[list_of_countries$countryCode == user_country,1], "at",
    #                     format((Sys.time() - 60* 60* 3), "%X %a %d %b %Y")),"UTF-8","latin1")

        t <- has_error(bot$sendMessage(chat_id = chat_id, text = text))
    if(t == T){ 
      erro <- c(erro, chat_id)
      ct_erro <- ct_erro + 1 
      print("A error here!")
    } else {
       bot$sendMessage(chat_id = chat_id, text = toString(country_to_send$message), parse_mode='Markdown') }}


# loading registered users, removing those with errors and overwriting the table
base_total <- dbReadTable(db_trends, "base_bot")
base_total <- base_total %>% filter(!User_ID %in% erro)
dbWriteTable(conn = db_trends, name = "base_bot" ,value = base_total, 
             row.names = FALSE, append = F, overwrite = T)

# loading user's country, removing those with errors and overwriting the table
base_transition_country <- base_transition_country %>% filter(!User_ID %in% erro)

dbWriteTable(conn = db_trends, name = "base_country" ,value = base_transition_country, 
             row.names = FALSE, append = F, overwrite = T)

# disconnecting the database
dbDisconnect(db_trends)

# sending to adm the summary of the process
bot2 = Bot(token = "TOKEN HERE")
bot2$sendMessage(chat_id = "YOUR TELEGRAM ID HERE", text = iconv(paste0("Rodou com ", ct_erro,
                                                      " erros de envio e tem ",
                                                      nrow(base_transition),
                                                      " usuários na base, em ",
                                                      length(unique(base_transition_country$Text))
                                                      ," países"),"UTF-8","latin1"))


# the easy way to check all messages sent to the bot
# msgs <- bot$getUpdates()
# verified <- function(msgs){
#   interm <- as.list(msgs)
#   print(paste(interm$message$from$first_name, "||",interm$message$text, as.character(interm$message$chat$id)))
# }
# invisible(lapply(msgs,verified ))
