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

# creating a database in sqlite
db_trends <- dbConnect(SQLite(), dbname="dados_trendingtopics_bot.sqlite")

# loading the bot token
bot = Bot(token = "TOKEN HERE")
msgs <- bot$getUpdates() #loading the messages sent to the bot

# loading the file with the list of countries avaliable
load("list_of_countries.RData")

# handling bot messages
processing_start_stop <- function(msgs){
  interm <- as.list(msgs)
  if(is.null(interm$message$text)){
    data <- data.frame(NULL)
    return(data)
  } else {
    data <- data.frame(User_ID = as.character(interm$message$chat$id),
                       Message_ID = as.character(interm$message$message_id),
                       Date = as.POSIXct(interm$message$date,origin = "1970-01-01"),
                       Name = interm$message$from$first_name,
                       Text = interm$message$text,
                       Text_type = NA)
    
    code <- paste0("/",list_of_countries$countryCode,"$")
    
    for(hh in code){
      if(str_detect(toupper(data$Text), hh) == TRUE ){
        data$Text_type <- "COUNTRY"}}
    
    if(str_detect(toupper(data$Text), "/START") == TRUE ){
      data$Text_type <- "START"
    } else if(str_detect(toupper(data$Text), "/STOP") == TRUE){
      data$Text_type <- "STOP" 
    } else if(str_detect(toupper(data$Text), "/HELP") == TRUE){
      data$Text_type <- "HELP" 
    } else if(str_detect(toupper(data$Text),"/COUNTRY") == TRUE){
    data$Text_type <- "HELP_COUNTRY" }
    
    data <- data %>% filter(!is.na(Text_type))
    if(nrow(data) == 0){ data <- data.frame(NULL)}  
    return(data)}}

base <- ldply(lapply(msgs, processing_start_stop), rbind) #%>% arrange(desc(Date))
 
# creating a table with the country data
base_country <- base %>% filter(Text_type == "COUNTRY")

########################################


if(nrow(base) != 0 | nrow(base_country) != 0){

base_help <- base %>% filter(Text_type == "HELP" | Text_type == "HELP_COUNTRY")

if(dbExistsTable(db_trends, "base_help_old") == TRUE){
  base_help_old <- dbSendQuery(db_trends, "Select Message_ID from base_help_old")
  base_help_old <- dbFetch(base_help_old)
  b_h_old <- intersect(base_help$Message_ID,base_help_old$Message_ID) 
  if(!is_empty(b_h_old)){
  base_help <- base_help %>% filter(!Message_ID %in% b_h_old)
  dbWriteTable(conn = db_trends, name = "base_help_old" ,value = base_help, 
               row.names = FALSE, append = T)}
} else {
    dbWriteTable(conn = db_trends, name = "base_help_old" ,value = base_help, 
               row.names = FALSE, append = T)}


base <- base %>% filter(Text_type == "START"  | Text_type == "STOP")

# checking if the user is already registered and sending a welcome message
if(dbExistsTable(db_trends, "base_bot") == TRUE){
  message_old <- dbSendQuery(db_trends, "Select User_ID, Message_ID from base_bot")
  message_old <- dbFetch(message_old)
  forced_country <- unique(message_old$User_ID)
  message_old <- message_old$Message_ID
  base <- base %>% filter(!Message_ID %in% message_old)
  nrow(base)} else {
    forced_country <- NULL}

# checking if the user has already chosen the country 
if(dbExistsTable(db_trends, "base_country") == TRUE){
  message_old2 <- dbSendQuery(db_trends, "Select Message_ID from base_country")
  message_old2 <- dbFetch(message_old2)
  message_old2 <- message_old2$Message_ID
  base_country <- base_country %>% filter(!Message_ID %in% message_old2)
  nrow(base_country)}

# Adding the country if the user has not chosen yet
if(nrow(base[base$Text_type == "START",]) != 0){
forced_user <- base %>% filter(Text_type == "START")
forced_country_base <- setdiff(forced_user,forced_country)
forced_country_base$Text <- "/GLOBAL"
forced_country_base$Text_type <- "COUNTRY"
base_country <- rbind(forced_country_base, base_country)}

# sending the welcome message
ct_erro_new <- 0
if(nrow(base) != 0){
  presentation <- base %>% filter(Text_type == "START") %>% 
    distinct(User_ID, Text_type,.keep_all = T)
  erro_new <- c()
  
  for(chat_id_new in presentation$User_ID) {
    country_to_send <- list_of_countries[,c(2,1)]
    country_to_send$message <- paste0("\n /",country_to_send$countryCode," - ", country_to_send$name)
    text_presentation <- iconv(paste0("Hello ",presentation[presentation$User_ID == chat_id_new,4],", I'm * Trendy * and I'm going to bring you the list of the main Twitter hashtags every hour. I'm still on beta, so if you have any suggestions just send them to my creator, @trifenol. To cut off relations with me just type _ / stop_ and you will not receive any more information. All hashtags are clickable and take you directly to Twitter. NA = Data not available ")," UTF-8 "," latin1 ")
    text_presentation_country <- iconv(paste0("Choose which country from the following list that you would like to receive trending topics. \n",
                                              toString(country_to_send$message),
                                              "\n \n In the next hour you will receive my messages. If you didn't choose any option then I will send you the GLOBAL."),"UTF-8","latin1")
   
    h <- has_error(bot$sendMessage(chat_id = chat_id_new, text = text_presentation, parse_mode = "Markdown"))
    if(h == T){ 
      erro_new <- c(erro_new, chat_id_new)
      ct_erro_new <- ct_erro_new + 1 
    } else {
      bot$sendAnimation(chat_id = chat_id_new, animation = "3ce93dd4-994c-48fe-8bf9-3459728bcfad.webp")
      bot$sendMessage(chat_id = chat_id_new, text = text_presentation_country, parse_mode = "Markdown")
      print(presentation[presentation$User_ID == chat_id_new,4])
    }}
  
# filter the users with errors of send
base <- base %>% filter(!User_ID %in% erro_new) 
base_country <- base_country %>% filter(!User_ID %in% erro_new) 

#saving the table in the database
dbWriteTable(conn = db_trends, name = "base_bot" ,value = base, 
               row.names = FALSE, append = T)}
dbWriteTable(conn = db_trends, name = "base_country" ,value = base_country, 
             row.names = FALSE, append = T)



###############################################

# helping users if they are in problem
base_help_country <- base_help %>% filter(Text_type == "HELP_COUNTRY")
base_help <- base_help %>% filter(Text_type == "HELP") 


if(nrow(base_help_country) != 0 | nrow(base_help) != 0){

ct_erro_new_help <- 0
if(nrow(base_help) != 0){
  base_help <- base_help %>% distinct(User_ID, .keep_all = T)
  erro_new_help <- c()
  
  for(need_help in base_help$User_ID) {
    
    text_help <- iconv(paste0("If you no longer want to receive my notifications just type /stop, if you want to change the country just type /country. But if you want something more complex then you can send a message to my creator, @trifenol"),"UTF-8","latin1")
    h <- has_error(bot$sendMessage(chat_id = need_help, text = text_help, parse_mode = "Markdown"))
    if(h == T){ 
      erro_new_help <- c(erro_new_help, need_help)
      ct_erro_new_help <- ct_erro_new_help + 1 }}
  }


ct_erro_new_help_country <- 0
if(nrow(base_help_country) != 0){
  base_help_country <- base_help_country %>% distinct(User_ID, .keep_all = T)
  erro_new_help_country <- c()
  country_to_send <- list_of_countries[,c(2,1)]
  country_to_send$message <- paste0("\n /",country_to_send$countryCode," - ", country_to_send$name)
  
  for(need_help_country in base_help_country$User_ID) {
    
    text_help_country <- iconv(paste0("Choose which country from the following list that you would like to receive trending topics. \n",
                                      toString(country_to_send$message),
                                      "\n \n In the next hour you will receive my messages. If you didn't choose any option then I will send you the GLOBAL."),"UTF-8","latin1")
    
        h <- has_error(bot$sendMessage(chat_id = need_help_country, text = text_help_country, parse_mode = "Markdown"))
    if(h == T){ 
      erro_new_help <- c(erro_new_help_country, need_help_country)
      ct_erro_new_help_country <- ct_erro_new_help_country + 1 }}
}
}

# Sending a message to another bot if there are any new users registered
bot2 = Bot(token = "TOKEN HERE")
if(nrow(base) != 0){
  bot2$sendMessage(chat_id = "YOUR TELEGRAM ID HERE", text = paste0("We have ", nrow(base), " new registered user(s)"))}
}
