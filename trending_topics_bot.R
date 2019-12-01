if(!require("install.load")) {
  install.packages("install.load")
  library(install.load)
}
install_load("dplyr","rtweet","RSQLite", "tidyr","curl","purrr","telegram", 
             "telegram.bot","stringr", "plyr", "lubridate")

setwd("/home/ec2-user/trending_topics/")

# carregando as chaves 
token <- create_token(
  app = "Chave aqui", #chaves de exemplos. Não funcionam
  consumer_key = "AbLJQNB3rbsRkjsdfhfbyudsgvfsudyfvsdyfWjQqwhMI6co81",
  consumer_secret = "vUmtCCpVk9zx30nULkjSfsdsdhfgsdfsdfsdfsyifgsdfdjF0TRjs6oDe00gWHhgU8QJ4UOT1Ozo",
  access_token = "2470959763-sdsdfPXxWwnwJffSDeC6OeyIHXkX7LSywgdhtjiYiKEKnTeDRnYA",
  access_secret = "74hOI76WwxkBBAH3KfQyyIkVogFcii3eLeR3fdgWDei0rChA")

#token <- readRDS("keys.rds")

#criando o banco em sqlite
db_trends <- dbConnect(SQLite(), dbname="dados_trendingtopics.sqlite")

# buscando informações do bot
bot = Bot(token = "87299345621:AAER6OoAxHNySMYofdsfsdf546hyujyILpBS18hg9HM_YbVkg0") # Chave falsa.
msgs <- bot$getUpdates() #carregando as msgs enviadas para o bot

#tratando as informações
processing <- function(msgs){
  interm <- as.list(msgs)
  if(is.null(interm$message$text)){
    data <- data.frame(NULL)
    return(data)
  } else {
    
    data <- data.frame(User_ID = as.character(interm$message$from_user),
                       Message_ID = as.character(interm$message$message_id),
                       Date = as.POSIXct(interm$message$date,origin = "1970-01-01"),
                       Name = interm$message$from$first_name,
                       Text = interm$message$text)
    
    if(str_detect(data$Text, fixed( "/start", ignore_case=TRUE)) == TRUE ){
      data$Text_type <- "START"
    } else if(str_detect(data$Text, fixed( "/stop", ignore_case=TRUE)) == TRUE){
      data$Text_type <- "STOP"  
    } else { data$Text_type <- NA}
    
    data <- data %>% filter(!is.na(Text_type))
    
    if(nrow(data) == 0){ data <- data.frame(NULL)}  
    
    return(data)}}

base <- ldply(lapply(msgs, processing), rbind) %>% arrange(desc(Date))

#salvando as msgs numa tabela e mandando msg de boas-vindas
if(dbExistsTable(db_trends, "base_bot") == TRUE){
message_old <- dbSendQuery(db_trends, "Select Message_ID from base_bot")
message_old <- dbFetch(message_old)
message_old <- message_old$Message_ID

base <- base %>% filter(!Message_ID %in% message_old)
if(nrow(base) != 0){
  
  presentation <- base %>% filter(Text_type == "START") %>% 
                          distinct(User_ID, Text_type,.keep_all = T)
  for(chat_id_new in presentation$User_ID) {
  text_presentation <- paste0("Olá ",presentation[presentation$User_ID == chat_id_new,4], ", eu sou o *Trendinho* e vou trazer pra você a lista das principais hashtags do Twitter a cada hora. Eu ainda estou em teste, caso tenha alguma sugestão é só enviar para o meu criador, o @trifenol. Para cortar relações comigo basta digitar _/stop_ e não receberá mais informações. Todas as hashtags são clicáveis e levam direto ao Twitter.ND = Dados não disponíveis")
  bot$sendMessage(chat_id = chat_id_new, text = text_presentation, parse_mode = "Markdown")
 # bot$sendAnimation(chat_id = chat_id_new, animation = "3ce93dd4-994c-48fe-8bf9-3459728bcfad.webp")
  }
#salvando o full
dbWriteTable(conn = db_trends, name = "base_bot" ,value = base, 
             row.names = FALSE, append = T)}
} else {
  dbWriteTable(conn = db_trends, name = "base_bot" ,value = base, 
               row.names = FALSE, append = T)}

# carregando os usuários cadastrados
base_transition <- dbReadTable(db_trends, "base_bot")

# Deixando apenas os que ainda querem receber
base_transition <- base_transition %>% 
  group_by(User_ID) %>% 
  filter(Message_ID == max(Message_ID) & Text_type == "START") %>%
  top_n(n=1, wt = Message_ID) 
  
# pacote purrr para teste de função
safe_test <- safely(trends_available)
for(i in 1:2){  safe_test()}

#buscando os dados na api
busca <- get_trends(woeid = "23424768", token = token)
busca <- busca %>% select(trend, promoted_content, tweet_volume,as_of,url) %>% as.data.frame()
  # armazenando numa tabela
dbWriteTable(conn = db_trends, name = "Brazil", value = busca, row.names = FALSE, append = T)

# criando as msgs que serão enviadas para os usuários
busca$tweet_volume <-  replace_na(busca$tweet_volume, "ND")
busca$message <- paste0(" \n [", busca$trend,"](",busca$url,") - Tweets: ", busca$tweet_volume)

text <- paste("Esses são os principais assuntos do Brasil às",
              format(Sys.time(), "%X %a %d %b %Y"))

# Enviando as msgs
  for (chat_id in base_transition$User_ID) {
   bot$sendMessage(chat_id = chat_id, text = text)
   bot$sendMessage(chat_id = chat_id, text = toString(busca$message), parse_mode='Markdown')
  }

#disconectando o banco
dbDisconnect(db_trends)






# verificando as msgs que os usuários mandam
# verifica <- function(msgs){
#   interm <- as.list(msgs)
#   print(paste(interm$message$from$first_name, "||",interm$message$text))
# }
# invisible(lapply(msgs,verifica ))
