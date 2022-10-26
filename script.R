library(jsonlite)
library(tidyverse)
library(tidytext)
library(dplyr)

#Gerando o Data Frame 

json_data <- read_json(path='C:/Users/David/Desktop/tcc/chat/exemplo/28maio22.json', simplifyVector = TRUE)

#Transformando em Colunas 

json_data <- unnest(json_data)

#Retirando as Palavras e o Tempo de cada Usuário

json_data <- json_data[, c('name', 'body', 'created_at')]

#Remover ‘ dos Unigrams e dos Bigrams

json_data <- mutate(json_data, body = gsub("['’]", "", body))
json_data <- json_data[json_data$name!="nightbot",]
json_data <- json_data[json_data$name!="fossabot",]
json_data <- mutate(json_data, body = gsub("@[A-Z,a-z,0-9]*", "", body))
json_data <- mutate(json_data, body = gsub("![A-Z,a-z,0-9]*", "", body))
json_data <- mutate(json_data, body = gsub("[A-Z,a-z,0-9]* subscribed with Prime.*", "", body))
json_data <- mutate(json_data, body = gsub("[A-Z,a-z,0-9]* subscribed at Tier [0-9]+.*", "", body))
json_data <- mutate(json_data, body = gsub("(sup(p)*)|(SUP(P)*)", "sup", body))

#Tokenizando os Unigrams 

unigram <- json_data %>%
  unnest_tokens(word, body)

unigram <- mutate(unigram, word = gsub("^[0-9]*[A-Z,a-z,0-9]*[0-9]$", "", word))

unigram <- unigram %>%
  filter(!word == "")

#Retirando as Palavras Negativas da Tabela de Stopwords

sw <- view(stop_words)
sw <- sw %>% filter(str_sub(word, -3, -1)!="not" & str_sub(word, -3, -1)!="n't")
sw <- mutate(sw, word = gsub("'", "", word))

#Adicionando os Campeões do LOL na Lista de Stopwords

mysw <- tibble(word = c("aatrox", "ahri", "akali", "akshan", "alistar",
                        "amumu", "anivia", "annie", "aphelios", "ashe",
                        "aurelion sol", "aurelion", "azir", "bard",
                        "blitzcrank", "brand", "braum", "caitlyn", "cait",
                        "camille", "cassiopeia", "cho'gath", "chogath",
                        "cho", "corki", "darius", "diana", "dr. mundo",
                        "mundo", "draven", "ekko", "elise", "evelynn",
                        "ezreal", "fiddlesticks", "fiddle", "fiora", "fizz",
                        "galio", "gangplank", "gp", "garen", "gnar", "gragas",
                        "graves", "gwen", "hecarim", "heca", "heimerdinger",
                        "heimer", "illaoi", "irelia", "ivern", "janna",
                        "jarvan IV", "jarvan", "jax", "jayce", "jhin", "jinx",
                        "kai'sa", "kaisa", "kalista", "karma", "karthus",
                        "kassadin", "katarina", "kayle", "kayn", "kennen",
                        "kha'zix", "khazix", "kindred", "kled", "kog'maw",
                        "kogmaw", "kog", "leblanc", "lee sin", "lee", "leona",
                        "lillia", "lissandra", "lucian", "lulu", "lux",
                        "malphite", "malph", "malzahar", "maokai", "master yi",
                        "master", "miss fortune", "mf","mordekaiser", "morgana",
                        "nami", "nasus", "nautilus", "neeko", "nidalee", "nida",
                        "nocturne", "noc", "nunu & willump", "nunu", "olaf",
                        "orianna", "ornn", "pantheon", "poppy", "pyke", "qiyana",
                        "quinn", "rakan", "rammus", "rek'sai", "reksai", "rell",
                        "renata glasc", "renata", "renekton","renek",
                        "rengar", "riven", "rumble", "ryze", "samira", "sejuani",
                        "senna", "seraphine", "sona2.0", "sett", "shaco", "shen",
                        "shyvana", "singed", "sion", "sivir", "skarner",
                        "sona", "soraka", "swain", "sylas", "syndra",
                        "tahm kench", "tahmkench", "tk", "taliyah",
                        "talon", "taric", "teemo", "thresh", "tristana",
                        "trundle", "tryndamere", "twisted fate","twistedfate",
                        "tf", "twitch", "udyr", "urgot", "varus", "vayne",
                        "veigar", "vel'koz", "velkoz", "vex", "vi",
                        "viego", "viktor", "vladimir", "volibear", "warwick",
                        "ww", "wukong", "xayah", "xerath", "xin zhao", "xinzhao",
                        "xin", "xz", "yasuo", "yone", "yorick", "yuumi",
                        "zac", "zed", "zeri", "ziggs", "zilean", "zoe",
                        "zyra", "korea", "toplane", "toplaner","toplaners",
                        "jg", "mid", "botlane", "adc", "adcs", "adcarry",
                        "support", "supports", "sup", "sups", "suport", "suports",
                        "supsup", "supps", "tyler", "game", "yep", "chat", "play", 
                        "bro", "stream", "stream", "korean", "games", "champ", 
                        "yeah", "skin", "ad", "ap", "map")
                              , lexicon = "lol")

unigram <- anti_join(unigram, mysw, 
                     by = "word")

#Retirando os Stopwords

unigram <- unigram %>%
  anti_join(sw)

#Tokenizando os Bigrams 

bigram <- json_data %>%
  unnest_tokens(bigram, body, token = "ngrams", n = 2)

#Separando os Bigrams

bigram  <- bigram  %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Retirando os Stopwords e as MyStopwords

bigram <- bigram %>%
  filter(!word1 %in% sw$word) %>%
  filter(!word2 %in% sw$word)

bigram <- bigram %>%
  filter(!word1 %in% mysw$word) %>%
  filter(!word2 %in% mysw$word)

#Retirando os Resultados NA dos Bigrams (linhas onde não existem bigrams)

bigram  <- bigram  %>%
  filter(!is.na(word1)) %>%
  filter(!is.na(word2))

#Criando o NRC Lexicon Modificado (Textos e Emotes juntos) 

nrc_te <- read.table('C:/Users/David/Desktop/tcc/chat/exemplo/nrc_text_emote - 23-05.txt',
                     header = TRUE,
                     sep = '\t',
                     stringsAsFactors = FALSE)

#Fazendo a Junção da Tabela de Unigram com seus Sentimentos

unigram_sentiments <- unigram %>%
  inner_join(nrc_te) %>%
  group_by(name)

unigram_sentiments <- unigram_sentiments %>%
  filter(value == 1)

#Fazendo a Junção da Tabela de Bigram com seus Sentimentos

bigram_sentiments <- bigram %>%
  inner_join(nrc_te, by = c(word1 = "word")) %>%
  group_by(name)

bigram_sentiments <- bigram_sentiments %>%
  rename(emotion1 = emotion, value1 = value, type1 = type)

bigram_sentiments <- bigram_sentiments %>%
  inner_join(nrc_te, by = c(word2 = "word")) %>%
  group_by(name)

bigram_sentiments <- bigram_sentiments %>%
  rename(emotion2 = emotion, value2 = value, type2 = type)

bigram_sentiments <- bigram_sentiments %>%
  filter(value1 == 1 & value2 == 1)

#Sentiment Expression - Unigrams

unigram_sentiments_count <- unigram_sentiments %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

#Regra 1 - Usando os not’s e don’ts para inverter os sentimentos

rule_1 <- bigram_sentiments %>% 
  filter(emotion1 == "negation") 

rule_1  <- rule_1[, c('name', 'created_at', 'word1', 'word2', 'emotion2')]
  
rule_1['emotion'] <- NA

rule_1 <- rule_1 %>%
  mutate( emotion = case_when(
    emotion2 == "negative" ~ "positive",
    emotion2 == "positive" ~ "negative",
    emotion2 == "trust" ~ "fear",
    emotion2 == "anticipation" ~ "surprise",
    emotion2 == "joy" ~ "sadness",
    emotion2 == "surprise" ~ "anticipation",
    emotion2 == "fear" ~ "trust",
    emotion2 == "anger" ~ "love",
    emotion2 == "sadness" ~ "joy",
    emotion2 == "disgust" ~ "appreciation"
  ))

rule_1_sentiments_count <- rule_1 %>%
  count(emotion2, emotion, sort=TRUE) %>%
  arrange(name)

rule_2 <- bigram_sentiments %>% 
  filter((type1 == "text") & 
        (emotion1 == "positive" | emotion1 == "trust" | 
        emotion1 == "anticipation" | emotion1 == "joy" | emotion1 == "surprise") & 
        (type2 == "emote") &
        (emotion2 == "posneg" | emotion2 == "negative"))

rule_2  <- rule_2[, c('name', 'created_at', 'word1', 'word2', 'emotion1', 'emotion2')]

rule_2['emotion'] <- "irony"

rule_2_1 <- distinct(rule_2, created_at, .keep_all = TRUE)

rule_2_sentiments_count <- rule_2_1 %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

rule_3 <- bigram_sentiments %>% 
  filter((type1 == "text") & 
           (emotion1 == "negative" | emotion1 == "fear" 
            | emotion1 == "anger" | emotion1 == "sadness" | emotion1 == "disgust"
            | emotion1 == "surprise") & 
           (type2 == "emote") &
           (emotion2 == "posneg" | emotion2 == "positive"))

rule_3  <- rule_3[, c('name', 'created_at', 'word1', 'word2', 'emotion1', 'emotion2')]

rule_3['emotion'] <- "humor"

rule_3_1 <- distinct(rule_3, created_at, .keep_all = TRUE)

rule_3_sentiments_count <- rule_3_1 %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

rule_4 <- bigram_sentiments %>% 
  filter((type1 == "text") & 
           (emotion1 == "positive" | emotion1 == "trust" | emotion1 == "anticipation" 
            | emotion1 == "joy" | emotion1 == "surprise") &
           (type2 == "emote") &
           (emotion2 == "positive"))

rule_4  <- rule_4[, c('name', 'created_at', 'word1', 'word2', 'emotion1', 'emotion2')]

rule_4['emotion'] <- "positive"

rule_4_1 <- distinct(rule_4, created_at, .keep_all = TRUE)

rule_4_sentiments_count <- rule_4_1 %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

rule_5 <- bigram_sentiments %>% 
  filter((type1 == "text") & 
           (emotion1 == "negative" | emotion1 == "fear" 
            | emotion1 == "anger" | emotion1 == "sadness" | emotion1 == "disgust"
            | emotion1 == "surprise") & 
           (type2 == "emote") &
           (emotion2 == "negative"))

rule_5  <- rule_5[, c('name', 'created_at', 'word1', 'word2', 'emotion1', 'emotion2')]

rule_5['emotion'] <- "negative"

rule_5_1 <- distinct(rule_5, created_at, .keep_all = TRUE)

rule_5_sentiments_count <- rule_5_1 %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

rule_6 <- bigram_sentiments %>% 
  filter((type1 == "emote") & 
           (emotion1 == "posneg") & 
           (type2 == "emote") &
           (emotion2 == "posneg"))

rule_6  <- rule_6[, c('name', 'created_at', 'word1', 'word2', 'emotion1', 'emotion2')]

rule_6['emotion'] <- "irony"

rule_6_1 <- distinct(rule_6, created_at, .keep_all = TRUE)

rule_6_sentiments_count <- rule_6_1 %>%
  count(name,emotion, sort=TRUE) %>%
  arrange(name)

unigram_sentiments_all <- left_join(unigram, unigram_sentiments)

unigram_sentiments_all_count <- unigram_sentiments_all %>%
  count(name,word,emotion, sort=TRUE) %>%
  arrange(name)

unigram_sentiments_all_count %>%
  filter_all(any_vars(! is.na(.)))

unigram_sentiments_all_count_na <- unigram_sentiments_all_count %>%
  filter(is.na(emotion))

unigram_sentiments_count_na <- unigram_sentiments_all_count_na %>%
  count(word, sort=TRUE)

unigram_sentiments_count_na %>% summarise(sum(unigram_sentiments_count_na$n))

unigram_sentiments$rownumber = 1:dim(unigram_sentiments)[1]
unigram_sentiments = dplyr::mutate(unigram_sentiments, emotecount=0)
unigram_sentiments_filter <- unigram_sentiments #%>%
  #filter(emotion == "positive" | emotion == "negative" | emotion == "posneg")

count <- 0
for(i in 1:nrow(unigram_sentiments_filter)) {
  if(unigram_sentiments_filter[i,6] == "text"){
    count <- 0
  }
  else{
    count <- count + 1
    unigram_sentiments_filter[i,8] <- count
  }
}

#unigram_sentiments_filter = dplyr::mutate(unigram_sentiments_filter, emotespam=0)
unigram_sentiments_filter <- unigram_sentiments_filter %>% 
  filter(emotespam == "0")

i <- nrow(unigram_sentiments_filter)
#print(i)
while(i >= 1)
  {
  #print(i)
  if(unigram_sentiments_filter[i,8] >= 50)
    {
    j <- unigram_sentiments_filter[i,8]
    while(j >= 1)
      {
      unigram_sentiments_filter[i,9] <- 1
      j <- j-1
      i <- i-1
      }
    i <- i+1
    }
    i <- i-1
}

unigram_sentiments_filter_count <- unigram_sentiments_filter %>%
  count(name,created_at,emotion, sort=TRUE) %>%
  arrange(name)

aldenioburner
rule_1_sentiments_count_all <- left_join(unigram_sentiments_filter_count, rule_1_sentiments_count, 
                              by = c("name", "emotion" = "emotion2")) %>%
                    arrange(name)

#rule_1_sentiments_count_all['n'] <- NA
rule_1_sentiments_count_all <- mutate(rule_1_sentiments_count_all, n = n.x)
rule_1_sentiments_count_all <- mutate(rule_1_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_1_sentiments_count_all <- rule_1_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_1_1_count <- rule_1 %>%
  count(emotion, sort=TRUE) %>%
  arrange(name)

rule_1_sentiments_count_all <- union_all(rule_1_sentiments_count_all, rule_1_1_count)

rule_1_sentiments_count_all <- aggregate(rule_1_sentiments_count_all$n, 
                      list(rule_1_sentiments_count_all$name,rule_1_sentiments_count_all$created_at,rule_1_sentiments_count_all$emotion),FUN=sum) %>%
                      rename(name = Group.1, created_at = Group.2, emotion = Group.3, n = x) %>%
                      arrange(name)

rule_2_sentiments_count_all <- rule_2 %>%
  count(emotion1, sort=TRUE) %>%
  arrange(name)

rule_2_sentiments_count_all <- left_join(rule_1_sentiments_count_all, rule_2_sentiments_count_all, 
                                            by = c("name", "emotion" = "emotion1")) %>%
                                  arrange(name)

rule_2_sentiments_count_all <- mutate(rule_2_sentiments_count_all, n = n.x)
rule_2_sentiments_count_all <- mutate(rule_2_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_2_sentiments_count_all <- rule_2_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

#rule_2_1 <- rule_2 %>% filter(emotion1 == "positive")
rule_2_1_count <- rule_2_1 %>%
  count(emotion2, sort=TRUE) %>%
  arrange(name)

rule_2_sentiments_count_all <- left_join(rule_2_sentiments_count_all, rule_2_1_count, 
                                         by = c("name", "emotion" = "emotion2")) %>%
  arrange(name)

rule_2_sentiments_count_all <- mutate(rule_2_sentiments_count_all, n = n.x)
rule_2_sentiments_count_all <- mutate(rule_2_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_2_sentiments_count_all <- rule_2_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_2_sentiments_count_all <- union_all(rule_2_sentiments_count_all, rule_2_sentiments_count)

rule_3_sentiments_count_all <- rule_3 %>%
  count(emotion1, sort=TRUE) %>%
  arrange(name)

rule_3_sentiments_count_all <- left_join(rule_2_sentiments_count_all, rule_3_sentiments_count_all, 
                                         by = c("name", "emotion" = "emotion1")) %>%
  arrange(name)

rule_3_sentiments_count_all <- mutate(rule_3_sentiments_count_all, n = n.x)
rule_3_sentiments_count_all <- mutate(rule_3_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_3_sentiments_count_all <- rule_3_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

#filtrar emotion1 antes de fazer o tratamento com a emotion2 df rule3
#rule_3_1 <- distinct(rule_3, created_at, .keep_all = TRUE)
rule_3_1_count <- rule_3_1 %>%
  count(emotion2, sort=TRUE) %>%
  arrange(name)

rule_3_sentiments_count_all <- left_join(rule_3_sentiments_count_all, rule_3_1_count, 
                                         by = c("name", "emotion" = "emotion2")) %>%
  arrange(name)

rule_3_sentiments_count_all <- mutate(rule_3_sentiments_count_all, n = n.x)
rule_3_sentiments_count_all <- mutate(rule_3_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_3_sentiments_count_all <- rule_3_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_3_sentiments_count_all <- union_all(rule_3_sentiments_count_all, rule_3_sentiments_count)

rule_4_sentiments_count_all <- rule_4_1 %>%
  count(emotion1, sort=TRUE) %>%
  arrange(name)

rule_4_sentiments_count_all <- left_join(rule_3_sentiments_count_all, rule_4_sentiments_count, 
                                         by = c("name", "emotion")) %>%
  arrange(name)

rule_4_sentiments_count_all <- mutate(rule_4_sentiments_count_all, n = n.x)
rule_4_sentiments_count_all <- mutate(rule_4_sentiments_count_all, n = ifelse(! is.na(n.y), n + n.y, n))
rule_4_sentiments_count_all <- rule_4_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_5_sentiments_count_all <- rule_5_1 %>%
  count(emotion1, sort=TRUE) %>%
  arrange(name)

rule_5_sentiments_count_all <- left_join(rule_4_sentiments_count_all, rule_5_sentiments_count, 
                                         by = c("name", "emotion")) %>%
  arrange(name)

rule_5_sentiments_count_all <- mutate(rule_5_sentiments_count_all, n = n.x)
rule_5_sentiments_count_all <- mutate(rule_5_sentiments_count_all, n = ifelse(! is.na(n.y), n + n.y, n))
rule_5_sentiments_count_all <- rule_5_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_6_sentiments_count_all <- rule_6 %>%
  count(emotion1, sort=TRUE) %>%
  arrange(name)

rule_6_sentiments_count_all <- left_join(rule_5_sentiments_count_all, rule_6_sentiments_count_all, 
                                         by = c("name", "emotion" = "emotion1")) %>%
  arrange(name)

rule_6_sentiments_count_all <- mutate(rule_6_sentiments_count_all, n = n.x)
rule_6_sentiments_count_all <- mutate(rule_6_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_6_sentiments_count_all <- rule_6_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_6_1_count <- rule_6_1 %>%
  count(emotion2, sort=TRUE) %>%
  arrange(name)

rule_6_sentiments_count_all <- left_join(rule_6_sentiments_count_all, rule_6_1_count, 
                                         by = c("name", "emotion" = "emotion2")) %>%
  arrange(name)

rule_6_sentiments_count_all <- mutate(rule_6_sentiments_count_all, n = n.x)
rule_6_sentiments_count_all <- mutate(rule_6_sentiments_count_all, n = ifelse(! is.na(n.y), n - n.y, n))
rule_6_sentiments_count_all <- rule_6_sentiments_count_all[, c('name', "created_at", 'emotion', 'n')]

rule_6_sentiments_count_all <- union_all(rule_6_sentiments_count_all, rule_6_sentiments_count)

unigram_rules <- rule_6_sentiments_count_all

write.table(unigram_rules, "C:/Users/David/Desktop/2805/unigram_rules_28maio22.csv")
write.table(bigram, "C:/Users/David/Desktop/2805/bigram.csv")
write.table(bigram_sentiments, "C:/Users/David/Desktop/2805/bigram_sentiments.csv")
write.table(json_data, "C:/Users/David/Desktop/2805/json_data.csv")
write.table(mysw, "C:/Users/David/Desktop/2805/mysw.csv")
write.table(nrc_te, "C:/Users/David/Desktop/2805/nrc_te.csv")
write.table(rule_1, "C:/Users/David/Desktop/2805/rule_1.csv")
write.table(rule_1_1_count, "C:/Users/David/Desktop/2805/rule_1_1_count.csv")
write.table(rule_1_sentiments_count, "C:/Users/David/Desktop/2805/rule_1_sentiments_count.csv")
write.table(rule_1_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_1_sentiments_count_all.csv")
write.table(rule_2, "C:/Users/David/Desktop/2805/rule_2.csv")
write.table(rule_2_1, "C:/Users/David/Desktop/2805/rule_2_1.csv")
write.table(rule_2_1_count, "C:/Users/David/Desktop/2805/rule_2_1_count.csv")
write.table(rule_2_sentiments_count, "C:/Users/David/Desktop/2805/rule_2_sentiments_count.csv")
write.table(rule_2_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_2_sentiments_count_all.csv")
write.table(rule_3, "C:/Users/David/Desktop/2805/rule_3.csv")
write.table(rule_3_1, "C:/Users/David/Desktop/2805/rule_3_1.csv")
write.table(rule_3_1_count, "C:/Users/David/Desktop/2805/rule_3_1_count.csv")
write.table(rule_3_sentiments_count, "C:/Users/David/Desktop/2805/rule_3_sentiments_count.csv")
write.table(rule_3_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_3_sentiments_count_all.csv")
write.table(rule_4, "C:/Users/David/Desktop/2805/rule_4.csv")
write.table(rule_4_1, "C:/Users/David/Desktop/2805/rule_4_1.csv")
write.table(rule_2_sentiments_count, "C:/Users/David/Desktop/2805/rule_2_sentiments_count.csv")
write.table(rule_2_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_2_sentiments_count_all.csv")
write.table(rule_5, "C:/Users/David/Desktop/2805/rule_5.csv")
write.table(rule_5_1, "C:/Users/David/Desktop/2805/rule_5_1.csv")
write.table(rule_5_sentiments_count, "C:/Users/David/Desktop/2805/rule_5_sentiments_count.csv")
write.table(rule_5_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_5_sentiments_count_all.csv")
write.table(rule_6, "C:/Users/David/Desktop/2805/rule_6.csv")
write.table(rule_6_1, "C:/Users/David/Desktop/2805/rule_6_1.csv")
write.table(rule_6_1_count, "C:/Users/David/Desktop/2805/rule_6_1_count.csv")
write.table(rule_6_sentiments_count, "C:/Users/David/Desktop/2805/rule_6_sentiments_count.csv")
write.table(rule_6_sentiments_count_all, "C:/Users/David/Desktop/2805/rule_6_sentiments_count_all.csv")
write.table(sw, "C:/Users/David/Desktop/2805/sw.csv")
write.table(unigram, "C:/Users/David/Desktop/2805/unigram.csv")
write.table(unigram_sentiments, "C:/Users/David/Desktop/2805/unigram_sentiments.csv")
write.table(unigram_sentiments_all, "C:/Users/David/Desktop/2805/unigram_sentiments_all.csv")
write.table(unigram_sentiments_all_count, "C:/Users/David/Desktop/2805/unigram_sentiments_all_count.csv")
write.table(unigram_sentiments_all_count_na, "C:/Users/David/Desktop/2805/unigram_sentiments_all_count_na.csv")
write.table(unigram_sentiments_count, "C:/Users/David/Desktop/2805/unigram_sentiments_count.csv")
write.table(unigram_sentiments_count_na, "C:/Users/David/Desktop/2805/unigram_sentiments_count_na.csv")
write.table(unigram_sentiments_filter, "C:/Users/David/Desktop/2805/unigram_sentiments_filter.csv")
write.table(unigram_sentiments_filter_count, "C:/Users/David/Desktop/2805/unigram_sentiments_filter_count.csv")


library(ggplot2)
ggplot(unigram_sentiments, aes(emotion, value, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_1_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_2_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_3_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_4_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_5_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(rule_6_sentiments_count_all, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)
ggplot(unigram_rules, aes(emotion, n, color="red")) +
  geom_col(show.legend = FALSE)


library(tidyr)
  frequency <- unigram_sentiments_all_count_na %>%
  mutate(proportion = n / sum(n)) %>% 
    select(-n) 
  
library(scales)
library(ggplot2)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(proportion, name)) +
      geom_jitter()

write.table(frequency, "C:/Users/David/Desktop/frequency.csv")

