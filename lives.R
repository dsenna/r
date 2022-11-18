library(jsonlite)
library(tidyverse)
library(tidytext)
library(dplyr)

unigram_rules.28maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_rules-28maio22.csv", sep="")
unigram_rules.29maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_rules-29maio22.csv", sep="")
unigram_rules.30maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_rules-30maio22.csv", sep="")
unigram_rules.31maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_rules-31maio22.csv", sep="")
unigram_rules.01junho22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_rules-01junho22.csv", sep="")
unigram_sentiments_all.28maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_all-28maio22.csv", sep="")
unigram_sentiments_all.29maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_all-29maio22.csv", sep="")
unigram_sentiments_all.30maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_all-30maio22.csv", sep="")
unigram_sentiments_all.31maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_all-31maio22.csv", sep="")
unigram_sentiments_all.01junho22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_all-01junho22.csv", sep="")
unigram_sentiments_filter.28maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_filter-28maio22.csv", sep="")
unigram_sentiments_filter.29maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_filter-29maio22.csv", sep="")
unigram_sentiments_filter.30maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_filter-30maio22.csv", sep="")
unigram_sentiments_filter.31maio22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_filter-31maio22.csv", sep="")
unigram_sentiments_filter.01junho22 <- read.csv("C:/Users/David/Desktop/tcc/dados/lives/unigram_sentiments_filter-01junho22.csv", sep="")

unigram_sentiments_all.28maio22 <- unigram_sentiments_all.28maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_all.29maio22 <- unigram_sentiments_all.29maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_all.30maio22 <- unigram_sentiments_all.30maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_all.31maio22 <- unigram_sentiments_all.31maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_all.01junho22 <- unigram_sentiments_all.01junho22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]

unigram_sentiments_filter.28maio22 <- unigram_sentiments_filter.28maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_filter.29maio22 <- unigram_sentiments_filter.29maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_filter.30maio22 <- unigram_sentiments_filter.30maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_filter.31maio22 <- unigram_sentiments_filter.31maio22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]
unigram_sentiments_filter.01junho22 <- unigram_sentiments_filter.01junho22[, c('name', 'created_at', 'word', 'emotion', 'value', 'type')]

unigram_rules.28maio22 <- unigram_rules.28maio22 [, c('name', 'created_at', 'emotion', 'n')]
unigram_rules.29maio22 <- unigram_rules.29maio22 [, c('name', 'created_at', 'emotion', 'n')]
unigram_rules.30maio22 <- unigram_rules.30maio22 [, c('name', 'created_at', 'emotion', 'n')]
unigram_rules.31maio22 <- unigram_rules.31maio22 [, c('name', 'created_at', 'emotion', 'n')]
unigram_rules.01junho22 <- unigram_rules.01junho22 [, c('name', 'created_at', 'emotion', 'n')]

lives <- union_all(unigram_sentiments_all.28maio22, unigram_sentiments_all.29maio22)
lives <- union_all(lives, unigram_sentiments_all.30maio22)
lives <- union_all(lives, unigram_sentiments_all.31maio22)
lives <- union_all(lives, unigram_sentiments_all.01junho22)

lives_filter <- union_all(unigram_sentiments_filter.28maio22, unigram_sentiments_filter.29maio22)
lives_filter <- union_all(lives_filter, unigram_sentiments_filter.30maio22)
lives_filter <- union_all(lives_filter, unigram_sentiments_filter.31maio22)
lives_filter <- union_all(lives_filter, unigram_sentiments_filter.01junho22)

lives_rules <- union_all(unigram_rules.28maio22, unigram_rules.29maio22)
lives_rules <- union_all(lives_rules, unigram_rules.30maio22)
lives_rules <- union_all(lives_rules, unigram_rules.31maio22)
lives_rules <- union_all(lives_rules, unigram_rules.01junho22)

write.table(lives, "C:/Users/David/Desktop/tcc/dados/lives/lives.csv")
write.table(lives_rules, "C:/Users/David/Desktop/tcc/dados/lives/lives_rules.csv")
write.table(lives_filter, "C:/Users/David/Desktop/tcc/dados/lives/lives_filter.csv")

i <- nrow(lives_rules)
while(i>=1){
  if(lives_rules[i,4]<0){
    lives_rules[i,4]=0
  }
  i<-i-1
}

write.table(lives_rules, "C:/Users/David/Desktop/tcc/dados/lives/lives_rules_sem_negativo.csv")


unigram_rules_count <- lives_rules %>%
  group_by(name,emotion) %>%
  summarise(soma_n = sum(n))

#nrow(distinct(teste_aggregate,name))

unigram_rules_count_1 <- unigram_rules_count %>% 
  filter(emotion == "irony" | emotion == "disgust" | emotion == "anger" | emotion == "fear")

unigram_rules_count_2 <- unigram_rules_count %>% 
  filter(emotion != "irony" & emotion != "disgust" & emotion != "anger" & emotion != "fear")

unigram_rules_count_1_soma <- aggregate(unigram_rules_count_1$soma_n, 
                                        list(unigram_rules_count_1$name),FUN=sum) %>%
  rename(name = Group.1, ntoxico = x) %>%
  arrange(name)

unigram_rules_count_2_soma <- aggregate(unigram_rules_count_2$soma_n, 
                                        list(unigram_rules_count_2$name),FUN=sum) %>%
  rename(name = Group.1, nntoxico = x) %>%
  arrange(name)

unigram_rules_count_soma <- aggregate(unigram_rules_count$soma_n, 
                                      list(unigram_rules_count$name),FUN=sum) %>%
  rename(name = Group.1, ntotal = x) %>%
  arrange(name)

unigram_rules_count_sum_all <- merge(unigram_rules_count_1_soma,unigram_rules_count_2_soma)
unigram_rules_count_sum_all <- merge(unigram_rules_count_1_soma, unigram_rules_count_2_soma, by.x = "name", by.y = "name", all.x = TRUE, all.y = TRUE)
unigram_rules_count_sum_all <- merge(unigram_rules_count_sum_all, unigram_rules_count_soma)

unigram_rules_count_sum_all[is.na(unigram_rules_count_sum_all)] <- 0

unigram_rules_count_sum_all['porcentagem'] <- 0

unigram_rules_count_sum_all <- mutate(unigram_rules_count_sum_all, porcentagem = (ntoxico/ntotal))

unigram_rules_count_sum_all['categoria'] <- unigram_rules_count_sum_all$porcentagem
i <- nrow(unigram_rules_count_sum_all)
while(i>=1){
  if(unigram_rules_count_sum_all[i,6]>0.5){
    unigram_rules_count_sum_all[i,6]="toxico"
  }
  else
  {
    unigram_rules_count_sum_all[i,6]="ntoxico" 
  }
  i<-i-1
}

unigram_rules_count_sum_all_filter <- unigram_rules_count_sum_all %>% 
  filter(categoria == "toxico")

unigram_rules_count_sum_all_filter %>% summarise(sum(unigram_rules_count_sum_all_filter$ntoxico))

unigram_rules_count_sum_all %>% summarise(sum(unigram_rules_count_sum_all$ntoxico))

write.table(unigram_rules_count_sum_all, "C:/Users/David/Desktop/tcc/dados/lives/lives_toxico.csv")

