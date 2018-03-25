library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(SnowballC)


library(httr)
library(jsonlite)
library(stringr)
library(lubridate)
myvk <- oauth_app("appfordata",
                  key = "5908943",
                  secret = "T52IuqcNwf9aj3rhVi4i")
vk <- oauth_endpoint(NULL,
                     authorize = "https://oauth.vk.com/authorize",
                     access = "https://oauth.vk.com/access_token"
)
vk_auth <- oauth2.0_token(vk, myvk, scope = "offline", type = "application/x-www-form-urlencoded", cache = FALSE)
tmp <- strsplit(toString(names(vk_auth$credentials)), '"')
token <- tmp[[1]][4]

### wall get comments


wa_se <- "https://api.vk.com/method/board.getComments?"
group_id = "group_id=28256828"
topic_id = "&topic_id=27992857"
cnt = "&count=100"
ext = "&extended=1"
vers <- "&v=5.73"
token_field = "&access_token="
offsetting <- "&offset="
zapros <- paste0(wa_se, group_id, topic_id, cnt, ext, vers, token_field, token)

comms <- fromJSON(zapros)
N_data <- comms$response$count
N_data_off <- ceiling(N_data/100)
# Запуск отсюда
offset_n = -100
all_users <- data.frame()
all_data <- data.frame()
for(i in 1:N_data_off) {
       offset_n = offset_n + 100
       zapros <- paste0(wa_se, group_id, topic_id, cnt, ext, offsetting, offset_n, vers, token_field, token)
       df_tmp <- fromJSON(zapros)
       if(length(df_tmp$response$items$id) > 0) {
              dat <- data.frame(user_id = df_tmp$response$items$from_id,
                                id = df_tmp$response$items$id,
                                date = df_tmp$response$items$date,
                                text = as.character(df_tmp$response$items$text))
       }
       if(length(df_tmp$response$profiles$first_name) > 0) {
              usrs <- data.frame(user_id = df_tmp$response$profiles$id,
                                 name = df_tmp$response$profiles$first_name,
                                 last_name = df_tmp$response$profiles$last_name,
                                 sex = df_tmp$response$profiles$sex,
                                 deactivated = if(length(df_tmp$response$profiles$deactivated) > 0) {
                                        deactivated = df_tmp$response$profiles$deactivated
                                 } else {deactivated = rep(NA, length(df_tmp$response$profiles$first_name))}
              )
       }
       all_data <- rbind(all_data, dat)
       all_users <- rbind(all_users, usrs)
       Sys.sleep(2)
}

all_data$date <- as.POSIXct(all_data$date, origin = "1970-01-01")
all_data <- unique(all_data)
dup <- all_data[duplicated(all_data),]
all_users <- unique(all_users)
save_data <- all_users

save_data$new_age <- NA

library(vkR)
for(i in 1:nrow(save_data)) {
       tmp_age <- try(age_predict(save_data$user_id[i]), silent = TRUE)
       if(grepl("error", tmp_age, ignore.case = TRUE) == FALSE) {
              save_data$new_age[i] <- tmp_age$year_of_birth
       } else {save_data$new_age[i] <- "NO"}
       print(i)
}

all_users <- save_data
all_users <- select(all_users, user_id, name, last_name, sex, deactivated, new_age)

all_data2 <- join(all_data, all_users, by = "user_id")

saveRDS(all_data2, "D:/Research/nostalgia/word_data_nostalgia.RDS")
## Load RDS
all_data2 <- readRDS("D:/Research/nostalgia/word_data_nostalgia.RDS")

#all_data2$text <- as.character(all_data2$text)
all_data2 <- all_data2[-1207,]
str(all_data2)

#all_data2 <- unique(all_data2)

#all_data2 <- all_data2 %>%
 #      filter(user_id != 11682573) %>%
  #     filter(user_id != 43829746)

all_data2$text <- all_data2$text %>%
       str_replace_all("^id([a-z0-9]+)\\|", "") %>%
       str_replace_all("^post([a-z0-9]+)\\|", "") %>%
       # make text lower case
       str_to_lower %>% 
       str_replace_all("[^[:alpha:]]", " ") %>% 
       # collapse multiple spaces
       str_replace_all("[[:punct:]]", " ") %>%
       str_replace_all("\\s+", " ") %>%
       str_replace_all("id bp", "")

repl <- c("руки вверх", "дикий ангел", "казаки разбойники", "сектор газа", "love is", "малиновые пиджаки", "развал ссср")

all_data2$text <- gsub("малиновый", "малиновые", all_data2$text)

repl_s <- gsub(" ", "_", repl)

for(i in 1:length(repl_s)) {
       all_data2$text <- gsub(repl[i], repl_s[i], all_data2$text)
}

### Making bigrams
library(tm)
library(RWeka)
text_vec <- paste(all_data2$text, collapse = " ")
corp <- VCorpus(VectorSource(text_vec), readerControl = list(language = "ru"))
corp <- tm_map(corp, removeWords, stopwords("russian"))
corp <- tm_map(corp, PlainTextDocument)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TDMBigram <- TermDocumentMatrix(corp, control=list(tokenizer=BigramTokenizer))

tidy_bigram <- tidy(TDMBigram)
tidy_bigram <- arrange(tidy_bigram, desc(count))

##  Unigrams
text_by_word <- all_data2 %>%
       unnest_tokens(word, text)
stop_words_ru <- data.frame(word = c(tokenizers::stopwords("ru"), "иван", "просто", "которые", "туда", "первые",
                                     "кирилл", "елена", "алексей", "максим", "сих", "пор", "такие"))

dict <- text_by_word$word
text_by_word <- text_by_word %>%
       anti_join(stop_words_ru) %>%
       mutate(word = wordStem(word, language = "russian")) %>%
       filter(nchar(word) > 1)

text_by_word$word <- stemCompletion(text_by_word$word, dict)

text_by_word$new_age <- round(as.numeric(text_by_word$new_age),0)
text_by_word <- mutate(text_by_word, age = 2018 - new_age)
text_by_word <- text_by_word[!is.na(text_by_word$age), ]
text_by_word$age_group <- cut(text_by_word$age, breaks = c(15,25,30,35,50,90))
prop.table(table(text_by_word$age_group))*100

library(janitor)       
word_auth <- select(text_by_word, user_id, word)
#word_auth <- unique(word_auth)
all_count <- as.data.frame(sort(table(word_auth$word), decreasing = TRUE))
all_count <- all_count %>%
       filter(Freq >= 25) %>%
       mutate(Percent = Freq / length(unique(word_auth$user_id)) * 100) %>%
       mutate(Percent = round(Percent, 0))


png("D:/Research/nostalgia/all_count.png", width = 2500, height = 2000, res = 350)
ggplot(all_count, aes(reorder(Var1, Percent), Percent, label = Percent)) +
              geom_bar(stat = "identity") +
              labs(x = NULL, y = "% от упоминаний") +
              coord_flip() +
              geom_text(aes(fontface = "bold", size = 16), nudge_y = -0.4, color = "white") +
              theme_bw() +
              theme(legend.position = "none", 
                    axis.text.x=element_blank(),
                    axis.ticks.x = element_blank(),
                    text = element_text(face = "bold"))
dev.off()

text_by_sex <- text_by_word %>%
       count(sex, word, sort = TRUE) %>%
       ungroup() %>%
       group_by(sex) %>% 
       top_n(15) %>%
       arrange(sex, desc(n))

text_by_age <- text_by_word %>%
       filter(age <= 50) %>%
       count(age_group, word, sort = TRUE) %>%
       ungroup() %>%
       group_by(age_group) %>%
       top_n(15) %>%
       arrange(age_group, desc(n))

library(ggplot2)       
library(ggpubr)
library(lattice)


png("D:/Research/nostalgia/word_by_age_15.png", width = 3000, height = 2500, res = 350)
text_by_age %>%
       na.omit() %>%
       ggplot(aes(reorder(word, n), n, fill = age_group)) +
       geom_bar(stat = "identity") +
       geom_col(show.legend = FALSE) +
       labs(x = NULL, y = "n") +
       facet_wrap(~age_group, ncol = 2, scales = "free") +
       coord_flip()
dev.off()

text_by_sex$sex <- factor(text_by_sex$sex, labels = c("female", "male"))
png("D:/Research/nostalgia/word_by_sex.png", width = 3000, height = 2000, res = 450)
text_by_sex %>%
       na.omit() %>%
       ggplot(aes(reorder(word, n), n, fill = sex)) +
       geom_bar(stat = "identity") +
       geom_col(show.legend = FALSE) +
       labs(x = NULL, y = "n") +
       facet_wrap(~sex, ncol = 2, scales = "free") +
       coord_flip()
dev.off()
