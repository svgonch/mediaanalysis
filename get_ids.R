library(httr)
library(xlsx)
library(dplyr)
library(jsonlite)
library(stringr)
library(lubridate)
library(curl)
myvk <- oauth_app("appfordata",
                  key = "5231387",
                  secret = "NmRYnpPuU5mzwppsQAPS")
vk <- oauth_endpoint(NULL,
                     authorize = "https://oauth.vk.com/authorize",
                     access = "https://oauth.vk.com/access_token"
)
vk_auth <- oauth2.0_token(vk, myvk, scope = "offline", type = "application/x-www-form-urlencoded", cache = FALSE)
tmp <- strsplit(toString(names(vk_auth$credentials)), '"')
token <- tmp[[1]][4]
## Creating df
setwd("D:/Research/VK/media_analysis")
users <- data.frame()

users_get <- function(data = repost_data) {
      users <- as.character(data[grep("^[0-9]", data$own_id),"own_id"])
      users <- unique(users)
      wa_se <- "https://api.vk.com/method/users.get?"
      user_ids <- "user_ids="
      param <- paste(c("sex", "bdate", "city", "country", "education", "followers_count", "followers_count", "personal"), collapse = "%2C")
      fields <- "&fields="
      vers <- "&v=5.45"

      ids <- data.frame()      
      np <- ceiling(length(users)/100)
      suppressWarnings(user_matrix <- matrix(users, nrow = np, byrow = TRUE))
      for(i in 1:nrow(user_matrix)) {
            portion <- user_matrix[i,]
            portion_col <- paste(portion, collapse = "%2C")
            zapros <- paste(wa_se, user_ids, portion_col, fields, param, vers, "&access_token=", token, sep = "")
            conn <- curl(zapros)
            ids_data <- readLines(conn, warn = FALSE)
            ids_data <- fromJSON(ids_data)
            close(conn)
            
            ids_temp <- data.frame(own_id = ids_data$response$id,
                                   first_name = ids_data$response$first_name,
                                   last_name = ids_data$response$last_name,
                                   sex = ids_data$response$sex,
                                   bd = ids_data$response$bdate,
                                   city = ids_data$response$city$title,
                                   country = ids_data$response$country$title,
                                   followers_count = ids_data$response$followers_count,
                                   uni = ids_data$response$university_name,
                                   political = NA, religion = NA, life_main = NA, people_main = NA
                                   )
            for(f in 1:nrow(ids_temp)) {
                  ifelse(length(ids_data$response$personal[[f]]$political) > 0, ids_temp[f,"political"] <- ids_data$response$personal[[f]]$political, NA)
                  ifelse(length(ids_data$response$personal[[f]]$religion) > 0, ids_temp[f,"religion"] <- ids_data$response$personal[[f]]$religion, NA)
                  ifelse(length(ids_data$response$personal[[f]]$life_main) > 0, ids_temp[f,"life_main"] <- ids_data$response$personal[[f]]$life_main, NA)
                  ifelse(length(ids_data$response$personal[[f]]$people_main) > 0, ids_temp[f,"people_main"] <- ids_data$response$personal[[f]]$people_main, NA)
            }
            ids <- rbind(ids, ids_temp)
      }
      ids <- unique(ids)
      ids$sex <- factor(ids$sex, levels = 0:2, labels = c("пол не указан", "женский", "мужской"))
      ids$political <- factor(ids$political, levels = 1:9, labels = c("коммунистические", "социалистические", "умеренные",
                                                                              "либеральные", "консервативные", "монархические",
                                                                              "ультраконсервативные", "индифферентные", "либертарианские"))
      ids$life_main <- factor(ids$life_main, levels = 1:8, labels = c("семья и дети", "карьера и деньги", "развлечения и отдых",
                                                                      "наука и исследования", "совершенствование мира", "саморазвитие",
                                                                      "красота и искусство", "слава и влияние"))
      ids$people_main <- factor(ids$people_main, levels = 1:6, labels = c("ум и креативность", "доброта и честность", "красота и здоровье",
                                                                          "власть и богатство", "смелость и упорство", "юмор и жизнелюбие"))
      assign("ids_data", ids, envir = globalenv())
}
