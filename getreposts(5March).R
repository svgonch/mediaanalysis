##Get reposts
library(httr)
library(xlsx)
library(dplyr)
library(jsonlite)
library(stringr)
library(lubridate)
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
repost <- data.frame()
repost <- filter(data, reposts > 0)
## Function
repost_search <- function(data = media_data) {
      repost <- filter(data, reposts > 0) %>%
            mutate(own_id = media_id, new_id = id) %>%
            select(media_id, own_id, id, new_id, comments, likes, reposts, date)
      rep_list <- select(repost, media_id, own_id, id, new_id, reposts) %>%
            mutate(ready = 0)
      wa_se <- "https://api.vk.com/method/wall.getReposts?"
      owner_id <- "owner_id="
      vers <- "&v=5.45"
      post_id <- "&post_id="
      count <- "&count=1000"
      offsetting <- "&offset="
      zapros <- NULL
      while(nrow(rep_list) > 0) {
            for(i in 1:nrow(rep_list)) {
                  offset = 0
                  temp_sum <- data.frame(media_id=NULL, own_id=NULL, id=NULL, new_id=NULL, comments=NULL, likes=NULL, reposts=NULL, date=NULL)
                  temp_dat <- NULL
                  while(nrow(temp_sum) < rep_list[i,"reposts"]) {
                        zapros <- paste0(wa_se, owner_id, rep_list[i,"media_id"], post_id, rep_list[i,"id"], count, offsetting, offset, vers, "&access_token=", token)
                        rep_media <- fromJSON(zapros)
                        if(length(rep_media$response$items$id) > 0) {
                              new_id <- as.factor(rep_media$response$items$id)
                              own_id <- as.factor(rep_media$response$items$from_id)
                              date <- as.POSIXct(rep_media$response$items$date, origin = "1970-01-01")
                              media_id <- as.factor(rep(rep_list[i,"media_id"], length(rep_media$response$items$id)))
                              id <- as.factor(rep(rep_list[i,"id"], length(rep_media$response$items$id)))
                              likes <- as.integer(rep_media$response$items$likes$count)
                              comments <- as.integer(rep_media$response$items$comments$count)
                              reposts <- as.integer(rep_media$response$items$reposts$count)
                              
                              temp_dat <- data.frame(media_id, own_id, id, new_id, comments, likes, reposts, date)
                              rep_temp <- filter(temp_dat, reposts > 0) %>%
                                    select(media_id, own_id, id, new_id, reposts) %>%
                                    mutate(ready = 0)
                              
                              repost <- rbind(repost, temp_dat)
                              
                              offset <- offset + length(rep_media$response$items$id)      
                        } else {temp_dat <- NULL}
                        temp_sum <- rbind(temp_sum, temp_dat)
                  }
                  rep_list[i,"ready"] <- 1
            }
            rep_list <- rbind(rep_list, rep_temp)
            rep_list <- filter(rep_list, ready == 0)
            ostalos <- nrow(rep_list)
            sdelano <- nrow(repost)
            printer <- paste("Сделано", sdelano, "  ", "Осталось", ostalos, sep = " ")
            print(printer)
      }
      repost$media_id <- factor(repost$media_id, 
                                      levels = media[media$num %in% intersect(repost$media_id, media$num),"num"], 
                                      labels = media[media$num %in% intersect(repost$media_id, media$num),"name"])
      assign("repost_data", repost, envir = globalenv())
}

##MISC
repost_data <- unique(repost_data)
repost_data <- na.omit(repost_data)
