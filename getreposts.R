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
repost_search <- function(data = "media_data") {
      repost <- filter(media_data, reposts > 0) %>%
      select(media_id, id, comments, likes, reposts, date) %>%
      mutate(ready = 0)
      wa_se <- "https://api.vk.com/method/wall.getReposts?"
      owner_id <- "owner_id="
      vers <- "&v=5.45"
      post_id <- "&post_id="
      count <- "&count=1000"
      offsetting <- "&offset="
      zapros <- NULL
      while(any(repost$ready == 0)) {
            if(repost[i,"reposts"] > 1000) {
                  nu_rep <- repost[i,"reposts"]
                  nt <- round(nu_rep/1000, 0)
                  offset = -1000
                  for(f in 1:(nt+1)) {
                        offset <- offset + 1000
                        zapros <- paste0(wa_se, owner_id, repost[i,"media_id"], post_id, repost[i,"id"], count, offsetting, offset, vers, "&access_token=", token)
                        rep_media <- fromJSON(zapros)
                        rep_id <- as.factor(rep_media$response$items$id)
                        own_id <- as.factor(rep_media$response$items$from_id)
                        dates <- as.character(rep_media$response$items$from_id)
                        media_id <- as.factor(rep(repost[i,"media_id"], length(rep_media$response$items$id)))
                        id <- as.factor(rep(repost[i,"id"], length(rep_media$response$items$id)))
                        likes <- as.integer(rep_media$response$items$likes$count)
                        comments <- as.integer(rep_media$response$items$comments$count)
                        reposts <- as.integer(rep_media$response$items$reposts$count)
                        repost_dat <- cbind(media_id, id, rep_id, own_id, dates, comments, likes, reposts)
                  }
            }

                  
            
      }
      
      
}
