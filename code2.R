library(httr)
library(xlsx)
library(dplyr)
library(jsonlite)
library(stringr)
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
## Read media list
setwd("D:/Research/VK/media_analysis")
media <- read.table("MEDIA_list.txt", sep = "", col.names = c("name", "num", "class"), colClasses = c("character", "character", "factor"))
id_s <- media[,2]
#################### KEYWORDS SEARCH ####################
# ÀÐÃÓÌÅÍÒÛ ÏÎÈÑÊÀ
## Media search
media_data <- data.frame()
media_search <- function(media_name = "all", query = "", query_grep = "", count = "50", offset = "0") {
      wa_se <- "https://api.vk.com/method/wall.search?"
      owner_id <- "owner_id="
      vers <- "&v=5.45"
      query_form <- "&query="
      ext <- "&extended=1"
      own <- "&owners_only=1"
      fields <- "&fields=members_count"
      counting <- "&count="
      offsetting <- "&offset="
      ## Go!
      if(media_name == "all") {
            id <- media[,2]
      }
      else {id <- media[which(media_name %in% media$name),2]}
      count <- as.numeric(count)
      offset <- as.numeric(offset)
      ## formula
      xxx <- 0
      yyy <- 0
      for(i in 1:length(id)) {
            zapros <- NULL
            zapros <- paste(wa_se, owner_id, id[i], query_form, "äàëüíîáîéùèê", own, counting, 100, offsetting, 0, ext, fields, vers, "&access_token=", token, sep = "")
            wall_media <- fromJSON(zapros)
            if(length(wall_media$response$count) > 0) {
                  dat <- data.frame(media_id = wall_media$response$items$from_id,
                                    id = wall_media$response$items$id,
                                    date = wall_media$response$items$date,
                                    comments = wall_media$response$items$comments$count,
                                    likes = wall_media$response$items$likes$count,
                                    reposts = wall_media$response$items$reposts$count,
                                    text = wall_media$response$items$text)
                  dat$text <- as.character(dat$text)
                  dat$text <- str_trim(dat$text)
                  if ("text" %in% names(wall_media$response$items) == TRUE) {
                        for (l in 1:nrow(dat)) {
                        grep_vec <- dat[l, "text", drop = TRUE]
                              if(grepl(query_grep, grep_vec, "text", ignore.case = TRUE) == TRUE) {
                              dat[l, "grep"] <- 1
                              } else {dat[l, "grep"] <- 0}
                        }
                  } else {yyy <- yyy+1}
            } else {xxx <- xxx+1}
            media_data <- rbind(media_data, dat)
            assign("media_data", media_data, envir = globalenv())
      }
}





