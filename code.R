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
## Read media list
setwd("D:/Research/VK/media_analysis")
media <- read.table("MEDIA_list.txt", sep = "", col.names = c("name", "num", "class"), colClasses = c("character", "character", "factor"))
id_s <- media[,2]
#################### KEYWORDS SEARCH ####################
# ÀÐÃÓÌÅÍÒÛ ÏÎÈÑÊÀ
## Media search
media_data <- data.frame()
##no resp
xxx <- 0
## no text
yyy <- 0
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
      ## formula if count > 100
      zapros <- NULL
      if(count > 100) {
            nt <- ceiling(count/100)
            offset = -100
            for(f in 1:nt) {
                  offset <- offset + 100
                  count = 100
                  for(i in 1:length(id)) {
                        zapros <- NULL
                        zapros <- paste(wa_se, owner_id, id[i], query_form, query, own, counting, count, offsetting, offset, ext, fields, vers, "&access_token=", token, sep = "")
                        wall_media <- fromJSON(zapros)
                        if(length(wall_media$response$count) > 0) {
                              dat <- data.frame(media_name = wall_media$response$items$from_id,
                                                media_id = wall_media$response$items$from_id,
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
                        assign("xxx", xxx, envir = globalenv())
                        assign("yyy", yyy, envir = globalenv())
                  }
            }
      }
      ## formula if count <= 100
      else {for(i in 1:length(id)) {
            zapros <- NULL
            zapros <- paste(wa_se, owner_id, id[i], query_form, query, own, counting, count, offsetting, offset, ext, fields, vers, "&access_token=", token, sep = "")
            wall_media <- fromJSON(zapros)
            if(length(wall_media$response$count) > 0) {
                  dat <- data.frame(media_name = wall_media$response$items$from_id,
                                    media_id = wall_media$response$items$from_id,
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
      }}
      media_data$date <- as.POSIXct(media_data$date, origin = "1970-01-01")
      media_data$grep <- factor(media_data$grep, labels = c("no","yes"))
      media_data$media_name <- factor(media_data$media_name, levels = media$num, labels = media$name)
      assign("media_data", media_data, envir = globalenv())
}

## MISC
media_data3 <- filter(media_data, grep == "yes")
## Plot
library(RColorBrewer)
col <- brewer.pal(4, "Set1")
pal <- colorRampPalette(col)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
with(media_data2, plot(date, likes, pch = 19, col = media_data2$media_name, xaxt = "n", yaxt = "n"))
axis.POSIXct(1, at=seq(as.Date(min(media_data2$date)), as.Date(max(media_data2$date)), by="10 day"), format="%m-%d")
axis(2, at=seq(min(media_data2$likes), max(media_data2$likes), by=1000), las=2)
legend("topright", inset=c(-0.52,0), pch=19, col= unique(media_data2$media_name), legend = unique(media_data2$media_name))
library(ggplot2)
qplot(date, likes, data = media_data3, color = media_data3$media_name, size = I(3), alpha = I(0.6), main = "Êîë-âî ëàéêîâ íîâîñòÿì ïðî äàëüíîáîéùèêîâ")
dev.copy(png, "daln.png", width = 550, height = 400)
dev.off()

media_data3$mn <- ddply(media_data3, .(media_name), summarize, mn = ave(likes, FUN = mean))[,2]
media_data3$sd <- ddply(media_data3, .(media_name), summarize, sd = ave(likes, FUN = sd))[,2]
media_data3 <- na.omit(media_data3)
media_data3 <- mutate(media_data3, sc = (likes - mn)/sd)

xyplot(sc~date | media_data3$media_name, data = media_data3, layout = c(3,2))

g <- ggplot(media_data3, aes(x=sc)) + geom_histogram(bins = 20) + facet_grid(.~media_name)
g
