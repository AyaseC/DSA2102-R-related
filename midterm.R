library(readxl)
bra <- read_excel("D:/DSA2101/data/BRA-BEL-FIFA-2018.xlsx", sheet=1)
bel <- read_excel("D:/DSA2101/data/BRA-BEL-FIFA-2018.xlsx", sheet=2)
fra <- read_excel("D:/DSA2101/data/FRA-ARG-FIFA-2018.xlsx", sheet=1)
arg <- read_excel("D:/DSA2101/data/FRA-ARG-FIFA-2018.xlsx", sheet=2)
passes <- function(x){
  no <- which(!is.na(x))
  return (x[,no])
}
rpass_seq <- function(init_player, n_passes, pass_matrix){
  x <- c()
  number <- init_player
  x <- c(x, number)
  for (i in 1:n_passes){
    n <- passes(pass_matrix[pass_matrix[,2]==number,4:14])
    number <- sample(colnames(n),size=1,replace =TRUE,prob = n)
    x <- c(x,number)
  }
  return(x)
}
times <- c()
for(i in 1:1000){
  final <- rpass_seq(init_player = "1", n_passes= 5, bra)[6]
  times <- c(times,final)
}
barplot(table(times))



find <- function(x,y){
  if(x %in% y$gk){
    return("gk")
  }
  if(x %in% y$def){
    return("def")
  }
  if(x %in% y$mid){
    return("mid")
  }
  else{
    return("fwd")
  }
}
bra_pos <- list(gk=1, def=c(2,3,12,22), mid=c(11, 15,17,19),
                fwd=c(9, 10))
bel_pos <- list(gk=1, def=c(2,4,5,15), mid=c(6,7,8,22),
                fwd=c(9, 10))
fra_pos <- list(gk=1, def=c(2,4,5,21), mid=c(6, 13, 14),
                fwd=c(7, 9, 10))
arg_pos <- list(gk=12, def=c(2,3,14,16,17), mid=c(7,11,15,22),
                fwd=c(10))


argma <- matrix(0,nrow = 4,ncol = 4)
rownames(argma) <- c("gk","def","mid","fwd")
colnames(argma) <- c("gk","def","mid","fwd")
for (row in rownames(arg)) {
  from <- find(arg[row,2],arg_pos)
  ps <- passes(arg[row,4:14])
  for (co in colnames(ps)){
    receive <- find(as.numeric(co),arg_pos)
    argma[from,receive] <- argma[from,receive]+as.numeric(ps[1,co])
  }
}

brama <- matrix(0,nrow = 4,ncol = 4)
rownames(brama) <- c("gk","def","mid","fwd")
colnames(brama) <- c("gk","def","mid","fwd")
for (row in rownames(bra)) {
  from <- find(bra[row,2],bra_pos)
  ps <- passes(bra[row,4:14])
  for (co in colnames(ps)){
    receive <- find(as.numeric(co),bra_pos)
    brama[from,receive] <- brama[from,receive]+as.numeric(ps[1,co])
  }
}

belma <- matrix(0,nrow = 4,ncol = 4)
rownames(belma) <- c("gk","def","mid","fwd")
colnames(belma) <- c("gk","def","mid","fwd")
for (row in rownames(bel)) {
  from <- find(bel[row,2],bel_pos)
  ps <- passes(bel[row,4:14])
  for (co in colnames(ps)){
    receive <- find(as.numeric(co),bel_pos)
    belma[from,receive] <- belma[from,receive]+as.numeric(ps[1,co])
  }
}

frama <- matrix(0,nrow = 4,ncol = 4)
rownames(frama) <- c("gk","def","mid","fwd")
colnames(frama) <- c("gk","def","mid","fwd")
for (row in rownames(fra)) {
  from <- find(fra[row,2],fra_pos)
  ps <- passes(fra[row,4:14])
  for (co in colnames(ps)){
    receive <- find(as.numeric(co),fra_pos)
    frama[from,receive] <- frama[from,receive]+as.numeric(ps[1,co])
  }
}
q1_matrices <- list(brama,belma,frama,argma)



library(rvest)

webpage <- read_html("https://en.wikipedia.org/wiki/2018_FIFA_World_Cup")

tbls <- html_nodes(webpage, "table")

head(tbls)
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[85] %>%
  html_table(fill = TRUE)
q1_money <- as.data.frame(tbls_ls)



cdnow <- read.csv("D:/DSA2101/data/CDNOW2.csv")
library(tidyverse)
library(dplyr)

q2_consec <- cdnow %>% 
  group_by(cid, grp = cumsum(c(1, diff(week_num) != 1))) %>% 
  filter(n() >= 2)
q2_consec



cdnow1<- cdnow %>%
  group_by(cid) %>%
  mutate(Date=as.Date("1994-01-03")+7*(week_num-1))
cdnow1

library(zoo)
week <- group_by(cdnow,week_num)%>%
  summarise(Quantity=sum(qty))
week
week_12 <- week[1:12,]%>%
  mutate(avg = rollmean(Quantity, 5, fill = NA))
week_66 <- week[13:78,]%>%
  mutate(avg = rollmean(Quantity, 5, fill = NA))
plot(week)
lines(x=week$week_num,y=week$Quantity)
lines(x = week_12$week_num,y = week_12$avg,col = "red")
lines(x = week_66$week_num,y = week_66$avg,col = "red")

a <- group_by(cdnow,cid)
fiveormore <- five <- distinct(filter(a,length(week_num)>=5),cid)
five <- distinct(filter(a,length(week_num)==5),cid)
more <- distinct(filter(a,length(week_num)>5),cid)

By comparing the number of customers who made 5 purchases(once per week) and stopped
(969), to the number of customers who made more than 5 purchases(2701), we can conclude 
that this customer is very likely to make the 6th purchase. The percentage of customer 
who made more than 5 purchases in the total number of customers who made 5 or more purchases
is 2701/2670=73.6%.