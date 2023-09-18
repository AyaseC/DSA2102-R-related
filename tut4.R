library(jsonlite)
fname <- "../data/restaurants_dataset.json"
rest_lines <- readLines(fname)
rest_json <- lapply(rest_lines, fromJSON)

reclass <- function(x){
  class(x) <- "restaurant"
  x
}
rest_class_list <- lapply(rest_json, reclass)


summary.restaurant <- function(x){
  cat(paste(x$name,"was graded", nrow(x$grades),"times."))
}
summary(rest_class_list[[10]])



library(sf)
coords<-sapply(rest_json, function(x) x$address$coord)



##coords<-matrix(unlist(coords),ncol=2)
names<-sapply(rest_json,function(x) x$name)
borough<-sapply(rest_json,function(x) x$borough)
cuisine<-sapply(rest_json, function(x) x$cuisine)
df1<-data.frame(cbind(names,t(coords)))
df2<-data.frame(cbind(df1,borough))
df3<-data.frame(cbind(df2,cuisine))
rest_sf<-st_as_sf(df3,coords = c("coordx","coordy"),crs=4326)
plot(st_geometry(rest_sf),axes=TRUE)
