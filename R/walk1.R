walk1<-function(i,z){  ## Go through one by one
  #da<-convertDF(z[[i]])
  da<-as.data.frame(unlist(z[[i]]))
  geom_polygon(data=da,aes(x=da[[1]],y=da[[2]]))
}