walk3<-function(i,j,k,z){  ## Go through one by one
  #da<-convertDF(z[[i]])
  da<-as.data.frame(unlist(z[[i]][[j]][[k]]))
  geom_polygon(data=da,aes(x=da[[1]],y=da[[2]]))
}