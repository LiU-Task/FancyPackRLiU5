ji<-function(x){
  len<-length(x)
  re<-1
  for(i in 1:len) re<-re*x[i]
  return (re)
}