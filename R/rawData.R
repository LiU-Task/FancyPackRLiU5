rawData<-function(land,module,date,language="") {
  
  url<-"http://api.thenmap.net"
  path<-paste("v2/",land,"/",module,"/",date,"/",language,sep="")
  
  if(module=="info" | module=="data")
    raw.data=fromJSON(paste(url,"/",path,sep="")) ## Structured data
  
  else if(module=="geo")
   {da<-fromJSON(paste(url,"/",path,sep=""))$features
    ta1<-da$properties$id   
    ta2<-da$geometry$coordinates
    ta3<-da$geometry$type
    len<-length(ta2)  ## NO entities
    for(i in 1:len)  
        {if(ta3[i]=="Polygon")
          {dim(ta2[[i]])<-c(ji(dim(ta2[[i]]))/2,2)
           colnames(ta2[[i]])=c("Longitude","Latitude")}
         
         else if (ta3[i]=="MultiPolygon")
              
           if(is.array(ta2[[i]]))
           {num<-dim(ta2[[i]])[1]       ## NO areas of entity
            shen<-length(dim(ta2[[i]])) ## weidu
            co<-list()
            for(j in 1:num) {if(shen==3) {jing<-ta2[[i]][j,,1];wei<-ta2[[i]][j,,2]}
                             else        {jing<-ta2[[i]][j,,,1];wei<-ta2[[i]][j,,,2]}
                             tem=cbind(jing,wei);colnames(tem)=c("Longitude","Latitude")
                             co<-append(co,tem)}
           
            ta2[[i]]<-co}
           
           else if(is.list(ta2[[i]]))
             {num<-length(ta2[[i]])     ## NO areas of entity
              #co<-list()
              for(j in 1:num) 
              {if(is.list(ta2[[i]][[j]]))
                          {te<-length(ta2[[i]][[j]])
                           for(k in 1:te) 
                               if(is.list(ta2[[i]][[j]][[k]])) 
                                    {z<-ta2[[i]][[j]][[k]]
                                     z<-as.data.frame(unlist(z))
                                     z<-data.frame(Longitude=z[[1]],Latitude=z[[2]])
                                     ta2[[i]][[j]][[k]]<-z
                                     colnames(ta2[[i]][[j]][[k]])=c("Longitude","Latitude")}}
               else
               {dim(ta2[[i]][[j]])<-c(ji(dim(ta2[[i]][[j]]))/2,2)
                colnames(ta2[[i]][[j]])=c("Longitude","Latitude")}
               }}

           }       
    raw.data=list(id=ta1,coordinates=ta2)}
  
  #raw.data<-GET(url=url,path=path)
 
  return (raw.data)
}


