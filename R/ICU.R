ICU<-setRefClass("ICU",
  fields=list(land="character",
              date="character",
              mode="character",
              #find="character",
              #language="character",
              guo="list"),               
  
   
  
  methods=list( 
     
    
    initialize=function(land="character",date="character",mode="character",guo="list"){
      options(stringsAsFactors = FALSE) # Turn off automatical conversion of text factoring
      require(httr)
      require(jsonlite)
      require(ggplot2)
      
      info<-rawData(land,"info",date)$description     ## Info
      enti<-as.data.frame(rawData(land,"data",date))  ## Data(id+name)
      if(mode=="geo")
      {gj<-rawData(land,"geo",date)                   ## GEO(id+coordinates)
      
      }
      #rawData(land,"geo",date)
      #if(mode=="svg)
      
      guo<<-list(Description=info,Entities=enti,Countries=gj)
    
     },
  
  
  
    visa=function(aim){
      if(!is.character(aim)) stop("The input should be a character!")
      
      
       z<-x$guo$Countries$coordinates
       enti<-x$guo$Entities
       gj<-x$guo$Countries
       #id<-enti[grep(aim,enti$name,ignore.case=TRUE),]$id
       i<-grep(aim,enti$name,ignore.case=TRUE)
       #i<-which(gj$id==as.numeric(id))
       
       if(length(i)==0) stop("Unfortunately there is no data for this entity yet...")
       else if(length(i)>1) stop(paste("Sorry! It seems there is more than one ",aim,sep="")) 
       else
       {len<-length(z)
       g<-ggplot()
       #for(i in 1:len)
       #cat(i)
        {if(is.list(z[[i]])&length(z[[i]])>1)
             {num<-length(z[[i]])
              for(j in 1:num)
               {if(is.list(z[[i]][[j]])&length(z[[i]][[j]])>1)
                    {te<-length(z[[i]][[j]])
                     for(k in 1:te) g<-g+walk3(i,j,k,z)}
                else g<-g+walk2(i,j,z)}}
         else g<-g+walk1(i,z)
               
         }}
       
       g
  },
  
  
    leta=function(){
    
  },
  
    summary=function(){
    ## No entities
  }
  

))