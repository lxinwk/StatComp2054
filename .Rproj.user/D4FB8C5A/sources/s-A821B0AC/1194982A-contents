
#' @title knn Class to predict
#' @description Use Knn to predict the category a point belongs to
#' @param train_data 
#' @param obse 
#' @param type 
#' @param calcols 
#' @param classcol
#' @param idcol
#' @param k
#' @return a random sample of size
#' @export
knn_classi<-function(train_data,obse,type,calcols,classcol,idcol,k)
{
  distance<-function(train_data,obse,type)
  {
    r<-nrow(train_data)
    near_distance<-matrix(nrow=r,ncol=1)
    for(i in 1:r){
      near_distance[i]<-norm(cbind(train_data[i,]-obse),type)#zhao		
    }
    near_distance
  }
  knn<-function(train_data,obse,type,calcols,classcol,idcol)
  {
    d<-distance(train_data[,calcols],obse,type)
    m <- data.frame(train_data[,idcol],train_data[,classcol],d)
    m
  }
  all_near<-knn(train_data,obse,type,calcols,classcol,idcol)
  all_near<-all_near[order(all_near[,3]),]
  r<-nrow(all_near)
  cla<-unique(all_near[,2])
  probs<-matrix(cla,ncol=2,nrow=length(cla),byrow=FALSE)
  k_nearest<-all_near[1:k,]
  freqs<-as.data.frame(table(k_nearest[,2]))
  freqs[,2]<-freqs[,2]/k
  colnames(freqs)<-c("class","probs")
  freqs<-freqs[order(freqs[,2],decreasing = TRUE),]
  re <- freqs[1,]
  re
}
NULL