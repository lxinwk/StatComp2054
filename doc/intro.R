## ----eval=FALSE---------------------------------------------------------------
#  function(a,b){
#    n <- length(a)
#    e1 <- e2 <- e3 <- e4 <- matrix(0,n,n)
#    re <- numeric(4)
#    for (i in 1:n) {
#  
#      for (j in (i+1):n-1){
#        y <- a[-c(i,j)]
#        x <- b[-c(i,j)]
#  
#        J1 <- lm(y ~ x)
#        yhat1 <- J1$coef[1] + J1$coef[2] * b[c(i,j)]
#        e1[i,j] <- mean((a[c(i,j)] - yhat1)^2)
#  
#        J2 <- lm(y ~ x + I(x^2))
#        yhat2 <- J2$coef[1] + J2$coef[2] * b[c(i,j)] + J2$coef[3] *
#          b[c(i,j)]^2
#        e2[i,j] <- mean((a[c(i,j)] - yhat2)^2)
#  
#        J3 <- lm(log(y) ~ x)
#        logyhat3 <- J3$coef[1] + J3$coef[2] * b[c(i,j)]
#        yhat3 <- exp(logyhat3)
#        e3[i,j] <-mean(( a[c(i,j)] - yhat3)^2)
#  
#        J4 <- lm(log(y) ~ log(x))
#        logyhat4 <- J4$coef[1] + J4$coef[2] * log(b[c(i,j)])
#        yhat4 <- exp(logyhat4)
#        e4[i,j] <- mean((a[c(i,j)] - yhat4)^2)
#  
#      }
#  
#    }
#    re <- c(2*sum(e1)/(n*(n-1)),2*sum(e2)/(n*(n-1)),2*sum(e3)/(n*(n-1)),2*sum(e4)/(n*(n-1)))
#    re
#  }

## ----eval=FALSE---------------------------------------------------------------
#  attach(ironslag)
#  print(Two_Fold_Cross_Validation(chemical,magnetic))

## ----eval=FALSE---------------------------------------------------------------
#  function(train_data,obse,type)
#  {
#    r<-nrow(train_data)
#    near_distance<-matrix(nrow=r,ncol=1)
#    for(i in 1:r){
#      near_distance[i]<-norm(cbind(train_data[i,]-obse),type)
#    near_distance
#    }
#  }

## ----eval=FALSE---------------------------------------------------------------
#  function(train_data,obse,type,calcols,classcol,idcol)
#  {
#    d<-distance(train_data[,calcols],obse,type)
#    m <- data.frame(train_data[,idcol],train_data[,classcol],d)
#    m
#  }

## ----eval=FALSE---------------------------------------------------------------
#  function(train_data,obse,type,calcols,classcol,idcol,k)
#  {
#    distance<-function(train_data,obse,type)
#    {
#      r<-nrow(train_data)
#      near_distance<-matrix(nrow=r,ncol=1)
#      for(i in 1:r){
#        near_distance[i]<-norm(cbind(train_data[i,]-obse),type)#zhao		
#      }
#      near_distance
#    }
#    knn<-function(train_data,obse,type,calcols,classcol,idcol)
#    {
#      d<-distance(train_data[,calcols],obse,type)
#      m <- data.frame(train_data[,idcol],train_data[,classcol],d)
#      m
#    }
#    all_near<-knn(train_data,obse,type,calcols,classcol,idcol)
#    all_near<-all_near[order(all_near[,3]),]
#    r<-nrow(all_near)
#    cla<-unique(all_near[,2])
#    probs<-matrix(cla,ncol=2,nrow=length(cla),byrow=FALSE)
#    k_nearest<-all_near[1:k,]
#    freqs<-as.data.frame(table(k_nearest[,2]))
#    freqs[,2]<-freqs[,2]/k
#    colnames(freqs)<-c("class","probs")
#    freqs<-freqs[order(freqs[,2],decreasing = TRUE),]
#    re <- freqs[1,]
#    re
#  }

## ----eval=FALSE---------------------------------------------------------------
#  knn_classi(iris,c(11,22),'2',c(2:3),4,1,5)

