###
### Claire Kelling
### 
### Multiple Imputation Project
###
### SVM Functions for Multiple Imputation
### Mostly derived from code for Spencer Roger's Masters Thesis:
###    https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=4214&context=etd
### 
### Created: 11/9/18
### Last Updated: 11/9/18
###


SVMI<- function(data,categ.vars,modlist,max.iter=100,min.tol=1e-4) {
  #categ.vars is a vector indicating the column numbers of the categorical variables
  #modlist should be a list containing the prefitted SVM models for each of the categ.vars
  nomiss<- which(apply(1*(is.na(data)),2,sum)==0)
  cts.vars<- c(1:ncol(data))[-c(categ.vars,nomiss)]
  cts.mat<- NULL
  for(i in 1:length(cts.vars)){
    cts.mat<- rbind(cts.mat,cbind(t(combn(cts.vars,i)),matrix(0,
                                                              ncol=length(cts.vars)-i,nrow=choose(length(cts.vars),i))))
  }
  # this is a subroutine that will be called later when performing the cts imputation
  hat.fun<- function(datmat,cts.list=cts.mat){
    datmat[,categ.vars]<- apply(as.matrix(datmat[,categ.vars]),2,factor)
    hat.list<- list()
    for(i in 1:nrow(cts.list)){
      X<- suppressWarnings(model.matrix(~.,data=datmat[,-cts.list[i,]]))
      y<- as.matrix(datmat[rownames(X),cts.list[i,]])
      hat.list[[i]]<- X%*%(solve(t(X)%*%X)%*%t(X)%*%y)
    }
    return(hat.list)
  }
  #The following function performs the continuous imputation step on one row
  replacefun<- function(vec,hat.mat){
    # Performs multivariate imputation on a single row of the data matrix
    rownum<- vec[1]
    vec<- vec[-1]
    nposs<- ncol(cts.mat)
    nnmiss<- length(which(is.na(vec)))
    misspot<- c(which(is.na(vec)),rep(0,nposs-nnmiss))
    hatelem<- vecfind(cts.mat,misspot)
    vec[misspot]<- hat.mat[[hatelem]][rownum,]
    return(vec)
  }
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                     asNumeric))
  
  ###### First We impute missing values where only one value on
  ###### an observation is missing and it is categorical
  oneinds<- which(apply(1*(is.na(data)),1,sum)==1)
  onemiss<- data[oneinds,]
  newinds<- as.numeric(rownames(na.omit(onemiss[,-categ.vars])))
  
  #we do not need to do multivariate imputation for amp.mar
  plusone <- which(apply(1*(is.na(data)),1,sum) > 1)
  
  #FIXING A TYPO IN THE CODE, this should be a new subset of onemiss, not data
  onemiss<- onemiss[which(rownames(onemiss) %in% newinds),]
  
  nmiss<- apply(is.na(onemiss)[,categ.vars],2,sum)
  na_count <-sapply(onemiss, function(y) sum(length(which(is.na(y)))))
  for(i in 1:length(categ.vars)){
    #i <- 5
    print(i)
    if(nmiss[i]>0){
      missvar<- onemiss[which(is.na(onemiss[,categ.vars[i]])),]
      newdata=missvar[,-categ.vars[i]]
      
      newdata$HRC.FT <- as.numeric(as.character(newdata$HRC.FT))
      newdata$DJT.FT <- as.numeric(as.character(newdata$DJT.FT))
      
      #convert to numeric for fill in
      data[,categ.vars[i]] <- as.numeric(as.character(data[,categ.vars[i]]))
      
      data[which(rownames(data) %in% rownames(missvar)),categ.vars[i]] <- as.numeric(
        as.character(predict(modlist[[i]], newdata=newdata)))
      
      #convert back to factor after fill in
      data[,categ.vars[i]] <- as.factor(data[,categ.vars[i]])
    }
  }
  
  #No need for multivariate imputation in this case
  plusone <- which(apply(1*(is.na(data)),1,sum) > 1)
  
  # ########### Filling in the missing values with an initial guess
  # # Convert to numeric
  # data <- factorsNumeric(data)
  # #data[,categ.vars] <- as.numeric(as.character(data[,categ.vars]))
  # mu0<- apply(na.omit(data),2,median)
  # 
  # mu0[categ.vars]<- as.numeric(apply(na.omit(data[,categ.vars]),2,median))
  # 
  # misscols<- apply(is.na(data),2,sum)
  # misscols<- which(misscols>0)
  # pfilled<- data
  # for(i in misscols){
  #   pfilled[which(is.na(pfilled[,i])),i]<- mu0[i]
  # }
  # 
  # # Convert back to factor
  # data[categ.vars] <- lapply(data[categ.vars], factor)
  # pfilled[categ.vars] <- lapply(pfilled[categ.vars], factor)
  # 
  # for(i in 1:length(categ.vars)){
  #   #convert to numeric for fill in
  #   pfilled[,categ.vars[i]] <- as.numeric(as.character(pfilled[,categ.vars[i]]))
  #   
  #   pfilled[which(is.na(data[,categ.vars[i]])),categ.vars[i]]<-
  #     as.numeric(as.character(predict(modlist[[i]],newdata=
  #                                       pfilled[which(is.na(data[,categ.vars[i]])),-categ.vars[i]])))
  #   
  #   #convert back to factor after fill in
  #   data[,categ.vars[i]] <- as.factor(data[,categ.vars[i]])
  # }
  # prefilled<- pfilled
  # pfilled[,-categ.vars]<- data[,-categ.vars]
  # ########## Perform the initial imputation #############
  # cleanind<- which(apply(1*!is.na(pfilled),1,prod)==1)
  # clean<- pfilled[cleanind,] # returns the rows that have no missing values
  # dirtind<- which(apply(1*!is.na(pfilled),1,prod)==0)
  # dirty<- pfilled[dirtind,] # returns the rows with missing values
  # hat.mat0<- hat.fun(datmat=prefilled)
  # filled<- t(apply(cbind(as.numeric(rownames(dirty)),dirty),MARGIN=1,
  #                  FUN=replacefun,hat.mat=hat.mat0))
  # # recombines the imputed rows with the clean rows
  # juntos<- rbind(clean,filled)[order(as.numeric(rownames(rbind(clean,filled)))),]
  # iter<- 1
  # tol<- 1
  # tols<- c(1000)
  # ##### Enter the loop where imputation will be performed iteratively ####
  # while((iter<max.iter) & (tol>min.tol)) {
  #   indys<- sample(length(categ.vars),size=length(categ.vars))
  #   for(i in indys){
  #     juntos[which(is.na(data[,categ.vars[i]])),categ.vars[i]]<-
  #       as.numeric(as.character(predict(modlist[[i]],newdata=
  #                                         as.matrix(juntos[which(is.na(data[,categ.vars[i]])),-categ.vars[i]]))))
  #   }
  #   clean<- juntos[cleanind,]
  #   dirty[,categ.vars]<- juntos[dirtind,categ.vars]
  #   hat.mat0<- hat.fun(datmat=juntos)
  #   fillednew<- t(apply(cbind(as.numeric(rownames(dirty)),dirty),MARGIN=1,
  #                       FUN=replacefun,hat.mat=hat.mat0))
  #   juntosnew<- rbind(clean,fillednew)[order(as.numeric(rownames(rbind(clean,fillednew)))),]
  #   sds<- apply(juntos[,-categ.vars],2,sd)
  #   comp.cts<- t(t(juntos[,-categ.vars]-juntosnew[,-categ.vars])/sds)
  #   comp.categ<- 1*(juntos[,categ.vars]!=juntosnew[,categ.vars])
  #   tol<- sum(abs(cbind(comp.cts,comp.categ)))
  #   filled<- fillednew
  #   juntosold<- juntos
  #   juntos<- juntosnew
  #   iter=iter+1
  #   tols<- c(tols,tol)
  # }
  # if(iter==max.iter){
  #   cat("The algorithm failed to converge in",iter,"iterations\n")
  # } else {cat("The algorithm converged in",iter,"iterations\n")}
  # out<- list(newdata=juntos,iterations=iter,tolerance=tols)
  return(data)
}


SVMI_all <- function(data,categ.vars,modlist,max.iter=100,min.tol=1e-4) {
  #categ.vars is a vector indicating the column numbers of the categorical variables
  #modlist should be a list containing the prefitted SVM models for each of the categ.vars
  nomiss<- which(apply(1*(is.na(data)),2,sum)==0)
  cts.vars<- c(1:ncol(data))[-c(categ.vars,nomiss)]
  cts.mat<- NULL
  for(i in 1:length(cts.vars)){
    cts.mat<- rbind(cts.mat,cbind(t(combn(cts.vars,i)),matrix(0,
                                                              ncol=length(cts.vars)-i,nrow=choose(length(cts.vars),i))))
  }
  # this is a subroutine that will be called later when performing the cts imputation
  hat.fun<- function(datmat,cts.list=cts.mat){
    datmat[,categ.vars]<- apply(as.matrix(datmat[,categ.vars]),2,factor)
    hat.list<- list()
    for(i in 1:nrow(cts.list)){
      X<- suppressWarnings(model.matrix(~.,data=datmat[,-cts.list[i,]]))
      y<- as.matrix(datmat[rownames(X),cts.list[i,]])
      hat.list[[i]]<- X%*%(solve(t(X)%*%X)%*%t(X)%*%y)
    }
    return(hat.list)
  }
  #The following function performs the continuous imputation step on one row
  replacefun<- function(vec,hat.mat){
    # Performs multivariate imputation on a single row of the data matrix
    rownum<- vec[1]
    vec<- vec[-1]
    nposs<- ncol(cts.mat)
    nnmiss<- length(which(is.na(vec)))
    misspot<- c(which(is.na(vec)),rep(0,nposs-nnmiss))
    hatelem<- vecfind(cts.mat,misspot)
    vec[misspot]<- hat.mat[[hatelem]][rownum,]
    return(vec)
  }
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                     asNumeric))
  
  ###### First We impute missing values where only one value on
  ###### an observation is missing and it is categorical
  oneinds<- which(apply(1*(is.na(data)),1,sum)==1)
  onemiss<- data[oneinds,]
  #newinds<- as.numeric(rownames(na.omit(onemiss[,-categ.vars])))
  
  #we do not need to do multivariate imputation for amp.mar
  plusone <- which(apply(1*(is.na(data)),1,sum) > 1)
  
  #FIXING A TYPO IN THE CODE, this should be a new subset of onemiss, not data
  #onemiss<- onemiss[which(rownames(onemiss) %in% newinds),]
  
  nmiss<- apply(is.na(onemiss),2,sum)
  na_count <-sapply(onemiss, function(y) sum(length(which(is.na(y)))))
  for(i in 1:ncol(data)){
    #i <- 5
    print(i)
    if(nmiss[i]>0){
      missvar <- onemiss[which(is.na(onemiss[,i])),]
      newdata=missvar[,-i]
      
      if(i != 5){
        newdata$HRC.FT <- as.numeric(as.character(newdata$HRC.FT))
      }
      if(i != 6){
        newdata$DJT.FT <- as.numeric(as.character(newdata$DJT.FT))
      }
      
      #convert to numeric for fill in
      data[,i] <- as.numeric(as.character(data[,i]))
      
      data[which(rownames(data) %in% rownames(missvar)),i] <- as.numeric(
        as.character(predict(modlist.all[[i]], newdata=newdata)))
      
      if(i %in% categ.vars){
        #convert back to factor after fill in
        data[,i] <- as.factor(data[,i])
      }
    }
  }
  
  #No need for multivariate imputation in this case
  plusone <- which(apply(1*(is.na(data)),1,sum) > 1)
  any0 <- which(apply(1*(is.na(data)),1,sum) == 1)
  
  # ########### Filling in the missing values with an initial guess
  # # Convert to numeric
  # data <- factorsNumeric(data)
  # #data[,categ.vars] <- as.numeric(as.character(data[,categ.vars]))
  # mu0<- apply(na.omit(data),2,median)
  # 
  # mu0[categ.vars]<- as.numeric(apply(na.omit(data[,categ.vars]),2,median))
  # 
  # misscols<- apply(is.na(data),2,sum)
  # misscols<- which(misscols>0)
  # pfilled<- data
  # for(i in misscols){
  #   pfilled[which(is.na(pfilled[,i])),i]<- mu0[i]
  # }
  # 
  # # Convert back to factor
  # data[categ.vars] <- lapply(data[categ.vars], factor)
  # pfilled[categ.vars] <- lapply(pfilled[categ.vars], factor)
  # 
  # for(i in 1:length(categ.vars)){
  #   #convert to numeric for fill in
  #   pfilled[,categ.vars[i]] <- as.numeric(as.character(pfilled[,categ.vars[i]]))
  #   
  #   pfilled[which(is.na(data[,categ.vars[i]])),categ.vars[i]]<-
  #     as.numeric(as.character(predict(modlist[[i]],newdata=
  #                                       pfilled[which(is.na(data[,categ.vars[i]])),-categ.vars[i]])))
  #   
  #   #convert back to factor after fill in
  #   data[,categ.vars[i]] <- as.factor(data[,categ.vars[i]])
  # }
  # prefilled<- pfilled
  # pfilled[,-categ.vars]<- data[,-categ.vars]
  # ########## Perform the initial imputation #############
  # cleanind<- which(apply(1*!is.na(pfilled),1,prod)==1)
  # clean<- pfilled[cleanind,] # returns the rows that have no missing values
  # dirtind<- which(apply(1*!is.na(pfilled),1,prod)==0)
  # dirty<- pfilled[dirtind,] # returns the rows with missing values
  # hat.mat0<- hat.fun(datmat=prefilled)
  # filled<- t(apply(cbind(as.numeric(rownames(dirty)),dirty),MARGIN=1,
  #                  FUN=replacefun,hat.mat=hat.mat0))
  # # recombines the imputed rows with the clean rows
  # juntos<- rbind(clean,filled)[order(as.numeric(rownames(rbind(clean,filled)))),]
  # iter<- 1
  # tol<- 1
  # tols<- c(1000)
  # ##### Enter the loop where imputation will be performed iteratively ####
  # while((iter<max.iter) & (tol>min.tol)) {
  #   indys<- sample(length(categ.vars),size=length(categ.vars))
  #   for(i in indys){
  #     juntos[which(is.na(data[,categ.vars[i]])),categ.vars[i]]<-
  #       as.numeric(as.character(predict(modlist[[i]],newdata=
  #                                         as.matrix(juntos[which(is.na(data[,categ.vars[i]])),-categ.vars[i]]))))
  #   }
  #   clean<- juntos[cleanind,]
  #   dirty[,categ.vars]<- juntos[dirtind,categ.vars]
  #   hat.mat0<- hat.fun(datmat=juntos)
  #   fillednew<- t(apply(cbind(as.numeric(rownames(dirty)),dirty),MARGIN=1,
  #                       FUN=replacefun,hat.mat=hat.mat0))
  #   juntosnew<- rbind(clean,fillednew)[order(as.numeric(rownames(rbind(clean,fillednew)))),]
  #   sds<- apply(juntos[,-categ.vars],2,sd)
  #   comp.cts<- t(t(juntos[,-categ.vars]-juntosnew[,-categ.vars])/sds)
  #   comp.categ<- 1*(juntos[,categ.vars]!=juntosnew[,categ.vars])
  #   tol<- sum(abs(cbind(comp.cts,comp.categ)))
  #   filled<- fillednew
  #   juntosold<- juntos
  #   juntos<- juntosnew
  #   iter=iter+1
  #   tols<- c(tols,tol)
  # }
  # if(iter==max.iter){
  #   cat("The algorithm failed to converge in",iter,"iterations\n")
  # } else {cat("The algorithm converged in",iter,"iterations\n")}
  # out<- list(newdata=juntos,iterations=iter,tolerance=tols)
  return(data)
}