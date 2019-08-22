#source can be used easily when the file is in the main directory
#or use file.choose() inside source to manually put the file. This can be commented out after running once.
source('PModelDeterministic.R')

###Sensitivity analysis!
grab.names <- function(ode.data){
  just.top <- ode.data[,1:dim(ode.data)[2]]
  jt.df <- data.frame(just.top)
  nms <- names(jt.df)
  return(nms)
}
# 
 
 
NSC <- function(response.out,original.out,delta){
  NSC.val <-  ((response.out-original.out)/original.out)/delta
  return(NSC.val)
}


Sensitivity.bal <- function(up,down,par,orig,delta,ind.CA){
  change = par[[up]] * delta #Amount changed in the object being analyzed
  par[[up]] = par[[up]]+change #Changing the parameter
  par[[down]] = par[[down]]- change #Changing the paired parameter
  
  #Running the model on the new parameters along with sensitivity analysis
  response.out = run_model(par)[,ind.CA]
  sens <- NSC(response.out,orig,delta)
  
  return(sens)
  
}


Sensitivity.slowandrich <- function(par,orig,delta=.01,ind.CA){
  df.this <- data.frame(replicate(4,rep(0,241))) 
  #Gets the indices for richly and sparsely perfused 
  ind.vr <- which(names(par)=="VRC")
  ind.vs <- which(names(par)=="VSC")
  ind.qr <- which(names(par)=="QRC")
  ind.qs <- which(names(par)=="QSC")
  
  df.this[,1] = Sensitivity.bal(ind.vr,ind.vs,par=par,orig=orig,delta=delta,ind.CA = ind.CA)
  df.this[,2] = Sensitivity.bal(ind.vs,ind.vr,par=par,orig=orig,delta=delta,ind.CA = ind.CA)
  df.this[,3] = Sensitivity.bal(ind.qr,ind.qs,par=par,orig=orig,delta=delta,ind.CA = ind.CA)
  df.this[,4] = Sensitivity.bal(ind.qs,ind.qr,par=par,orig=orig,delta=delta,ind.CA = ind.CA)
  
  names(df.this) = c("VR","VS","QR","QS")
  return(df.this)
}


Calc.NSC <- function(string,par,orig,delta,ind.CA){
  ind.par <- which(names(par)==string)
  par[[ind.par]] = par[[ind.par]]*(1 + delta)
  response.out = run_model(par)[,ind.CA]
  sens <- NSC(response.out,orig,delta)
  return(sens)
}



#All this function takes is the parameters necessary for the pbpk model
Sensitivity.general <- function(par = assign_parameters(), delta= .01 ){
  ##Used previously instead of names
  #num.par <- length(par) #length counts the number of parameters
  sensitive <- names(parameters)
  sensitive <- setdiff(sensitive,c("VR","VS","QRC","QSC","CINT","tstart","TSTOP"))
  
  original.out.all <- run_model(par) #The concentration data in this will be used as "baseline"
  
  df.vals <- data.frame(replicate(length(sensitive)+4,rep(0,241))) #initializes the data frame
  
  #Might want to wrap this in a function for easier to read code
  names.of.output <- grab.names(original.out.all)
  ind.CA <- which(names.of.output=="CA")
  original.out <- original.out.all[,ind.CA]
  
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = length(sensitive), width = 300) #This will be a progress bar
  
  j=1
  for(i in sensitive){
    df.vals[,j] <- Calc.NSC(i,par,original.out,delta,ind.CA)
    setWinProgressBar(pb, j, title=paste( round(j/length(sensitive)*100, 0),
                                                   "% done"))
    j=j+1
  }
  ind.l = length(sensitive)+1
  ind.u = length(sensitive)+4
  df.vals[,ind.l:ind.u] <- Sensitivity.slowandrich(par,original.out,delta,ind.CA)
  
  #naming the sensitivity parameters
  names(df.vals) <- c(sensitive,"VRC","VSC","QRC","QSC")
  close(pb)
  
   return(df.vals[-1,])
}
sens<- Sensitivity.general()
max.sensitivity.each <- lapply(sens,max)