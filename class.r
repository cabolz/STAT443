## Function for predicting class posterior probabilities
## Code produced by GUIDE 36.2 on 2/11/21 at 10:28
predicted <- function(){
 catvalues <- c("CHF","MOSF w/Sepsis")
 if(cat1 %in% catvalues){
   if(is.na(meanbp1) | meanbp1 <= 68.5000000000 ){
     if(!is.na(pafi1) & pafi1 <= 266.156250000 ){
       nodeid <- 8
       predclass <- "RHC"
       posterior <- c( 0.30382E+00, 0.69618E+00)
     } else {
       catvalues <- c("No insurance","Private","Private & Medicare")
       if(ninsclas %in% catvalues){
         nodeid <- 18
         predclass <- "RHC"
         posterior <- c( 0.37295E+00, 0.62705E+00)
       } else {
         nodeid <- 19
         predclass <- "NoRHC"
         posterior <- c( 0.61468E+00, 0.38532E+00)
       }
     }
   } else {
     nodeid <- 5
     predclass <- "NoRHC"
     posterior <- c( 0.61837E+00, 0.38163E+00)
   }
 } else {
   if(!is.na(pafi1) & pafi1 <= 142.359375000 ){
     catvalues <- c("No")
     if(resp %in% catvalues){
       catvalues <- c("No")
       if(dnr1 %in% catvalues){
         catvalues <- c("ARF","Lung Cancer","MOSF w/Malignancy")
         if(cat1 %in% catvalues){
           nodeid <- 48
           predclass <- "RHC"
           posterior <- c( 0.34475E+00, 0.65525E+00)
         } else {
           nodeid <- 49
           predclass <- "NoRHC"
           posterior <- c( 0.64935E+00, 0.35065E+00)
         }
       } else {
         nodeid <- 25
         predclass <- "NoRHC"
         posterior <- c( 0.65152E+00, 0.34848E+00)
       }
     } else {
       catvalues <- c("Yes")
       if(seps %in% catvalues){
         nodeid <- 26
         predclass <- "RHC"
         posterior <- c( 0.36364E+00, 0.63636E+00)
       } else {
         nodeid <- 27
         predclass <- "NoRHC"
         posterior <- c( 0.63727E+00, 0.36273E+00)
       }
     }
   } else {
     nodeid <- 7
     predclass <- "NoRHC"
     posterior <- c( 0.76449E+00, 0.23551E+00)
   }
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## newdata.txt is the file containing the data to be predicted
## Missing value code is NA
newdata <- read.table("newdata.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## pred contains predicted posterior probabilities
node <- NULL
pred <- NULL
pred.class <- NULL
for(i in 1:nrow(newdata)){
    cat1 <- as.character(newdata$cat1[i])
    meanbp1 <- as.numeric(newdata$meanbp1[i])
    pafi1 <- as.numeric(newdata$pafi1[i])
    dnr1 <- as.character(newdata$dnr1[i])
    ninsclas <- as.character(newdata$ninsclas[i])
    resp <- as.character(newdata$resp[i])
    seps <- as.character(newdata$seps[i])
    tmp <- predicted()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    pred <- rbind(pred,as.numeric(tmp[-c(1,2)]))
}
