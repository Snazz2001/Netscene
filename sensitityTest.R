install.packages('MCMCpack')
require(MCMCpack)
#library(scatterplot3d) 
#library(rgl)
rdirichlet(20,c(1,1,1))
a <- rdirichlet(20,c(1,1,1))
apply(a,1,sum)
a <- rdirichlet(1000,c(2,4,4))
a <- rdirichlet(1000,c(2,4,4))
hist(a[,1])
hist(a[,2])
hist(a[,3])
apply(a,1,sum)
a <- rdirichlet(1000,c(200,400,400))
hist(a[,3])
par(mfrow=c(1,2))
hist(a[,3])
a <- rdirichlet(1000,c(2,4,4))
hist(a[,3])
sd(a[,3])
a <- rdirichlet(1000,c(20,40,40))
sd(a[,3])

####The above code is used to familiar with dirichelt distribution

fit.net.z=function(nnodes,net){
  print('calling fit.net.z function')
  net_spec <- list()
  for(i in 1:length(nnodes)){
    net_spec[[nnodes[[i]]@name]]<-nnodes[[i]]@model$model
  }
  print('done fit.net.z!')
  cgfit <- custom.fit(net,dist=net_spec)  
}


require(methods)
require(bnlearn)
require(ggplot2)
###define two type of nodes, continous node and discrete node###
setClass("GRNode_c",
         representation(name="character",type="character",model="list",values="vector",parents="vector",children="vector"))
setClass("GRNode_d",
         representation(name="character",type="character",model="list",values="vector",parents="vector",children="vector"))

###add some method to here###
setGeneric("setValues",function(the_obj,values){
  standardGeneric("setValues")
})

setMethod(f="setValues",
          signature="GRNode_c",
          definition=function(the_obj,values){
            the_obj@values <- values
            return(the_obj)
          })

setMethod(f="setValues",
          signature="GRNode_d",
          definition=function(the_obj,values){
            the_obj@values <- values
            return(the_obj)
          })

setGeneric("getValues",function(the_obj){
  standardGeneric("getValues")
})

setMethod(f="getValues",
          signature="GRNode_c",
          definition=function(the_obj){
            return(the_obj@values)
          })

setMethod(f="getValues",
          signature="GRNode_d",
          definition=function(the_obj){
            return(the_obj@values)
          })

setGeneric("setType",function(the_obj,values){
  standardGeneric("setType")
})

setMethod(f="setType",
          signature="GRNode_c",
          definition=function(the_obj){
            the_obj@type <- "c" 
            return(the_obj)
          })

setMethod(f="setType",
          signature="GRNode_d",
          definition=function(the_obj){
            the_obj@type <- "d"
            return(the_obj)
          })

setGeneric("getType",function(the_obj,values){
  standardGeneric("getType")
})

setMethod(f="getType",
          signature="GRNode_c",
          definition=function(the_obj){
            return("c")
          })

setMethod(f="getType",
          signature="GRNode_d",
          definition=function(the_obj){
            return("d")
          })

setGeneric("setName",function(the_obj,name){
  standardGeneric("setName")
})

setMethod(f="setName",
          signature="GRNode_c",
          definition=function(the_obj,name){
            the_obj@name <- name
            return(the_obj)
          })

setMethod(f="setName",
          signature="GRNode_d",
          definition=function(the_obj,name){
            the_obj@name <- name
            return(the_obj)
          })

setGeneric("getName",function(the_obj){
  standardGeneric("getName")
})

setMethod(f="getName",
          signature="GRNode_c",
          definition=function(the_obj){
            return(the_obj@name)
          })

setMethod(f="getName",
          signature="GRNode_d",
          definition=function(the_obj){
            return(the_obj@name)
          })

setGeneric("getModel",function(the_obj){
  standardGeneric("getModel")
})

setMethod(f="getModel",
          signature="GRNode_c",
          definition=function(the_obj){
            return(the_obj@model)
          })

setMethod(f="getModel",
          signature="GRNode_d",
          definition=function(the_obj){
            return(the_obj@model)
          })

setGeneric("setModel",function(the_obj,model){
  standardGeneric("setModel")
})

setMethod(f="setModel",
          signature="GRNode_c",
          definition=function(the_obj,model){
            the_obj@model <- model
            return(the_obj)
          })

setMethod(f="setModel",
          signature="GRNode_d",
          definition=function(the_obj,model){
            the_obj@model[[1]] <- model
            return(the_obj)
          })

evi_list <- list() ## should be ok if i replace << with < 

result_bin<-list()

income_level <- c('0%','2.5%','5%')
inflation_level <- c('<1.5%','1.5%-2.5%','>2.5%')
boe_level <- c('0.5%','1.5%','2.5%')
spread_level <- c('1%','2%')#new add
hpi_level <- c('-10%','0%','10%')#5% 20% 75% LTV->44% 48% 55%

Income_1_model <- matrix(c(0.1,0.8,0.1),ncol=3,dimnames=list(NULL,'Income_1'=income_level))
#Inflation_1_model <- matrix(c(0.9,0.09,0.01,0.1,0.6,0.3,0.01,0.3,0.69),ncol=3,dimnames=list('Inflation_1'=inflation_level,'Income_1'=income_level))
Inflation_1_model <- matrix(c(0.5,0.4,0.1,0.2,0.4,0.4,0.01,0.39,0.6),ncol=3,dimnames=list('Inflation_1'=inflation_level,'Income_1'=income_level))
HPI_1_model <- matrix(c(0.05,0.20,0.75),ncol=3,dimnames=list(NULL,'HPI_1'=hpi_level))
#HPI_1_model <- matrix(c(0.000,0.20,0.80),ncol=3,dimnames=list(NULL,'HPI_1'=hpi_level))
#BoERates_1_model <- c(0.99,0.005,0.005,0.9,0.09,0.01,0.8,0.19,0.01,0.99,0.005,0.005,0.6,0.39,0.01,0.2,0.6,0.2,0.9,0.09,0.01,0.2,0.7,0.1,0.01,0.4,0.59)
BoERates_1_model <- c(0.82,0.18,0.00,0.62,0.37,0.01,0.36,0.59,0.05,0.36,0.59,0.05,0.16,0.69,0.15,0.05,0.59,0.36,0.05,0.59,0.36,0.01,0.37,0.62,0.00,0.18,0.82)
dim(BoERates_1_model) <- c(3,3,3)
dimnames(BoERates_1_model) <- list('BoERates_1'=boe_level,'Income_1'=income_level,'Inflation_1'=inflation_level)
Spread_1_model <- matrix(c(0.65,0.35),ncol=2,dimnames=list(NULL,'Spread_1'=spread_level))#new add

#LTV_1_model <- list(coef = c("(Intercept)" = 0.4813), sd = 0.2248)
LTV_1_model <- list(coef = matrix(c(0.66,1,0.6,1,0.54,1),ncol=3,dimnames=list(c("(Intercept)","HPI_1"),hpi_level)), sd = c(0.2,0.2,0.2))
#the below is make up number
ltv_level <-c('<20%','20%-30%','30%-40%','40%-50%','50%-60%','60%-70%','70%-75%','75%+')
#LTV_1_model <- matrix(c(0.02,0.05,0.09,0.14,0.19,0.2,0.19,0.12),ncol=8,dimnames=list(NULL,'LTV_1'=ltv_level))

dti_level <- c('<5%','5%-10%','10%-15%','15%-20%','20%-25%','25%+')
#DTI_1_model <- matrix(c(0.16,0.16,0.16,0.16,0.16,0.20,
#  					0.13,0.14,0.15,0.16,0.18,0.24,
#						0.01,0.02,0.04,0.09,0.20,0.64,
#						0.14,0.16,0.18,0.18,0.16,0.18,
#						0.16,0.18,0.14,0.18,0.16,0.18,
#						0.18,0.18,0.16,0.18,0.16,0.14,
#						0.64,0.20,0.09,0.04,0.02,0.01,
#						0.24,0.18,0.16,0.15,0.14,0.13,
#						0.20,0.16,0.16,0.16,0.16,0.16))

DTI_1_model <- matrix(c(0.10934005,0.212965337,0.26596152,0.212965337,0.10934005,0.089427706,
                        0.007597324,0.035993978,0.10934005,0.212965337,0.26596152,0.368141791,
                        0.000089220,0.001028186,0.007597324,0.035993978,0.10934005,0.845951242,
                        0.114010082,0.217410991,0.265827535,0.208400454,0.104755682,0.089595256,
                        0.009749629,0.043349392,0.123582843,0.225898624,0.264758077,0.332661435,
                        0.000166240,0.0017234,0.011455602,0.048823625,0.133420438,0.804410695,
                        0.118324906,0.22133932,0.265473518,0.204156902,0.100667008,0.090038346,
                        0.012135265,0.050930353,0.137051678,0.236467934,0.261601594,0.301813177,
                        0.000286379,0.002696578,0.016280421,0.063022926,0.156427004,0.761286694,
                        0.00759732,0.03599398,0.10934005,0.21296534,0.26596152,0.36814179,
                        0.00008922,0.00102819,0.00759732,0.03599398,0.10934005,0.84595124,
                        0.00000018,0.00000496,0.00008922,0.00102819,0.00759732,0.99128013,
                        0.00974963,0.04334939,0.12358284,0.22589862,0.26475808,0.33266144,
                        0.00016624,0.00172340,0.01145560,0.04882363,0.13342044,0.80441070,
                        0.00000057,0.00001366,0.00021173,0.00210401,0.01340593,0.98426410,
                        0.01213526,0.05093035,0.13705168,0.23646793,0.26160159,0.30181318,
                        0.00028638,0.00269658,0.01628042,0.06302293,0.15642700,0.76128669,
                        0.00000156,0.00003285,0.00044501,0.00386494,0.02152292,0.97413273
))
dim(DTI_1_model) <- c(6,3,3,2)
dimnames(DTI_1_model) <- list('DTI_1'=dti_level,'BoERates_1'=boe_level,'Income_1'=income_level,'Spread_1'=spread_level)##add Spread_1

##########
###current implementation####
Defaults_1_model <- list(coef=matrix(c(0.00,0.4,0.04,0.4,0.1,0.4,0.13,0.4,0.16,0.4,0.18,0.4),ncol=6,dimnames=list(c("(Intercept)",'LTV_1'),dti_level)),
                         sd=c(0.01,0.01,0.01,0.01,0.01,0.01))

#dtv_model <- list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
#                              dimnames = list(c("(Intercept)", "ltv"), NULL)),
#                sd = c(0.3, 0.6))

defaults_level <- c('1','0')


Income_1 <- new("GRNode_d",name="Income_1",type='d',model=list(model=Income_1_model),values=income_level,parents=c(NA,NA),children=c(NA,NA))
Inflation_1 <- new("GRNode_d",name="Inflation_1",type='d',model=list(model=Inflation_1_model),values=inflation_level,parents=c(NA,NA),children=c(NA,NA))
HPI_1 <- new("GRNode_d",name="HPI_1",type='d',model=list(model=HPI_1_model),values=hpi_level,parents=c(NA,NA),children=c(NA,NA))
BoERates_1 <- new("GRNode_d",name="BoERates_1",type='d',model=list(model=BoERates_1_model),values=boe_level,parents=c(NA,NA),children=c(NA,NA))
DTI_1 <- new("GRNode_d",name="DTI_1",type='d',model=list(model=DTI_1_model),values=dti_level,parents=c(NA,NA),children=c(NA,NA))
LTV_1 <- new("GRNode_c",name="LTV_1",type='c',model=list(model=LTV_1_model),values=ltv_level,parents=c(NA,NA),children=c(NA,NA))
Spread_1 <- new("GRNode_d",name="Spread_1",type='d',model=list(model=Spread_1_model),values=spread_level,parents=c(NA,NA),children=c(NA,NA))
Defaults_1 <- new("GRNode_c",name="Defaults_1",type='c',model=list(model=Defaults_1_model),values=defaults_level,parents=c(NA,NA),children=c(NA,NA))

networkstring <- "[HPI_1][LTV_1|HPI_1][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][Spread_1][DTI_1|Income_1:BoERates_1:Spread_1][Defaults_1|DTI_1:LTV_1]"
#networkstring <- "[ALTV][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][DTI_1|Income_1:BoERates_1][Defaults_1|DTI_1:ALTV]"
net <- model2network(networkstring)
#get.model(Income_1) just comment it out at 10:19 March 17

###Extract the node from the list
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
for(nd in nnodes){
  print(nd@type)
}

###Extract the value of the prob from the model
results <- matrix(rep(0,200),ncol=2)

BoERates_1@model[[1]] <- raw_value
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
cgfit <- fit.net.z(nnodes,net)
cgfit
ddd<-rbn(cgfit,n=2000)
default_mean <- mean(ddd$Defaults_1)
results[1,1] <- 0
results[1,2] <- default_mean

bak_model <- BoERates_1@model
class(bak_model)
value <- bak_model[[1]]
class(value)
dim(value)
raw_value <- value
value[,1,1]

###Test sensitivity of the perturbed distribution using dirichlet distributionn###
conf_values <- c(50,100,200,400,1000)
perturbed_values <- data.frame()
i <- 3
j <- 1
for(lambda in conf_values){
  results <- rdirichlet(2000,round(raw_value[,i,j]*lambda))
  results_df <- as.data.frame(results)    
  colnames(results_df) <- BoERates_1@values
  results_df$lambda <- lambda
  perturbed_values <- rbind(perturbed_values,results_df)
}
sd_states <- aggregate(bbb[,1:3],list(lambda=bbb$lambda),sd)
write.csv(sd_states,file='sd_state.csv',row.names=FALSE)

model_name <- 'HPI_1'#'BoERates_1'#'Inflation_1'##BoERates_1'#'HPI_1'#
model <- (eval(parse(text=model_name)))
#model <- with(nnodes,get('model_name'))
raw_value <- model@model[[1]]
value <- raw_value

lambda <- 10
sen_Test <- function(lambda){
update_value <- value
#confidence_value <- 10
sen_df <- data.frame()
#results <- matrix(rep(0,500*5),ncol=5)
sim.times <- 1000
results <- matrix(rep(0,sim.times*5),ncol=5)
temps <- matrix(rep(0,sim.times*3),ncol=3)
counter <- 1
#for(lambda in conf_values){
for(index in 1:sim.times){
#for(lambda in seq(50,1049) ){
###Update the value of the model###
# for(i in 1:dim(raw_value)[2]){
#   for(j in 1:dim(raw_value)[3]){
#     update_value[,i,j] <- rdirichlet(1,round(raw_value[,i,j]*lambda))
#   }
# }
###No parent vector###
if(dim(raw_value)[1]==1){
  update_value[1,] <- rdirichlet(1,round(raw_value[1,]*lambda))#update_value[1,]# rdirichlet(1,round(raw_value[1,]*lambda))#c(0.6,0.3,0.1)#rdirichlet(1,round(raw_value[1,]*lambda))
  print(update_value[1,])
}else{
  dim_len = length(dim(raw_value))
  if(dim_len==2){
    for(i in 1:dim(raw_value)[2]){
      update_value[,i] <- rdirichlet(1,round(raw_value[,i]*lambda))
    }
  }
  if(dim_len==3){
    for(i in 1:dim(raw_value)[2]){
      for(j in 1:dim(raw_value)[3]){
        update_value[,i,j] <- rdirichlet(1,round(raw_value[,i,j]*lambda))
      }
    }
  }
  if(dim_len==4){
    for(i in 1:dim(raw_value)[2]){
      for(j in 1:dim(raw_value)[3]){
        for(k in 1:dim(raw_value)[4])
          update_value[,i,j,k]  <- rdirichlet(1,round(raw_value[,i,j,k]*lambda))
      }
    }
  }
}
  
  


##Check the devation of the sample data
deviation <- sum(abs(update_value-raw_value))
##Update the model parameter
# BoERates_1@model[[1]] <- update_value
# BoERates_1@model
model@model[[1]] <- update_value
model@model

assign(model@name,model,.GlobalEnv)
##compile the new model
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
cgfit <- fit.net.z(nnodes,net)
cgfit

ddd<-rbn(cgfit,n=2000)
ddd$Defaults_1[which(ddd$Defaults_1<0)] <- 0
#default_mean <- mean(ddd$Defaults_1)
default_sd <- sd(ddd$Defaults_1)
default_mean <- mean(ddd$Defaults_1)
min_default <- min(ddd$Defaults_1)
results[counter,1] <- deviation
#results[counter,2] <- default_mean
results[counter,2] <- default_sd
results[counter,3] <- default_mean
results[counter,4] <- min_default
results[counter,5] <- lambda
temps[counter,1] <- update_value[1,1]
temps[counter,2] <- update_value[1,2]
temps[counter,3]  <- default_mean
print(paste0('default mean ', default_mean))
#sen_df <- rbind(sen_df,results)
counter <- counter+1
print(paste(counter,lambda,sep='|'))
rm(ddd)
rm(cgfit)
#BoERates_1@model[[1]] <- raw_value
}
#}
model@model[[1]] <- raw_value
assign(model@name,model,.GlobalEnv)
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
cgfit <- fit.net.z(nnodes,net)
sen_df <- as.data.frame(results)
#colnames(sen_df) <- c('deviation','default_mean','default_max','default_min','lambda')
colnames(sen_df) <- c('deviation','default_sd','default_mean','default_min','lambda')
#sen_df$range <- sen_df$default_max - sen_df$default_min
max_sen <- sen_df[which.max(sen_df$range),]
max_mean <- sen_df[which.max(sen_df$default_mean),]

ggplot(sen_df,aes(x=lambda,y=default_mean))+geom_point()+stat_smooth()
ggplot(sen_df,aes(x=deviation,y=default_mean))+geom_point()+geom_text(data=NULL,x=max_mean$deviation,y=max_mean$default_mean,label=round(max_mean$default_mean,3),hjust=0,vjust=0)
ggplot(sen_df,aes(x=default_mean))+geom_histogram()
###update y to be default_mean
#ggplot(sen_df,aes(x=deviation,y=range))+geom_point()+geom_text(data=NULL,x=max_sen$deviation,y=max_sen$range,label=round(max_sen$range,3),hjust=0,vjust=0)

#ggplot(sen_df,aes(x=deviation,y=default_mean))+geom_point()+geom_text(data=NULL,x=max_mean$deviation,y=max_mean$default_mean,label=round(max_mean$default_mean,3),hjust=0,vjust=0)
#plot(results,xlab='Deviation',ylab='Default_Mean')
#temps_df <- as.data.frame(temps)
#colnames(temps_df) <- c('Prob1','Prob2','Default_mean')
#scatterplot3d(temps_df$Prob1,temps_df$Prob2,temps_df$Default_mean, pch=16, highlight.3d=TRUE,
 #             type="h", main="3D Scatterplot")
#plot3d(temps_df$Prob1,temps_df$Prob2,temps_df$Default_mean,col="red", size=3)
}
library(manipulate)
manipulate(sen_Test(lambda),lambda=slider(50,1000,step=50))


pd.plot <- sen_df 
head(pd.plot)
pd.plot <- rbind(pd.plot,sen_df)
ggplot(pd.plot,aes(x=default_mean))+geom_histogram()+facet_grid(lambda~.)

###For continuous node, we take the sample from N(u,1/lambda*u) as perturbation.
