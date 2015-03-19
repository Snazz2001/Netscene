require(compiler)
enableJIT(3)
require(MCMCpack)
library(MASS)
library(Matrix)
model_name <- 'LTV_1'#'HPI_1'#BoERates_1'#'HPI_1'#'BoERates_1'#'Inflation_1'##BoERates_1'#'HPI_1'#
model <- (eval(parse(text=model_name)))
#model <- with(nnodes,get('model_name'))
raw_value <- model@model$model$coef
value <- raw_value

###For every lambda, run it 1000 times for find distribution of pert parameter
perturb_coef <- function(model,lambda){
  x <- model@model$model$coef[1,]
  # lambda <- 10
  n <- 1000
  sigma <- Diagonal(length(x),x/lambda)
  data.plot <- mvrnorm(n,x,sigma)
  data.plot <- as.data.frame(data.plot)
  data.plot$lambda <- lambda
  data.plot.long <- melt(data.plot,id.vars = c('lambda'))
  ggplot(data.plot.long,aes(x=value))+geom_histogram()+facet_grid(.~variable)+scale_x_continuous(limits = c(-5, 5))
}
library(manipulate)
manipulate(perturb_coef(model,lambda),lambda=slider(0.1,1000,step=0.2))


###For every lambda, run it 600 times
perturb_coef <- function(x,lambda){
  n <- 1
  # lambda <- 10
  sigma <- Diagonal(length(x),x/lambda)
  mvrnorm(n,x,sigma)
}



###For every lambda, run it 600 times
perturb_coef <- function(x,lambda){
  n <- 1
 # lambda <- 10
  sigma <- Diagonal(length(x),x/lambda)
  mvrnorm(n,x,sigma)
}

sim_mean <- function(cgfit){
  ddd<-rbn(cgfit,n=2000)
  ddd$Defaults_1[which(ddd$Defaults_1<0)] <- 0
  mean(ddd$Defaults_1)
}

results_c <- rep(0,600)
results_df <- data.frame()
sd_df <- matrix(rep(0,8*2),ncol=2)
counter <- 1
for(lambda in c(0.001,0.01,0.05,0.1,0.5,1,5,10)){
  for(j in 1:600){

    for(i in 1:dim(value)[1]){
      value[i,] <- perturb_coef(raw_value[i,],lambda)
    }

    model@model$model$coef <- value
    model
    assign(model@name,model,.GlobalEnv)
    nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
    cgfit <- fit.net.z(nnodes,net)
    cgfit

    results_c[j] <- sim_mean(cgfit)
  }
  tmp <- data.frame('default_mean'=results_c,'lambda'=lambda)
  if(dim(results_df)[1]==0){
    results_df <- tmp
  }else{
    results_df <- rbind(results_df,tmp)
  }
  sd_df[counter,] <- c(sd(results_c),lambda)
  counter <- counter + 1
}
colnames(sd_df) <- c('default_sd','lambda')
sd_df <- as.data.frame(sd_df)
ggplot(results_df,aes(x=as.factor(lambda),y=default_mean))+geom_boxplot()

ggplot(sd_df,aes(x=lambda,y=sd.mean.norm))+geom_line()+geom_point()+ggtitle('Sd of mean default by perturbing LTV')

model@model$model$coef <- raw_value
assign(model@name,model,.GlobalEnv)
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
cgfit <- fit.net.z(nnodes,net)
cgfit

results_unpert <- rep(0,600)
for(j in 1:600){  
  results_unpert[j] <- sim_mean(cgfit)
}
mean(results_unpert)#0.3762383
sd(results_unpert)#0.002101426
