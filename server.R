
require("shiny")
setwd("C:\\Projects\\BayesianNetwork\\Netscene")
require(bnlearn)
require(Rgraphviz)
require(vcd)
require(ggplot2)
require(stringr)
require(reshape)
#include the definition of the class nodes
source("C:\\Projects\\BayesianNetwork\\Netscene\\GraphRiskNode_class_z.R")


###Convert the vec to a list with name is the value of vec and element of list is also value of list
### input: vec = c("a","b")
### output:$a    $b
###        [1] a [2] b
namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}

pn<-{}
nodes<-{}
#yn   <- c(0,1)
#decl <- seq(0.1,1.1,1)
#dech <- seq(0.1,1.1,1)

EEselNode<-1

evi_list <- list() ## should be ok if i replace << with < 

result_bin<-list()

####old network starts here###
####specify the node configure
#hpi_model <- matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
#ltv_model <- list(coef = c("(Intercept)" = 2), sd = 1)
#dtv_model <- list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
#                              dimnames = list(c("(Intercept)", "ltv"), NULL)),
#                sd = c(0.3, 0.6))
#vintage_model <- list(coef=c("(Intercept)"=3,"ltv"=1.6),sd=1.5)
#BoeIR_model <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
#IntGearing_model <- list(coef = matrix(c(-1.2, 1.1, 2, 2.6), ncol = 2,
#                              dimnames = list(c("(Intercept)", "BoeIR"), NULL)),
#                sd = c(0.13, 0.36))
#Unemp_model <- list(coef=c("(Intercept)"=0.5),sd=0.3)
#exog_model <- list(coef=c("(Intercept)"=3,"Unemp"=1.6,"IntGearing"=-0.6),sd=1.5)
#maturity_model <- matrix(c(0.7, 0.3), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
#DefRate_model <- list(coef = matrix(c(-1.2, 1.1, 2, -2.6), ncol = 2,
#                                  dimnames = list(c("(Intercept)", "maturity"), NULL)),
#                    sd = c(0.13, 0.36))
####note for 2 discrete nodes, we need to use 8 parameters(4 can be derived from another four)####

####wrap node configure into GRNode class, GRNode_d is for discrete node and GRNode_c is for continous node, parents,children here works
####as place holder
##hpi <- new("GRNode_d",name="hpi",model=list(model=hpi_model),values=c("LOW", "HIGH"),parents=c(NA,NA),children=c(NA,NA))
#hpi <- new("GRNode_d",name="hpi",model=list(model=hpi_model),values=colnames(hpi_model),parents=c(NA,NA),children=c(NA,NA))
#ltv <- new("GRNode_c",name="ltv",model=list(model=ltv_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#dtv <- new("GRNode_c",name="dtv",model=list(model=dtv_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#vintage <- new("GRNode_c",name="vintage",model=list(model=vintage_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
##BoeIR <- new("GRNode_d",name="BoeIR",model=list(model=BoeIR_model),values=c("LOW", "HIGH"),parents=c(NA,NA),children=c(NA,NA))
#BoeIR <- new("GRNode_d",name="BoeIR",model=list(model=BoeIR_model),values=colnames(BoeIR_model),parents=c(NA,NA),children=c(NA,NA))
#IntGearing <- new("GRNode_c",name="IntGearing",model=list(model=IntGearing_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Unemp <- new("GRNode_c",name="Unemp",model=list(model=Unemp_model),values=c(0,1),parents=c(NA,NA),children=c(NA,NA))
#exog <- new("GRNode_c",name="exog",model=list(model=exog_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
##maturity <- new("GRNode_d",name="maturity",model=list(model=maturity_model),values=c("YES","NO"),parents=c(NA,NA),children=c(NA,NA))
#maturity <- new("GRNode_d",name="maturity",model=list(model=maturity_model),values=colnames(maturity_model),parents=c(NA,NA),children=c(NA,NA))
#DefRate <- new("GRNode_c",name="DefRate",model=list(model=DefRate_model),values=c(0,1),parents=c(NA,NA),children=c(NA,NA))

###How to setup the model structure.
#networkstring <- "[hpi][ltv][dtv|hpi:ltv][vintage|ltv][BoeIR][IntGearing|BoeIR:ltv][Unemp][exog|Unemp:IntGearing][maturity][DefRate|maturity:exog]"
###old network ends here###

income_level <- c('0','2.5%','5%')
inflation_level <- c('<1.5%','1.5%-2.5%','>2.5%')
boe_level <- c('0.5%','1.5%','2.5%')
spread_level <- c('1%','2%')#new add
hpi_level <- c('-10%','0%','10%')#5% 20% 75% LTV->44% 48% 55%

Income_1_model <- matrix(c(0.1,0.8,0.1),ncol=3,dimnames=list(NULL,'Income_1'=income_level))
Inflation_1_model <- matrix(c(0.9,0.09,0.01,0.1,0.6,0.3,0.01,0.3,0.69),ncol=3,dimnames=list('Inflation_1'=inflation_level,'Income_1'=income_level))
HPI_1_model <- matrix(c(0.05,0.20,0.75),ncol=3,dimnames=list(NULL,'HPI_1'=hpi_level))
BoERates_1_model <- c(0.99,0.005,0.005,0.9,0.09,0.01,0.8,0.19,0.01,0.99,0.005,0.005,0.6,0.39,0.01,0.2,0.6,0.2,0.9,0.09,0.01,0.2,0.7,0.1,0.01,0.4,0.59)
dim(BoERates_1_model) <- c(3,3,3)
dimnames(BoERates_1_model) <- list('BoERates_1'=boe_level,'Income_1'=income_level,'Inflation_1'=inflation_level)
Spread_1_model <- matrix(c(0.65,0.35),ncol=2,dimnames=list(NULL,'Spread_1'=spread_level))#new add

#LTV_1_model <- list(coef = c("(Intercept)" = 0.4813), sd = 0.2248)
LTV_1_model <- list(coef = matrix(c(0.4400,1,0.4813,1,0.5500,1),ncol=3,dimnames=list(c("(Intercept)","HPI_1"),NULL)), sd = c(0.20,0.20,0.20))
#the below is make up number
ltv_level <-c('<20%','20%-30%','30%-40%','40%-50%','50%-60%','60%-70%','70%-75%','75%+')
#LTV_1_model <- matrix(c(0.02,0.05,0.09,0.14,0.19,0.2,0.19,0.12),ncol=8,dimnames=list(NULL,'LTV_1'=ltv_level))

dti_level <- c('<5%','5%-10%','10%-15%','15%-20%','20%-25%','25%+')
#DTI_1_model <- matrix(c(0.16,0.16,0.16,0.16,0.16,0.20,
#						0.13,0.14,0.15,0.16,0.18,0.24,
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
Defaults_1_model <- list(coef=matrix(c(0.00,0.4,0.04,0.4,0.1,0.4,0.13,0.4,0.16,0.4,0.18,0.4),ncol=6,dimnames=list(c("(Intercept)",'LTV_1'),NULL)),
	sd=c(0.01,0.01,0.01,0.01,0.01,0.01))

#dtv_model <- list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
#                              dimnames = list(c("(Intercept)", "ltv"), NULL)),
#                sd = c(0.3, 0.6))

defaults_level <- c('1','0')
#Defaults_1_model <- matrix(c(0.001,0.999,0.00088,0.99912,0.00108,0.99892,0.1,0.9,0.00139,0.99861,0.00165,0.99835,0.00222,0.99778,0.00261,0.99739,
#						0.00112,0.99888,0.00110,0.9989,0.00130,0.9987,0.00132,0.99868,0.00186,0.99814,0.00204,0.99796,0.00236,0.99764,0.00302,0.99698,
#						0.00115,0.99885,0.00130,0.9987,0.00132,0.99868,0.00162,0.99838,0.00224,0.99776,0.00249,0.99751,0.00311,0.99689,0.00399,0.99601,
#						0.00136,0.99864,0.00162,0.99838,0.00169,0.99831,0.00167,0.99833,0.00254,0.99746,0.00278,0.99722,0.00369,0.99631,0.00384,0.99616,
#						0.00133,0.99867,0.00188,0.99812,0.00195,0.99805,0.00181,0.99819,0.00259,0.99741,0.00288,0.99712,0.00393,0.99607,0.00466,0.99534,
#						0.00179,0.99821,0.00176,0.99824,0.00175,0.99825,0.00181,0.99819,0.00281,0.99719,0.00307,0.99693,0.00386,0.99614,0.00403,0.99597))
#dim(Defaults_1_model) <- c(2,6,8)
#dimnames(Defaults_1_model) <- list('Defaults_1'=defaults_level,'DTI_1'=dti_level,'LTV_1'=ltv_level)


###specify the node configure
##hpi_model <- matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
#Income_1_model <- list(coef = c("(Intercept)" = 0.3), sd = 0.2)#based on page 9 at document.
#Inflation_1_model <- list(coef=c("(Intercept)"=3,"Income_1"=2),sd=1.5)
#BoERates_1_model <- list(coef=c("(Intercept)"=3,"Income_1"=-11.6,"Inflation_1"=2.6),sd=1.5)#

#DTI_1_model <- list(coef=c("(Intercept)"=3,"BoERates_1"=1.6,"Income_1"=-0.6,"LTV_1"=1.2,"Spread_1"=1.3),sd=1.5)#update
##LTV_1_model <- list(coef = c("(Intercept)" = 0.4813), sd = 0.2248)##sd is derived from the table, basically it is a mean.
###estimate from beta distribution with shape1=5,shape2=2, it is transformed data(d = 1.94485, transformed data is (x**d-1)/d.)
###to get the raw LTV data, we need (td+1)**(1/d), t is transformed data, d is 1.94485
#LTV_1_model <- list(coef = c("(Intercept)" = -0.2344), sd = 0.1119) 
#Spread_1_model <- list(coef=c("(Intercept)" = 0.013), sd = 0.001)##based on the assumption that the spread is fixed and we model it using peak gaussian.
#Defaults_1_model <- list(coef=c("(Intercept)"=-0.0087,"DTI_1"=0.3034,"LTV_1"=0.3455),sd=0.1)#sd is derived from the default rate sd. Note the weight is multiply by 100 here

#Income_2_model <- list(coef=c("(Intercept)"=3,"BoERates_1"=2),sd=1.5)
#Inflation_2_model <- list(coef=c("(Intercept)"=3,"Income_2"=2),sd=1.5)
#BoERates_2_model <- list(coef=c("(Intercept)"=3,"Income_2"=1.6,"Inflation_2"=-0.6),sd=1.5)
#DTI_2_model <- list(coef=c("(Intercept)"=3,"BoERates_2"=1.6,"Income_2"=-0.6),sd=1.5)
#LTV_2_model <- list(coef = c("(Intercept)" = 8), sd = 1)
#Spread_2_model <- list(coef=c("(Intercept)" = 2), sd = 1)
#Defaults_2_model <- list(coef=c("(Intercept)"=3,"DTI_2"=1.6,"LTV_2"=-0.6,"Spread_2"=2.2),sd=1.5)

###note for 2 discrete nodes, we need to use 8 parameters(4 can be derived from another four)###

###wrap node configure into GRNode class, GRNode_d is for discrete node and GRNode_c is for continous node, parents,children here works
###as place holder
#hpi <- new("GRNode_d",name="hpi",model=list(model=hpi_model),values=c("LOW", "HIGH"),parents=c(NA,NA),children=c(NA,NA))
#Income_1 <- new("GRNode_c",name="Income_1",model=list(model=Income_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Inflation_1 <- new("GRNode_c",name="Inflation_1",model=list(model=Inflation_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#BoERates_1 <- new("GRNode_c",name="BoERates_1",model=list(model=BoERates_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#DTI_1 <- new("GRNode_c",name="DTI_1",model=list(model=DTI_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#LTV_1 <- new("GRNode_c",name="LTV_1",model=list(model=LTV_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Spread_1 <- new("GRNode_c",name="Spread_1",model=list(model=Spread_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Defaults_1 <- new("GRNode_c",name="Defaults_1",model=list(model=Defaults_1_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))

Income_1 <- new("GRNode_d",name="Income_1",model=list(model=Income_1_model),values=income_level,parents=c(NA,NA),children=c(NA,NA))
Inflation_1 <- new("GRNode_d",name="Inflation_1",model=list(model=Inflation_1_model),values=inflation_level,parents=c(NA,NA),children=c(NA,NA))
HPI_1 <- new("GRNode_d",name="HPI_1",model=list(model=HPI_1_model),values=hpi_level,parents=c(NA,NA),children=c(NA,NA))
BoERates_1 <- new("GRNode_d",name="BoERates_1",model=list(model=BoERates_1_model),values=boe_level,parents=c(NA,NA),children=c(NA,NA))
DTI_1 <- new("GRNode_d",name="DTI_1",model=list(model=DTI_1_model),values=dti_level,parents=c(NA,NA),children=c(NA,NA))
LTV_1 <- new("GRNode_c",name="LTV_1",model=list(model=LTV_1_model),values=ltv_level,parents=c(NA,NA),children=c(NA,NA))
Spread_1 <- new("GRNode_d",name="Spread_1",model=list(model=Spread_1_model),values=spread_level,parents=c(NA,NA),children=c(NA,NA))
Defaults_1 <- new("GRNode_c",name="Defaults_1",model=list(model=Defaults_1_model),values=defaults_level,parents=c(NA,NA),children=c(NA,NA))

#Income_2 <- new("GRNode_c",name="Income_2",model=list(model=Income_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Inflation_2 <- new("GRNode_c",name="Inflation_2",model=list(model=Inflation_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#BoERates_2 <- new("GRNode_c",name="BoERates_2",model=list(model=BoERates_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#DTI_2 <- new("GRNode_c",name="DTI_2",model=list(model=DTI_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#LTV_2 <- new("GRNode_c",name="LTV_2",model=list(model=LTV_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Spread_2 <- new("GRNode_c",name="Spread_2",model=list(model=Spread_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
#Defaults_2 <- new("GRNode_c",name="Defaults_2",model=list(model=Defaults_2_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))

#networkstring <- "[Spread_1][LTV_1][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][DTI_1|Income_1:BoERates_1][Defaults_1|DTI_1:LTV_1:Spread_1]
#[Income_2|BoERates_1][Inflation_2|Income_2][BoERates_2|Inflation_2:Income_2][DTI_2|Income_2:BoERates_2][Spread_2][LTV_2][Defaults_2|DTI_2:LTV_2:Spread_2]"

#networkstring <- "[Spread_1][LTV_1][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][DTI_1|Income_1:BoERates_1:Spread_1:LTV_1][Defaults_1|DTI_1:LTV_1]"
networkstring <- "[HPI_1][LTV_1|HPI_1][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][Spread_1][DTI_1|Income_1:BoERates_1:Spread_1][Defaults_1|DTI_1:LTV_1]"
#networkstring <- "[ALTV][Income_1][Inflation_1|Income_1][BoERates_1|Inflation_1:Income_1][DTI_1|Income_1:BoERates_1][Defaults_1|DTI_1:ALTV]"
net <- model2network(networkstring)

###put all the GRNode into one list
#nnodes <- list(Income_1,Inflation_1,BoERates_1,DTI_1,Spread_1,LTV_1,Defaults_1,Income_2,Inflation_2,BoERates_2,DTI_2,Spread_2,LTV_2,Defaults_2)
#nnodes <- list(Income_1,Inflation_1,BoERates_1,DTI_1,Spread_1,LTV_1,Defaults_1)
nnodes <- list(HPI_1,Income_1,Inflation_1,BoERates_1,DTI_1,LTV_1,Defaults_1,Spread_1)
print(paste('nodes length is ',length(nnodes)))
###build the network###
cgfit <- fit.net.z(nnodes,net)

###get all names for every node###
node_names <- get.name(nnodes)
print(paste0('the name of list from get name method is ',node_names))
###query the network###
#cpquery(cgfit, (maturity=="YES"), TRUE)
#bin=0.1
#b<-cpdist(cgfit, "DefRate",(maturity=="NO")&(Unemp>0.1)&(Unemp<0.12))
#plot(density(b$DefRate))
#mean(b$DefRate)
## [1] -22.44305
#plot(density(b$DefRate))

ddd<-rbn(cgfit,n=2000)


#nodes<-list(hpi, ltv,dtv,br,ig,ue,e,m,v,DR)
#plist <- compileCPT(lapply(nodes,as,"cptable"))
#pn    <- grain(plist)
#pnc <- compile(pn, propagate=TRUE)
#ddd<-simulate2(pnc,n=1000)


nAttrs <- list()

print('so far good to enter shiny server')
#nAttrs$fillcolor<-c(hpi="white",dtv="red")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session){

#output$selectUI3 <- renderUI() ({
##	print("presenting the ui3 ")
##	print(node_names)
#	checkboxGroupInput("ParentNodeList","Parents",choices= node_names,inline = TRUE)
#})
# This code defines plot for Distributions tab

#output$distPlot <- renderPlot({ #renderGvis
#	print (input$ChartChoice)
#	if ((input$ChartChoice == "Scatter Plot" || input$ChartChoice == "Heat Map") && (length(input$ExamineNodeX)>0)){
#		###To bin continous variable into different group###
#		print('scatterplot starts! ')
#		print(paste0('nodex is ',input$ExamineNodeX))
#		print(paste0('nodey is ',input$ExamineNodeY))
#		
#		x <- get.node.info(nnodes,input$ExamineNodeX)
#		y <- get.node.info(nnodes,input$ExamineNodeY)#

#		x_type <- x[['type']]
#		y_type <- y[['type']]#

#		#x_data <- with(ddd,get(input$ExamineNodeX))
#		#y_data <- with(ddd,get(input$ExamineNodeY))
#		plot.obj<<-list()
#		plot.obj$data <<- ddd
#		print('*************************')
#		print(head(plot.obj$data))
#		plot.obj$x <<- with(ddd,get(input$ExamineNodeX))
#		plot.obj$y <<- with(ddd,get(input$ExamineNodeY))#

#		if(x_type=='c' && y_type=='c'){
#		g_data<-data.frame(
#		round(3*ddd[, input$ExamineNodeX],0)/3,
#		round(3*ddd[, input$ExamineNodeY],0)/3
#		)
#		print('1111')
#		g_dat2<-table(g_data)
#		g_dat3<-data.frame(col = as.numeric(rep(colnames(g_dat2), each = nrow(g_dat2))), 
#           		row = as.numeric(rep(rownames(g_dat2), ncol(g_dat2))), 
#           		value = as.vector(g_dat2))
#		g_dat4<-data.frame(label=as.character(g_dat3[,"value"]),g_dat3)
#		g_dat5<-g_dat4[g_dat4$value>0,]
#		}
#		print('222')
#	#	ooo<-gvisBubbleChart(g_dat5,idvar="label",xvar="row",yvar="col",
#	#		colorvar="value",sizevar="value",
#	#		options = list (  width=900, height=600,
#	#	#	sizeAxis= '{minValue: 0,  maxSize: 50}',
#	#	      gvis.editor = "Edit me!"))#

#		if (input$ChartChoice == "Scatter Plot"){
#			print("inside the scatterplot to see what variables are in ")
#			print(get.name(nnodes))
#			print(paste0('x_type is ',x_type,' y_type is ',y_type))
#			if (x_type=='c' && y_type=='c'){
#				sunflowerplot(g_dat3[,1],g_dat3[,2],g_dat3[,3],main="Scatter Plot",
#				xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY));
##				y11 <- paste0('x=',input$ExamineNodeX)
##				y22 <- paste0('y=',input$ExamineNodeY)
##				ggplot(ddd,aes(eval(parse(text=y11)),eval(parse(text=y22))))+geom_point()
#			} else if(x_type=='d' && y_type=='d'){
#				 fmla<-as.formula(paste0('~',input$ExamineNodeX,'+',input$ExamineNodeY))
#				 mosaic(fmla,ddd,shade=TRUE,legend=TRUE)
#			} else if(x_type=='d' && y_type=='c'){
#				flm <- paste0('x=',input$ExamineNodeY)
#				print(paste('flm is ',flm))
#				facet <- paste0(input$ExamineNodeX,'~.')
#				print(paste('00000',flm))
#				print(paste('1111',"2222"))
#				print(colnames(plot.obj$data))
#			#	p<-ggplot(ddd,aes(x=zzz,fill=as.factor(BoeIR)))+geom_density(alpha=.75)+xlab('zzz')	
#			#	p<-ggplot(plot.obj$data, aes(x=plot.obj$y))+geom_histogram()
#				p<-ggplot(plot.obj$data,aes(x=plot.obj$y,fill=as.factor(plot.obj$x)))+geom_density(alpha=.75)+xlab(input$ExamineNodeY)				
#			#	p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeY)+facet_grid(eval(parse(text=facet)))
#			#	p <- ggplot(ddd, aes(x=y_data,fill=x_data)) + geom_histogram() + xlab(input$ExamineNodeY) 
#				print(p)
#				print('print!!!')
#			} else {
#				flm <- paste0('x=',input$ExamineNodeX)
#				print(paste('flm here is ',flm))
#				facet <- paste0(input$ExamineNodeY,'~.')
#				print(paste('2222',flm))
#				print(paste('3333',facet))
#			#	p<-ggplot(plot.obj$data, aes(x=plot.obj$x))+geom_histogram()
#				p<-ggplot(plot.obj$data,aes(x=plot.obj$x,fill=as.factor(plot.obj$y)))+geom_density(alpha=.75)+xlab(input$ExamineNodeX)			
#			#	p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeX)+facet_grid(eval(parse(text=facet)))
#			#	p <- ggplot(ddd, aes(x=x_data,fill=y_data)) + geom_histogram() + xlab(input$ExamineNodeX) 		
#				print(p)
#				print('print!!!!!!!')
#			}
#		} 
#		print('3333')
#		if (input$ChartChoice == "Heat Map" && x_type=='c' && y_type=='c'){
#			filled.contour(g_dat2,main="Heat Map",
#				xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY))
#		}
#	}
#			print('444')
#	if(input$ChartChoice == "Histogram" && x_type=='c'){
#		plot(hist(ddd[, input$ExamineNodeX]), main="Histogram", xlab=print(input$ExamineNodeX))
#	}	
#  })

output$distPlot <- renderPlot({ #renderGvis
#	print (input$ChartChoice)
	input$Plot
	isolate({
#	if ((input$ChartChoice == "Scatter Plot" || input$ChartChoice == "Heat Map") && (length(input$ExamineNodeX)>0)){
	if (length(input$ExamineNodeX)>0){
		###To bin continous variable into different group###
		print('scatterplot starts! ')
		print(paste0('nodex is ',input$ExamineNodeX))
		print(paste0('nodey is ',input$ExamineNodeY))
		
		x <- get.node.info(nnodes,input$ExamineNodeX)
		y <- get.node.info(nnodes,input$ExamineNodeY)

		x_type <- x[['type']]
		y_type <- y[['type']]

	#	plot.obj<<-list()
	#	plot.obj$data <<- ddd
		print('*************************')
	#	print(head(plot.obj$data))
	#	plot.obj$x <<- with(ddd,get(input$ExamineNodeX))
	#	plot.obj$y <<- with(ddd,get(input$ExamineNodeY))
		plot.df <- data.frame(x=with(ddd,get(input$ExamineNodeX)),y=with(ddd,get(input$ExamineNodeY)))
		#x_data <- with(ddd,get(input$ExamineNodeX))
		#y_data <- with(ddd,get(input$ExamineNodeY))
		if(input$ExamineNodeX!=input$ExamineNodeY){


			if(x_type=='c' && y_type=='c'){
				g_data<-data.frame(
				round(3*ddd[, input$ExamineNodeX],0)/3,
				round(3*ddd[, input$ExamineNodeY],0)/3
				)
				print('1111')
				g_dat2<-table(g_data)
				g_dat3<-data.frame(col = as.numeric(rep(colnames(g_dat2), each = nrow(g_dat2))), 
           			row = as.numeric(rep(rownames(g_dat2), ncol(g_dat2))), 
           			value = as.vector(g_dat2))
				g_dat4<-data.frame(label=as.character(g_dat3[,"value"]),g_dat3)
				g_dat5<-g_dat4[g_dat4$value>0,]
			}
			print('222')
	#	ooo<-gvisBubbleChart(g_dat5,idvar="label",xvar="row",yvar="col",
	#		colorvar="value",sizevar="value",
	#		options = list (  width=900, height=600,
	#	#	sizeAxis= '{minValue: 0,  maxSize: 50}',
	#	      gvis.editor = "Edit me!"))

#			if (input$ChartChoice == "Scatter Plot"){
				print("inside the scatterplot to see what variables are in ")
				print(get.name(nnodes))
				print(paste0('x_type is ',x_type,' y_type is ',y_type))
				if (x_type=='c' && y_type=='c'){
#					sunflowerplot(g_dat3[,1],g_dat3[,2],g_dat3[,3],main="Scatter Plot",
#					xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY));
#					p <- ggplot(plot.obj$data,aes(x=plot.obj$x,y=plot.obj$y))+geom_point()+xlab(input$ExamineNodeX)+ylab(input$ExamineNodeY)
					p <- ggplot(plot.df,aes(x=x,y=y))+geom_point()+geom_density2d()+xlab(input$ExamineNodeX)+ylab(input$ExamineNodeY)
					print(p)
#					y11 <- paste0('x=',input$ExamineNodeX)
#					y22 <- paste0('y=',input$ExamineNodeY)
#					ggplot(ddd,aes(eval(parse(text=y11)),eval(parse(text=y22))))+geom_point()
				} else if(x_type=='d' && y_type=='d'){
					 #fmla<-as.formula(paste0('~',input$ExamineNodeX,'+',input$ExamineNodeY))
					 #mosaic(fmla,ddd,shade=TRUE,legend=TRUE)
					# ds <- table(plot.obj$x,plot.obj$y)
					 ds <- table(plot.df$x,plot.df$y)
					 color_length <- max(dim(ds))
					 ord <-order(apply(ds,1,sum),decreasing=TRUE)
					 mosaicplot(ds[ord,],main="Mosaic of the simulated data",color=colorspace::rainbow_hcl(color_length),cex=1.0,xlab=input$ExamineNodeX,ylab=input$ExamineNodeY)
				} else if(x_type=='d' && y_type=='c'){ 
			#		flm <- paste0('x=',input$ExamineNodeY)
			#		print(paste('flm is ',flm))
			#		facet <- paste0(input$ExamineNodeX,'~.')
			#		print(paste('00000',flm))
					print(paste('1111',"2222"))
			#		print(colnames(plot.obj$data))
			#		p<-ggplot(ddd,aes(x=zzz,fill=as.factor(BoeIR)))+geom_density(alpha=.75)+xlab('zzz')	
			#		p<-ggplot(plot.obj$data, aes(x=plot.obj$y))+geom_histogram()
			#		p<-ggplot(plot.obj$data,aes(x=plot.obj$y,fill=as.factor(plot.obj$x)))+geom_density(alpha=.75)+xlab(input$ExamineNodeY)	
					#p<-ggplot(plot.obj$data,aes(x=plot.obj$y))+facet_wrap(~plot.obj$x)+geom_histogram(aes(y=..density..))+geom_density(alpha=.75)+xlab(input$ExamineNodeY)
					p<-ggplot(plot.df,aes(x=y))+facet_wrap(~x)+geom_histogram(aes(y=..density..,fill=..count..))+geom_density(alpha=.75,colour='pink')+xlab(input$ExamineNodeY)
			#		p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeY)+facet_grid(eval(parse(text=facet)))
			#		p <- ggplot(ddd, aes(x=y_data,fill=x_data)) + geom_histogram() + xlab(input$ExamineNodeY) 
					print(p)
					print('print!!!')
				} else {
			#		flm <- paste0('x=',input$ExamineNodeX)
			#		print(paste('flm here is ',flm))
			#		facet <- paste0(input$ExamineNodeY,'~.')
			#		print(paste('2222',flm))
			#		print(paste('3333',facet))
			#		p<-ggplot(plot.obj$data, aes(x=plot.obj$x))+geom_histogram()
			#		p<-ggplot(plot.obj$data,aes(x=plot.obj$x,fill=as.factor(plot.obj$y)))+geom_density(alpha=.75)+xlab(input$ExamineNodeX)			
					p<-ggplot(plot.df,aes(x=x))+facet_wrap(~y)+geom_histogram(aes(y=..density..,fill=..count..))+geom_density(alpha=.75,colour='pink')+xlab(input$ExamineNodeX)
			#		p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeX)+facet_grid(eval(parse(text=facet)))
			#		p <- ggplot(ddd, aes(x=x_data,fill=y_data)) + geom_histogram() + xlab(input$ExamineNodeX) 		
					print(p)
					print('print!!!!!!!')
				}
#			print('3333')
#			if (input$ChartChoice == "Heat Map" && x_type=='c' && y_type=='c'){
#				filled.contour(g_dat2,main="Heat Map",
#				xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY))
#			}
		}else if(input$ExamineNodeX==input$ExamineNodeY&&x_type=='c'){
			p<-ggplot(plot.df,aes(x=x))+geom_histogram(aes(y=..density..,fill=..count..))+geom_density(alpha=.75,colour='pink')+xlab(input$ExamineNodeX)
			print(p)
		}else{
			p<-ggplot(plot.df,aes(x=x))+geom_bar()+xlab(input$ExamineNodeX)
			print(p)
		}
	}
#	print('444')
#	if(input$ChartChoice == "Histogram" && x_type=='c'){
#		plot(hist(ddd[, input$ExamineNodeX]), main="Histogram", xlab=print(input$ExamineNodeX))
#	}	
	})
  })

###In evidence tab, to extract the EvidenceNode type based on the type information in the input$EvidenceNode
###Maybe descard
choose_states<-reactive({
  	switch(get.node.info(nnodes,input$EvidenceNode)[['type']],
  		"c" = "c",
  		"d" = "d"
  		)
  	})


output$ExamineNodeX<- renderUI({
# print(paste("boo2",input$id))
#    if(input$id){
	print('enter examine node x component')
	selectInput("ExamineNodeX",label="Examine",choices = get.name(nnodes), selected="hpi")
#    }
})

#Enter Evidence
  output$netPlot1 <- renderPlot({	
		###plot the network, note here it is not updated yet. - zheng zhu
		#graphviz.plot(net)
		#graphviz.plot(net,highlight=list(nodes=c('DTI_1'),fill='Yellow'))
		input$Update
		isolate({
			print(paste0('******',length(evi_list)))
		if(length(evi_list) > 0){
			highlight_list <- unlist(lapply(evi_list,function(x) x$name))
			print('the following is the highlight list ')
			print(highlight_list)
			print('done highlight list')
			graphviz.plot(net.reactive(),shape="ellipse",highlight=list(nodes=as.vector(highlight_list),fill='Yellow'))
#			graphviz.plot(net,shape="ellipse",highlight=list(nodes=as.vector(highlight_list),fill='Yellow'))
		}else {
			print('no highlight node for netplot1!')
			graphviz.plot(net.reactive(),shape="ellipse")
#			graphviz.plot(net,shape="ellipse")
			}
		})
		###leave the simulation later
#		plot(net,nodeAttrs=nAttrs)




#		pnloc<-evalq(pn,envir=.GlobalEnv) #Get the global version	
#		plot(pnloc$dag,nodeAttrs=nAttrs)
#		print("Render Evidence Plot") #

#		if (input$EvidenceNode != "")
#		{
#			print(paste(input$EvidenceNode,"1",""))
#			pncloc<-evalq(pnc,envir=.GlobalEnv)
#			#pncloc<-setEvidence(pncloc,input$EvidenceNode,paste(input$EvidenceNode,"1",sep=""))
#			pncloc<-setEvidence(pnc,input$EvidenceNode,list(c(0.98,0.02)))
#			dddloc<-simulate2(pncloc,n=1000)
#			assign("pnc",pncloc,.GlobalEnv)
#			assign("ddd",dddloc,.GlobalEnv)
#			print(colMeans(dddloc))
#		}
  })

#  output$netPlot2 <- renderPlot({	
#		###plot the network, note here it is not updated yet. - zheng zhu
#		#graphviz.plot(net)
#		#graphviz.plot(net,highlight=list(nodes=c('DTI_1'),fill='Yellow'))
#			print('no highlight node for netplot1!')
#			graphviz.plot(net,shape="ellipse")
#  })

###In Build network tab, it used to update network string so that we can have updated net structure(not compiled)
###Use input$ParentNodeList and input$NewNodeName and global GRnode list to update the net
net.reactive <- reactive({
	if(length(input$ParentNodeList)>0){
	parents <- input$ParentNodeList
	name <- input$NewNodeName
	pnode_spec <- get.parents.info(nnodes,parents)
	networkstring <<- construct.networkstring(name,pnode_spec,networkstring)
	print(paste('network string is ',networkstring))
	net <- model2network(networkstring)##Do not use <<- to assign global variable here, wrong!!!
	}else{
	net}
	})


#Build Network
 output$netPlot2 <- renderPlot({	
		#pnloc<-evalq(pn,envir=.GlobalEnv)
		#nodesloc<-evalq(nodes,envir=.GlobalEnv)
		print('enter building net plot 2 component')
	#	input$CreateNode
		input$UpdateNetWork
		isolate({if(input$NewNodeName!=""&&!is.null(input$weight1)&&!is.na(input$weight1)&&!input$ParentNodeList==''){
			##get the input from ui
			name <- input$NewNodeName
			parents <- input$ParentNodeList
			type <- input$ContinuousOrDiscrete
			print(paste('the type is ',type))
			print(paste('the name is ',name))
			##get the parent information
			pnode_spec <- get.parents.info(nnodes,parents)
			ptypes <- sapply(pnode_spec,function(x) x$type)#
			##update the network string and compile network
			networkstring <<- construct.networkstring(name,pnode_spec,networkstring)
			print(paste('network string is ',networkstring))
			net <<- model2network(networkstring)#
			##setup the weights used for manually input based on the combination of node type###
			if(type=='c'){
				if(length(ptypes)==2){
					if(ptypes[1]=='c'&&ptypes[2]=='c'){
						weights <- c(input$weight1,input$weight2,input$weight3,input$weight4)
					}else if(ptypes[1]=='c'&&ptypes[2]=='d'){
						weights <- c(input$weight1,input$weight2,input$weight3,input$weight4,input$weight5,input$weight6)
					}else if(ptypes[1]=='d'&&ptypes[2]=='c'){
						weights <- c(input$weight1,input$weight2,input$weight3,input$weight4,input$weight5,input$weight6)						
					}
				}else if(length(ptypes)==1){
					if(ptypes[1]=='c'){
						weights <- c(input$weight1,input$weight2,input$weight3)
					}else if(ptypes[1]=='d'){
						weights <- c(input$weight1,input$weight2,input$weight3,input$weight4)
					}
				}else if(length(ptypes)==3){
					if(ptypes[1]=='c'&&ptypes[2]=='c'&&ptypes[3]=='c'){
						weights <- c(input$weight1,input$weight2,input$weight3,input$weight4,input$weight5)
					}
				}
			}else if(type=='d'){
				if(length(ptypes)==2){
					weights <- c(input$weight1,input$weight2,input$weight3,input$weight4,input$weight5,input$weight6,input$weight7,input$weight8)
				}else if(length(ptypes)==1){
					weights <- c(input$weight1,input$weight2,input$weight3,input$weight4)
				}
			}#

#			if(!is.null(input$weight3)){
#			weights <- c(input$weight1,input$weight2,input$weight3)
#			}else {
#			weights <- c()
#			}#


			##set up the new node configure
			conf_str <- conf.new.node(name,type,pnode_spec,weights)
			print(paste('conf str is ',conf_str))
			if(conf_str!=''){
				eval(parse(text=conf_str))
#				print(paste0('call newnode.class.string with name ',name,' and type is ',type))
				newnode_str <- newnode.class.string(name,type)
				print(paste('new node str is ',newnode_str))
				eval(parse(text=newnode_str))
				print('==============')
			#	print(name)
				#print(zz_model)
			#r	print(paste('1 name is ',name))
				nnodes <<- update.nodes.list(nnodes,eval(parse(text=name)))
			#	print(paste('2 name is ',name))
				node_names <<- get.name(nnodes)
				print(nnodes)
				print('---------')
				print(net)
				print(paste('the length of nnodes is ',length(nnodes)))
				cgfit <<- fit.net.z(nnodes,net)
				ddd<<-rbn(cgfit,n=2000)
				print('done fit network!!!')
			}     			
		}
		graphviz.plot(net,shape="ellipse")})
		print("Render Build Plot") 	
  })

####This is to generate a list includes the label for nodes(which can be added on the fly)
####So the dynamic UI can reflect the dynamic content in the network it is used in Distribution and Inference Tab
####It is based on reactive which if input$CreateNode update, it will change automatically.
get.name.reactive<-reactive({
	var.opts <- namel(colnames(ddd)) ##update here at 15:23
	if(input$CreateNode){
		var.opts <- namel(colnames(ddd))
		}
#    var.opts <- namel(get.name(nnodes))
#    print(paste0('in get name reactive, nodes are ',get.name(nnodes)))
	var.opts
	})


output$EnterParam <- renderUI({
	input$CreateNode
	isolate({
		if(input$ContinuousOrDiscrete=='c'&&length(input$ParentNodeList)>0){
			print(paste0('node is c'))
			print(paste0('parentlist is ',input$ParentNodeList))
			pnodes_spec <- get.parents.info(nnodes,input$ParentNodeList)
			ptypes <- sapply(pnodes_spec,function(x) x$type)
			pnames <- sapply(pnodes_spec,function(x) x$name)
			pvalues <- sapply(pnodes_spec,function(x) x$values)
			textInput("text", label = h3("Text input"), value = "Enter text...") 
			isStarted <- FALSE
			if(length(input$ParentNodeList)==2){
					if(ptypes[1]=='c'&&ptypes[2]=='c'){
						c(numericInput('weight1', label = paste0('Weight for Intercept ',pnames[1]),value = ''),
							numericInput('weight2', label = paste0('Weight for ',pnames[1]),value = ''),
						numericInput('weight3', label = paste0('Weight for ',pnames[2]), value = ''),
						numericInput('weight4', label = 'Standard deviation ', value = ''))
					}else if(ptypes[1]=='c'&&ptypes[2]=='d'){
						c(numericInput('weight1', label = paste0('Weight for Intercept under LOW ',pnames[2]), value = ''),
						numericInput('weight2', label = paste0('Weight for ',pnames[1],' under LOW ',pnames[2]) , value = ''),
						numericInput('weight3', label =  paste0('Weight for Intercept under High ',pnames[2]), value = ''),
						numericInput('weight4', label =  paste0('Weight for ',pnames[1],' under HIGH ',pnames[2]), value = ''),
						numericInput('weight5', label =  paste0('Weight for Standard deviation under LOW ',pnames[2]), value = ''),
						numericInput('weight6', label =  paste0('Weight for  Standard deviation under HIGH ',pnames[2]), value = ''))
					}else if(ptypes[1]=='d'&&ptypes[2]=='c'){
						c(numericInput('weight1', label = paste0('Weight for Intercept under LOW ',pnames[1]), value = ''),
						numericInput('weight2', label = paste0('Weight for ',pnames[2],' under LOW ',pnames[1]) , value = ''),
						numericInput('weight3', label =  paste0('Weight for Intercept under High ',pnames[1]), value = ''),
						numericInput('weight4', label =  paste0('Weight for ',pnames[2],' under HIGH ',pnames[1]), value = ''),
						numericInput('weight5', label =  paste0('Weight for Standard deviation under LOW ',pnames[1]), value = ''),
						numericInput('weight6', label =  paste0('Weight for Standard deviation under HIGH ',pnames[1]), value = ''))
					}
			}else if(length(input$ParentNodeList)==1){
				if(ptypes[1]=='c'){
						c(numericInput('weight1', label = 'Weight for Interception ', value = ''),
						numericInput('weight2', label = paste0('Weight for ',pnames[1]), value = ''),
						numericInput('weight3', label = paste0('standard deviation for ',pnames[1]), value = ''))
				}else if(ptypes[1]=='d'){
						c(numericInput('weight1', label = paste0('Weight for LOW ',pnames[1]), value = ''),
						numericInput('weight2', label = paste0('Weight for HIGH ',pnames[1]), value = ''),
						numericInput('weight3', label = paste0('Standard deviation for LOW ',pnames[1]), value = ''),
						numericInput('weight4', label = paste0('Standard deviation for HIGH ',pnames[1]), value = ''))
				}
			}else if(length(input$ParentNodeList)==3){
					if(ptypes[1]=='c'&&ptypes[2]=='c'&&ptypes[3]=='c'){
						c(numericInput('weight1', label = paste0('Weight for Intercept ',pnames[1]),value = ''),
						numericInput('weight2', label = paste0('Weight for ',pnames[1]),value = ''),
						numericInput('weight3', label = paste0('Weight for ',pnames[2]), value = ''),
						numericInput('weight4', label = paste0('Weight for ',pnames[3]), value = ''),
						numericInput('weight5', label = 'Standard deviation ', value = ''))
					}
				}
			}else if(input$ContinuousOrDiscrete=='d'&&length(input$ParentNodeList)>0){
				print(paste0('node is d'))
				print(paste0('parentlist is ',input$ParentNodeList))
				pnodes_spec <- get.parents.info(nnodes,input$ParentNodeList)
				ptypes <- sapply(pnodes_spec,function(x) x$type)
				pnames <- sapply(pnodes_spec,function(x) x$name)
				pvalues <- sapply(pnodes_spec,function(x) x$values)

				if(length(input$ParentNodeList)==2){
					if(ptypes[1]=='d'&&ptypes[2]=='d'){
					  c(numericInput('weight1', label = paste0('Probability for LOW ', input$NewNodeName, ' LOW ', pnames[1], ' and LOW ',pnames[2]), value = ''),
						numericInput('weight2', label = paste0('Probability for HIGH ', input$NewNodeName, ' LOW ', pnames[1], ' and LOW ',pnames[2]), value = ''),
						numericInput('weight3', label = paste0('Probability for LOW ', input$NewNodeName, ' HIGH ', pnames[1], ' and LOW ',pnames[2]), value = ''),
						numericInput('weight4', label = paste0('Probability for HIGH ', input$NewNodeName, ' HIGH ', pnames[1], ' and LOW ',pnames[2]), value = ''),
						numericInput('weight5', label = paste0('Probability for LOW ', input$NewNodeName, ' LOW ', pnames[1], ' and HIGH ',pnames[2]), value = ''),
						numericInput('weight6', label = paste0('Probability for HIGH ', input$NewNodeName, ' LOW ', pnames[1], ' and HIGH ',pnames[2]), value = ''),
						numericInput('weight7', label = paste0('Probability for LOW ', input$NewNodeName, ' HIGH ', pnames[1], ' and HIGH ',pnames[2]), value = ''),
						numericInput('weight8', label = paste0('Probability for HIGH ', input$NewNodeName, ' HIGH ', pnames[1], ' and HIGH ',pnames[2]), value = '')
						)
					}
				}else if(length(input$ParentNodeList)==1){
					if(ptypes[1]=='d'){
						c(numericInput('weight1', label = paste0('Probability for LOW ',input$NewNodeName,' LOW ',pnames[1]) , value = ''),
						numericInput('weight2', label = paste0('Probability for HIGH ',input$NewNodeName,' LOW ',pnames[1]), value = ''),
						numericInput('weight3', label = paste0('Probability for LOW ',input$NewNodeName,' HIGH ',pnames[1]) , value = ''),
						numericInput('weight4', label = paste0('Probability for HIGH ',input$NewNodeName,' HIGH ',pnames[1]), value = '')
						)
					}
				}
			}else{
				print('')
			}
		})	

	})

  output$netPlot_RT <- renderPlot({	
		###plot the network, note here it is not updated yet. - zheng zhu
		#graphviz.plot(net)
		#graphviz.plot(net,highlight=list(nodes=c('DTI_1'),fill='Yellow'))
		graphviz.plot(net.reactive(),shape="ellipse")
#		print(paste0('******',length(evi_list)))
#		if(length(evi_list) > 0){
#			highlight_list <- unlist(lapply(evi_list,function(x) x$name))
#			print('the following is the highlight list ')
#			print(highlight_list)
#			print('done highlight list')
#			graphviz.plot(net.reactive(),shape="ellipse",highlight=list(nodes=as.vector(highlight_list),fill='Yellow'))
#		}else {
#			print('no highlight node for netplot1!')
#			graphviz.plot(net.reactive(),shape="ellipse")
#			}
		
		###leave the simulation later
  })

output$ParentsUI <- renderUI({
	print(paste0('select parents ui ',colnames(ddd)))
	checkboxGroupInput("ParentNodeList","Parents",choices = get.name.reactive(),inline = FALSE,selected=input$ParentNodeList)
	})


output$selectUIDist1 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI1 is working ',colnames(ddd)))
	selectInput("ExamineNodeX",label="Examine",choices = get.name.reactive(),selected="BoERates_1")
#	selectInput("ExamineNodeX",label="Examine",choices = namel(get.name(nnodes)),selected="IntGearing")

#	selectInput("ExamineNodeX",label="Examine",choices = var.opts, selected="IntGearing")
})#

output$selectUIDist2 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI2 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("ExamineNodeY",label="Examine",choices = get.name.reactive(), selected="LTV_1")
#	selectInput("ExamineNodeY",label="Examine",choices = namel(get.name(nnodes)), selected="vintage")
})

output$selectUIEvid3 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI3 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("EvidenceNode",label="Select Node",choices = get.name.reactive(), selected="DTI_1")
#	selectInput("EvidenceNode",label="Select Node",choices = namel(get.name(nnodes)), selected="hpi")
})

output$selectUIEvid4 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI4 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("InterestNode",label="Node of Interests",choices = get.name.reactive(), selected="DefRate")
#	selectInput("InterestNode",label="Node of Interests",choices = namel(get.name(nnodes)), selected="DefRate")
})

# Code to deal with entering evidence on Enter Evidence page, need to consider the binary or continuous case separately - zheng
  output$ChooseState <- renderUI({
  	print('enter choose state component')
  	#selectInput("selection","Please do your selection",choose_states
  	if(length(input$EvidenceNode)>0){
  	print(paste0('evidence node is ',input$EvidenceNode))
  	enode <<- get.node.info(nnodes,input$EvidenceNode)
  	data <- with(ddd, get(input$EvidenceNode))
  	print('finish choose state soon! ')
  	if(is.numeric(data)){
  		print(min(data))
  		print(max(data))
  		print(head(data))
  	}
  	switch(choose_states(),
  		"c" = c(sliderInput(inputId = "mincimumValue",
                  label = "Start of the data range",
                  min = ifelse(round(min(data),3)>0,round(min(data),3),0),
                  max = round(max(data),3),
                  value = round(median(data),3),
                  step = round((max(data)-min(data))/200,5)
      ),
sliderInput(inputId = "maximumValue",
                  label = "End of the data range",
                  min = ifelse(round(min(data),3)>0,round(min(data),3),0),
                  max = round(max(data),3),
                  value = round(median(data),3)+0.03,
                  step = round((max(data)-min(data))/200,5)
      )
    ),
      "d" = selectInput("EvidenceChoice",label="Choose State",
   choices=enode[['values']])

  		)
  }
  })

###The blow function update the text based on user's input evidence and add them to evi_list
	output$evidences <- renderText({
		input$Update
		isolate({
		isValid <- TRUE
		if(input$EnterOrRetract == 'Enter'&&length(input$EvidenceNode)>0) # if it is enter evidence 
		{	
			isExist <<- FALSE
			updateIndex <- length(evi_list)+1
			if(length(evi_list)>0){
				for(i in 1:length(evi_list)){
  					if(evi_list[[i]]$name==input$EvidenceNode){
      					updateIndex <- i
  					}
				}
			}##ignore the !isExist 
#			if(length(input$EvidenceNode)>0&&choose_states()=="c"&&!isExist&&input$mincimumValue<input$maximumValue){ # to make sure it is continuous value and have more than one evidence and the evidence is valid
#				name_value_pairs <- list('name'=input$EvidenceNode,'type'='c',value=list(input$mincimumValue,input$maximumValue))
#				evi_list[[length(evi_list)+1]] <<- name_value_pairs
#			}else if(length(input$EvidenceNode)>0&&choose_states()=="d"&&!isExist){
#				name_value_pairs <- list('name'=input$EvidenceNode,'type'='d',value=list(input$EvidenceChoice))
#				evi_list[[length(evi_list)+1]] <<- name_value_pairs
#			}else if(length(input$EvidenceNode)>0&&choose_states()=="c"&&!isExist&&input$mincimumValue>=input$maximumValue){
#				isValid <- FALSE
#			}
			if(length(input$EvidenceNode)>0&&choose_states()=="c"&&input$mincimumValue<input$maximumValue){ # to make sure it is continuous value and have more than one evidence and the evidence is valid
				name_value_pairs <- list('name'=input$EvidenceNode,'type'='c',value=list(input$mincimumValue,input$maximumValue))
				evi_list[[updateIndex]] <<- name_value_pairs
			}else if(length(input$EvidenceNode)>0&&choose_states()=="d"){
				name_value_pairs <- list('name'=input$EvidenceNode,'type'='d',value=list(input$EvidenceChoice))
				evi_list[[updateIndex]] <<- name_value_pairs
			}else if(length(input$EvidenceNode)>0&&choose_states()=="c"&&!isExist&&input$mincimumValue>=input$maximumValue){
				isValid <- FALSE
			}
		}else if(input$EnterOrRetract == 'Retract'&&length(input$EvidenceNode)>0&length(evi_list)>0){ # if it is remove evidence
			index_to_remove <- -999
			for(i in 1:length(evi_list)){ # search the index to remove
  				if(evi_list[[i]]$name==input$EvidenceNode){
      				index_to_remove<-i
  				}
			}
			if(index_to_remove>0){ # remove it
  				evi_list[[index_to_remove]]<<-NULL
			}
		}
		print('==============done for evidence text rendering')
	#	unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"==",x$value),paste0(x$name,'=',x$value[[1]],'->',x$value[[2]]))))
		if(isValid){
			unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"==",x$value),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
			}else{
			paste0('Please input valid data')	
			}
#		print(evi_list)
#		evi_list
#		switch(choose_states(),
#			"c" = paste0(input$EvidenceNode,'=',input$mincimumValue,':',input$maximumValue),
#			"d" =  paste0(input$EvidenceNode,'=',input$EvidenceChoice))
		})
		})

	output$inference <- renderPlot({
		input$Inference
		isolate({
			p.obj <<- list()
			if(length(input$InterestNode)>0){
			type <- get.node.info(nnodes,input$InterestNode)[['type']]
			print(paste0("**********",input$InterestNode))
			evi_string <- ''
			print(paste0('doing inference now with type ',type))
			print(evi_list)
			if(type == 'c'&&length(evi_list)>0){
				evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
				print('entering c branch!!!')
				print(evi_vector)
				for(i in 1:length(evi_vector)){
					if(str_count(evi_vector[i],' ')==0){##if it has on combined evidence, i.e. only one value
						if(evi_string=='')##if it is the first evidence
						{
							evi_string <- paste0(evi_vector[i],'')
						}else {##otherwise append one 
							evi_string <- paste0(evi_string,'&',evi_vector[i])
						}
					}else{##if it has combined evidence
						temp <- str_replace(evi_vector[i],' ','&')
						if(evi_string==''){
							evi_string <- paste0(temp,'')
						}else {
							evi_string <- paste0(evi_string,'&',temp)
						}
					}
				}
		#		evi_string <<- str_sub(evi_string,2)
				evi_string <- gsub(' ','',evi_string)

				print(paste0("the evi_string is ",evi_string))
#				if(grepl("^[&]",evi_string)){
#					evi_string <<-str_sub(evi_string,2)
#				}
				evi_string <- gsub('<-','< -',evi_string)
				print(paste0("now the evi_string is ",evi_string))
				print(paste0("the interest node is ",input$InterestNode))
				eval_string <- paste0('cpdist(cgfit,"',input$InterestNode,'",',evi_string,')')
				print(eval_string)
				seed <- 1
				set.seed(seed)
				result <- eval(parse(text=eval_string))
				while(dim(result)<500&&seed<30){
					seed <- seed + 1
					set.seed(seed)
					tmp <- eval(parse(text=eval_string))
					result <-  rbind(result,tmp)
				}

			#	result <- cpdist(cgfit,input$InterestNode,eval(parse(text=evi_string)))
			####Need to make sure result has valid value instead of 0.
				if(nrow(result)>0){
					print(paste('the range of it is ',range(result),collapse=' || '))
					x_data <- with(result,get(input$InterestNode))
					p<-ggplot(result,aes(x_data))+geom_density(alpha=.75)+xlab(input$InterestNode)+geom_text(data=NULL,x=round(mean(x_data),1),y=0.01,label=paste0('Mean is ',round(mean(x_data),2),' sd is ',round(sd(x_data),2)),col='blue')+geom_vline(xintercept = round(mean(x_data),1),col='red')

					plot(density(x_data),xlab=input$InterestNode,main='Density')
					rug(jitter(x_data))
					abline(v=mean(x_data),col='blue')
					y_pos <- max(unclass(density(x_data))$y)/2
					text(mean(x_data),y_pos,paste0('mean is ',round(mean(x_data),4)*1000,' sd is ',round(sd(x_data),4)*1000),col='red')
				}else{
					print('No valid output')
				}
			#	p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeX)+facet_grid(eval(parse(text=facet)))
			#	p <- ggplot(ddd, aes(x=x_data,fill=y_data)) + geom_histogram() + xlab(input$ExamineNodeX) 		
			#	print(paste0("output is ",round(mean(result[,1]),2), " sd is ",round(sd(result[,1]),2)))
			#	print(paste0("output is for x_data ",round(mean(x_data),2), " sd is ",round(sd(x_data),2)))				
			#	print(p)
			#	paste0("output is ",round(mean(result[,1]),4), " sd is ",round(sd(result[,1]),4)) 
			}else if(type == 'd'&&length(evi_list)>0){
#				
				if(length(evi_list)>0){#to make sure that there is at least one evidence in the list
					evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
					print('entering d branch!!!')
					print(evi_vector)
					for(i in 1:length(evi_vector)){
						if(str_count(evi_vector[i],' ')==0){
							if(evi_string==''){
								evi_string <- paste0(evi_vector[i],'')
							}else {
							evi_string <- paste0(evi_string,'&',evi_vector[i])
							print(paste0('for d, the evi_string 1 is ',evi_string))
							}
						}else{
							temp <- str_replace(evi_vector[i],' ','&')
							if(evi_string==''){
								evi_string <- paste0(temp,'')
							}else {
								evi_string <- paste0(evi_string,'&',temp)
								print(paste0('for d, the evi_string 2 is ',evi_string))
							}						
						}
					}
					print(paste0('3 here the evi_string is ',evi_string))
					evi_string <- str_sub(evi_string,1)
					print(paste0('4 here the evi_string is ',evi_string))
					evi_string <- str_replace(evi_string,'<-','<  -')
					v <- get.node.info(nnodes,input$InterestNode)$values[1]
					#temp_interest <- paste0(input$InterestNode,"=='",v,"'")
					#print(paste0('temp_interest is ',temp_interest))
					print(paste0('evi_string is ',evi_string))
					eval_string <- paste0('cpdist(cgfit,node=\'',input$InterestNode,'\',evidence=(',evi_string,'))')
					print(eval_string)
					seed <- 1
					set.seed(seed)
					result <- eval(parse(text=eval_string))
					while(dim(result)<500&&seed<20){
						seed <- seed + 1
						set.seed(seed)
						tmp <- eval(parse(text=eval_string))
						result <-  rbind(result,tmp)
					}
					print(head(result))
					p.obj$data <<- result
					p.obj$x <<- with(result,get(input$InterestNode))
					head(p.obj$x)
					int_node <- input$InterestNode
			#	result <- cpquery(cgfit,eval(parse(text=temp_interest)),eval(parse(text=evi_string)))
				##If there is valid result, then print it out.
					if(nrow(result)>0){
						print(head(result))
						print(int_node)
						p <- ggplot(p.obj$data,aes(p.obj$x))+geom_histogram()+xlab(int_node)
						print(p)
						#print('output for d node is ',result)
						#paste0("output for", input$InterestNode ," is of being ",v, " is around ",round(result,3))
					}
				}
			}else{
				paste0("")
			}
			
			}
			})
		})


#	output$inference <- renderPlot({
#		input$Inference
#		isolate({
#			type <- get.node.info(nnodes,input$InterestNode)[['type']]
#			print(paste0("**********",input$InterestNode))
#			evi_string <<- ''
#			print(paste0('doing inference now with type ',type))
#			if(type == 'c'){
#				evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
#				print('entering c branch!!!')
#				for(i in 1:length(evi_vector)){
#					if(str_count(evi_vector[i],' ')==0){
#						evi_string <- paste0(evi_string,'&',evi_vector[i])
#					}else{
#						temp <- str_replace(evi_vector[i],' ','&')
#						evi_string <- paste0(evi_string,'&',temp)
#					}
#				}
#				evi_string <<- str_sub(evi_string,2)
#				print(paste0("the evi_string is ",evi_string))
#				result <- cpdist(cgfit,input$InterestNode,eval(parse(text=evi_string)))
#				paste0("output is ",round(mean(result[,1]),4), " sd is ",round(sd(result[,1]),4))
#				plot(density(result[,4]))
#				abline(v=round(mean(result[,1]),3))
#			}else if(type == 'd'){
##				
#				if(length(evi_list)>0){#to make sure that there is at least one evidence in the list
#				evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
#				print('entering d branch!!!')
#				for(i in 1:length(evi_vector)){
#					if(str_count(evi_vector[i],' ')==0){
#						evi_string <- paste0(evi_string,'&',evi_vector[i])
#					}else{
#						temp <- str_replace(evi_vector[i],' ','&')
#						evi_string <- paste0(evi_string,'&',temp)
#					}
#				}
#				evi_string <<- str_sub(evi_string,2)
#				v <- get.node.info(nnodes,input$InterestNode)$values[1]
#				temp_interest <<- paste0(input$InterestNode,"=='",v,"'")
#				result <- cpquery(cgfit,eval(parse(text=temp_interest)),eval(parse(text=evi_string)))
#				print('output for d node is ',result)
#				paste0("output for D node is ",result)
#				}#
#			}else{
#				paste0("")
#			}
#			
#			})#
#		})

output$selectUI_RT <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI4 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
#	selectInput("TargetNode",label="Target Node",choices = get.name.reactive(), selected="Defaults_1")
	selectInput("TargetNode",label="Target Node",choices = "Defaults_1", selected="Defaults_1")
#	selectInput("InterestNode",label="Node of Interests",choices = namel(get.name(nnodes)), selected="DefRate")
})



  output$ChooseTargetState <- renderUI({
  	print('enter choose state component')
  	#selectInput("selection","Please do your selection",choose_states
  	if(length(input$TargetNode)>0){
  	print(paste0('Target Node is ',input$TargetNode))
  	tnode <- get.node.info(nnodes,input$TargetNode)
  	data <- with(ddd, get(input$TargetNode))
  	print('finish choose target state soon! ')
  	switch(get.node.info(nnodes,input$TargetNode)[['type']],
  		"c" = c(sliderInput(inputId = "minValue",
                  label = "Start of the data range",
                 # min = round(min(data),4),
                  min = ifelse(round(min(data),4)>0,round(min(data),4),0),
                  max = round(max(data),4),
                  value = round(median(data),4),
                  step = round((max(data)-min(data))/200,4)
      ),
sliderInput(inputId = "maxValue",
                  label = "end of the data range",
                  min = ifelse(round(min(data),4)>0,round(min(data),4),0),
                  max = round(max(data),4),
                  value = round(median(data),4),
                  step = round((max(data)-min(data))/200,4)
      )
    ),
      "d" = selectInput("TargetChoice",label="Choose State",
   choices=tnode[['values']])

  		)
  }
  })

##To get the best configuration for parents node
#output$BestConf <- renderTable({
#		input$Action
#		isolate({
#			isValidValue <- FALSE
#			targetStateStr <- ''
#			allButOne <- NULL
#			if(length(input$minValue)>0){
#				print(paste0('In BestConf, the target node is ',input$TargetNode,' with value range ',input$minValue,' and ',input$maxValue))
#				if(input$minValue>=input$maxValue){
#					isValidValue <- FALSE
#				}else{
#					allNames <- get.name.reactive()#allNames<-namel(colnames(ddd))
#					if(!(input$TargetNode %in% allNames)){
#						isValidValue <- FALSE
#					}else{
#						isValidValue <- TRUE
#						allNames[[input$TargetNode]] <- NULL
#						allButOne <- unlist(lapply(allNames,function(x) x[[1]]))
#						names(allButOne) <- NULL
#					}
#				}
#			}
#			if(!isValidValue){
#				notice<-as.data.frame('Please input valid data')
#				colnames(notice)<-'Error'	
#				notice
#			}else{
#				evi_string <- paste0('(',input$TargetNode,'>',input$minValue,'&',input$TargetNode,'< ',input$maxValue,')')
#			#	querynodes <- paste(allButOne,collapse = '","')
#				querynodes <- get.parents.by.childname(input$TargetNode)
#				querynodes <- paste(querynodes,collapse = '","')
#			#	querynodes <- paste0('c("',querynodes,'")')
#				eval_string <- paste0('cpdist(cgfit,c("',querynodes,'"),',evi_string,')')
#				print('in best conf')
#				print(eval_string)
#				result <- eval(parse(text=eval_string))
#				print(head(result))
#				result_bin<<-lapply(result,function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
#				result_bin_df <- as.data.frame(result_bin)
#			### Do try to run the following code, machine will crashed!	
#		#		result_count_df <- as.data.frame(table(result_bin)) 
#				result_count_df <- as.data.frame(table(result_bin_df))
#				print(head(result_count_df))
#				bestConfName <- colnames(result_count_df)
#				print(result_count_df[which.max(result_count_df$Freq),])
#				result_count_df[which.max(result_count_df$Freq),]
#			#	paste((result_count_df[which.max(result_count_df$Freq),]))
#			}
#		})
#	})

####The above code is to do best conf for parents node, below code is for best conf for all nodes in networks
output$BestConf <- renderTable({
		input$Action
		isolate({
			isValidValue <- FALSE
			targetStateStr <- ''
			allButOne <- NULL
			visit_list<-list()
			#unvisit_list<-list()
			best_conf<-list()#
#check whether the value exist?
			if(length(input$minValue)>0){
				print(paste0('In BestConf, the target node is ',input$TargetNode,' with value range ',input$minValue,' and ',input$maxValue))
#check whether the minValue is less thanmaxValue, i.e. it is a valid values
				if(input$minValue>=input$maxValue){
					isValidValue <- FALSE
				}else{
#if it is valid values, then check the target node exists in the network
					allNames <- get.name.reactive()#allNames<-namel(colnames(ddd))
					if(!(input$TargetNode %in% allNames)){
						isValidValue <- FALSE
					}else{
#if all are valid, retrieve all nodes except target node
						isValidValue <- TRUE
						allButOne <- allNames
						allButOne[[input$TargetNode]] <- NULL
						visit_list[[length(visit_list)+1]] <- input$TargetNode
						best_conf[[input$TargetNode]] <- paste0('(',input$minValue,',',input$maxValue,']')
						allButOne <- unlist(lapply(allButOne,function(x) x[[1]]))
						names(allButOne) <- NULL##unname(allButOne)
					}
				}
			}##
			if(!isValidValue){
				notice<-as.data.frame('Please input valid data')
				colnames(notice)<-'Error'	
				notice
			}else{
			#   compile the child parents list
				child_parents_list <- list()
				for(i in allButOne){
					parents<-get.parents.by.childname(i)
					if(length(parents)>0){
						child_parents_list[[i]] <- parents
					}
				}
				inference_from_child <- list()
			#   retrieve all possible data for the target node given specific range.	
				evi_string <- paste0('(',input$TargetNode,'>',input$minValue,'&',input$TargetNode,'< ',input$maxValue,')')
				querynodes <- paste(allButOne,collapse = '","')
			#	querynodes <- get.parents.by.childname(input$TargetNode)
			#	querynodes <- paste(querynodes,collapse = '","')
			#	querynodes <- paste0('c("',querynodes,'")')
				eval_string <- paste0('cpdist(cgfit,c("',querynodes,'"),',evi_string,')')
				print('in best conf')
				print(eval_string)
				seed <- 1
				set.seed(seed)
				result <- eval(parse(text=eval_string))
				while(dim(result)<500&&seed<20){
					seed <- seed + 1
					set.seed(seed)
					tmp <- eval(parse(text=eval_string))
					result <-  rbind(result,tmp)
				}
				print(head(result))
			#   discretize the parents node for plotting purpose, note that since we use discrete variable now, so no need to cut it. the next line need update!
			#	something goes wrong here!!!
			#	result_bin <<- lapply(result[,colnames(result) %in% get.parents.by.childname(input$TargetNode)],function(x) ifelse(!is.factor(x),cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)),x))
		
				result_bin <<- result[,colnames(result) %in% get.parents.by.childname(input$TargetNode)]

			#	result_bin <<- result[,colnames(result) %in% get.parents.by.childname(input$TargetNode)]
			#   discretize the all nodes for test purpose need update as well
			#	result_bin_full <- lapply(result,function(x) ifelse(!is.factor(x),cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)),x))
			#	result_bin_df_full <- as.data.frame(result_bin_full)
				result_bin_df_full <- result
				result_bin_full <- result
				factorVar <- sapply(result_bin_full,function(x) is.factor(x))
				if(!all(factorVar)){
					result_bin_full[!factorVar] <- sapply(result_bin_full[!factorVar],function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
				}

			#   First get the parents node bin distribution
				result_bin_df <- result_bin_full
				result_bin_df <- result_bin_df[,colnames(result_bin_df) %in% get.parents.by.childname(input$TargetNode)]	
			#	result_count_df <- as.data.frame(table(result_bin)) Do try to run the following code, machine will crashed!
				result_count_df <- as.data.frame(table(result_bin_df))
				print(head(result_count_df))
				bestConfName <- colnames(result_bin_df)
				bestSet <- result_count_df[which.max(result_count_df$Freq),]
				print('best set currently is ')
				print(bestSet)
			#   store the visit_list and best configuration for the visit_list
				for(i in bestConfName){
					if(i!='Freq'&&!(i %in% visit_list)){
						visit_list[[length(visit_list)+1]] <- i
			#			best_conf[[length(best_conf)+1]] <- as.character(bestSet[[i]])
						best_conf[[i]] <- as.character(bestSet[[i]])
						print('best item is')
						print(best_conf[[i]])
						parents <- get.parents.by.childname(i)
						if(length(parents)>0){
							inference_from_child[[length(inference_from_child)+1]] <- i ##the next search start from i
						}
					}
				}#
				total <- 0
				while(length(visit_list)<length(allNames)&&total<10){#if we have not go through all nodes and we got some child nodes.
					inference_from_child_bak <- inference_from_child##copy by value, not reference???
					print(inference_from_child)
					print('***************')
					if(length(inference_from_child_bak)<1){
						total<-total+1
						break
					}else{
					for(ii in 1:length(inference_from_child_bak)){#
						seed_node <- inference_from_child_bak[[ii]]
						#inference_from_child[[i]] <- NULL#
						print(paste0('@@@ processing seed node :',seed_node))
						print(paste0('--- For now the inference from child list has ',inference_from_child_bak))
						remove_index <- -1
					    for(i in 1:length(inference_from_child)){
      						if(inference_from_child[[i]]==seed_node){
        						remove_index <- i
      						}
    					}
    					if(remove_index > 0){
    						inference_from_child[[remove_index]] <- NULL
    					}#

						parents <- child_parents_list[[seed_node]]
						interested_parents <- parents
						result_bin_df <- result_bin_df_full#

						if(!all(parents %in% visit_list)){
							for(p in parents){
								if(p %in% visit_list){
									interested_parents <- interested_parents[!interested_parents %in% p]#if parent is in visited list, add it to constraints and remove from interested parents node
									result_bin_df <- subset(result_bin_df,result_bin_df[,p]==best_conf[[p]])
								}
							}#
							result_bin_df <- subset(result_bin_df,result_bin_df[,seed_node]==best_conf[[seed_node]])##this can be problematic
							result_bin_df <- subset(result_bin_df,select = interested_parents)
							#result_bin_df <- result_bin_df[,interested_parents]
							result_count_df <- as.data.frame(table(result_bin_df))
							bestConfName <- colnames(result_bin_df)
							bestSet <- result_count_df[which.max(result_count_df$Freq),]#
							index <- 1#
							for(j in bestConfName){
								if(j!='Freq'&&!(j %in% visit_list)){
									visit_list[[length(visit_list)+1]] <- j
				#					best_conf[[length(best_conf)+1]] <- as.character(bestSet[[i]])
									if(!is.null(bestSet[[j]])){#2 or more ways table
										best_conf[[j]] <- as.character(bestSet[[j]])
									}else{#1 ways table
										best_conf[[j]] <- as.character(bestSet[,index])
										index <- index + 1
									}
									parents <- get.parents.by.childname(j)
									if(length(parents)>0&&!all(parents %in% visit_list)){
										inference_from_child[[length(inference_from_child)+1]] <- j ##the next search start from j
									}
								}
							}
						}
					}
					print('---the best conf so far is ')
					print(best_conf)
					}
					total <- total + 1
					print(paste0('!!!! total is ',total))
				}
				best_conf_table<-as.data.frame(unlist(best_conf))#
				names(best_conf_table)<-'Range'
				best_conf_table
			}#
			})
	})

output$BestConfPlot <- renderPlot({
		input$Action
		isolate({
			print('In best conf plot, result bin is as follows:')
			print(head(result_bin))
			print(str(result_bin))
#			factorVar <- sapply(result_bin,function(x) is.factor(x))
#			if(!all(factorVar)){
#				#result_bin[!factorVar] <- sapply(result_bin[!factorVar],function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
#				result_bin[!factorVar] <- cut(result_bin[!factorVar],12)
#			}
			for(cname in colnames(result_bin)){
				if(is.numeric(result_bin[,cname])){
					result_bin[,cname] <- cut(result_bin[,cname],10)
				}
			}


			if(length(input$minValue)>0){
				#result_bin is discretized parents node for plotting purpose
				#since we do contour plot, we need two variables, so length(result_bin)>1
				if(length(result_bin)>1){
					print(paste0('the length of the result bin is ',length(result_bin)))
					result_bin_bak <- result_bin
					print(str(result_bin_bak))
#					for(i in 1:length(result_bin_bak)){
#						##add levels to discreted factor and convert it back to numeric
#						levels(result_bin_bak[[i]])<-seq(1:length(levels(result_bin_bak[[i]])))
#						result_bin_bak[[i]] <- as.numeric(as.character(result_bin_bak[[i]]))
#					}
					for(i in 1:dim(result_bin_bak)[2]){
						##add levels to discreted factor and convert it back to numeric
						levels(result_bin_bak[,i])<-seq(1:length(levels(result_bin_bak[[i]])))
						result_bin_bak[,i] <- as.numeric(as.character(result_bin_bak[[i]]))
					}

					print('result bin bak is as follows:')
					print(str(result_bin_bak))
					##get freq table
					result_bin_table <- table(result_bin_bak[[1]],result_bin_bak[[2]])
					##convert to long format
					result_bin_3d <- melt(result_bin_table)
					colnames(result_bin_3d)<- c("x","y","z")
					print(result_bin_3d[which.max(result_bin_3d$z),])
					v <- ggplot(result_bin_3d,aes(x,y,z=z))
					p <- v+geom_tile(aes(fill=z))+stat_contour()+scale_size_area()+xlab(colnames(result_bin_bak)[1])+ylab(colnames(result_bin_bak)[2])
					print(p)
				}
			}
		})
	})

#  output$txt <- renderText({
#	#nAttrs$fillcolor
#	print('enter txt output component')
#	input$EvidenceNode	
#  })

#  output$downloadData <- downloadHandler(
#    filename = function() {
#    print('enter download Data component')
#    paste('data-', Sys.Date(), '.Rdata', sep='')
#  },
#  content = function(file) {
#  	print('enter content Data component')
#    save(nodes, file=file)
#  }
#    )

 })

