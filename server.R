
require("shiny")
setwd("C:\\Projects\\BayesianNetwork\\Netscene1110")
require(bnlearn)
require(Rgraphviz)
require(vcd)
require(ggplot2)
require(stringr)
#include the definition of the class nodes
source("C:\\Projects\\BayesianNetwork\\Netscene1110\\Netscene\\GraphRiskNode_class_z.R")


namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}

pn<-{}
nodes<-{}
yn   <- c(0,1)
decl <- seq(0.1,1.1,1)
dech <- seq(0.1,1.1,1)

EEselNode<-1

evi_list <<- list()

###specify the node configure
hpi_model <- matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
ltv_model <- list(coef = c("(Intercept)" = 2), sd = 1)
dtv_model <- list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
                              dimnames = list(c("(Intercept)", "ltv"), NULL)),
                sd = c(0.3, 0.6))
vintage_model <- list(coef=c("(Intercept)"=3,"ltv"=1.6),sd=1.5)
BoeIR_model <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
IntGearing_model <- list(coef = matrix(c(-1.2, 1.1, 2, 2.6), ncol = 2,
                              dimnames = list(c("(Intercept)", "BoeIR"), NULL)),
                sd = c(0.13, 0.36))
Unemp_model <- list(coef=c("(Intercept)"=0.5),sd=0.3)
exog_model <- list(coef=c("(Intercept)"=3,"Unemp"=1.6,"IntGearing"=-0.6),sd=1.5)
maturity_model <- matrix(c(0.7, 0.3), ncol = 2, dimnames = list(NULL, c("YES", "NO")))
DefRate_model <- list(coef = matrix(c(-1.2, 1.1, 2, -2.6), ncol = 2,
                                  dimnames = list(c("(Intercept)", "maturity"), NULL)),
                    sd = c(0.13, 0.36))
###note for 2 discrete nodes, we need to use 8 parameters(4 can be derived from another four)###

###wrap node configure into GRNode class, GRNode_d is for discrete node and GRNode_c is for continous node, parents,children here works
###as place holder
hpi <- new("GRNode_d",name="hpi",model=list(model=hpi_model),values=c("LOW", "HIGH"),parents=c(NA,NA),children=c(NA,NA))
ltv <- new("GRNode_c",name="ltv",model=list(model=ltv_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
dtv <- new("GRNode_c",name="dtv",model=list(model=dtv_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
vintage <- new("GRNode_c",name="vintage",model=list(model=vintage_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
BoeIR <- new("GRNode_d",name="BoeIR",model=list(model=BoeIR_model),values=c("LOW", "HIGH"),parents=c(NA,NA),children=c(NA,NA))
IntGearing <- new("GRNode_c",name="IntGearing",model=list(model=IntGearing_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
Unemp <- new("GRNode_c",name="Unemp",model=list(model=Unemp_model),values=c(0,1),parents=c(NA,NA),children=c(NA,NA))
exog <- new("GRNode_c",name="exog",model=list(model=exog_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
maturity <- new("GRNode_d",name="maturity",model=list(model=maturity_model),values=c("YES","NO"),parents=c(NA,NA),children=c(NA,NA))
DefRate <- new("GRNode_c",name="DefRate",model=list(model=DefRate_model),values=c(0,1),parents=c(NA,NA),children=c(NA,NA))

###How to setup the model structure.
networkstring <- "[hpi][ltv][dtv|hpi:ltv][vintage|ltv][BoeIR][IntGearing|BoeIR:ltv][Unemp][exog|Unemp:IntGearing][maturity][DefRate|maturity:exog]"
net <- model2network(networkstring)

###put all the GRNode into one list
nnodes <- list(hpi,ltv,dtv,vintage,BoeIR,IntGearing,Unemp,exog,maturity,DefRate)
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

output$distPlot <- renderPlot({ #renderGvis
	print (input$ChartChoice)
	if ((input$ChartChoice == "Scatter Plot" || input$ChartChoice == "Heat Map") && (length(input$ExamineNodeX)>0)){
		###To bin continous variable into different group###
		print('scatterplot starts! ')
		print(paste0('nodex is ',input$ExamineNodeX))
		print(paste0('nodey is ',input$ExamineNodeY))
		
		x <- get.node.info(nnodes,input$ExamineNodeX)
		y <- get.node.info(nnodes,input$ExamineNodeY)

		x_type <- x[['type']]
		y_type <- y[['type']]

		#x_data <- with(ddd,get(input$ExamineNodeX))
		#y_data <- with(ddd,get(input$ExamineNodeY))
		plot.obj<<-list()
		plot.obj$data <<- ddd

		plot.obj$x <<- with(plot.obj$data,get(input$ExamineNodeX))
		plot.obj$y <<- with(plot.obj$data,get(input$ExamineNodeY))

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

		if (input$ChartChoice == "Scatter Plot"){
			print("inside the scatterplot to see what variables are in ")
			print(get.name(nnodes))
			if (x_type=='c' && y_type=='c'){
				sunflowerplot(g_dat3[,1],g_dat3[,2],g_dat3[,3],main="Scatter Plot",
				xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY));
#				y11 <- paste0('x=',input$ExamineNodeX)
#				y22 <- paste0('y=',input$ExamineNodeY)
#				ggplot(ddd,aes(eval(parse(text=y11)),eval(parse(text=y22))))+geom_point()
			} else if(x_type=='d' && y_type=='d'){
				 fmla<-as.formula(paste0('~',input$ExamineNodeX,'+',input$ExamineNodeY))
				 mosaic(fmla,ddd,shade=TRUE,legend=TRUE)
			} else if(x_type=='d' && y_type=='c'){
				flm <- paste0('x=',input$ExamineNodeY)
				print(paste('flm is ',flm))
				facet <- paste0(input$ExamineNodeX,'~.')
				print(paste('00000',flm))
				print(paste('1111',facet))
				p<-ggplot(plot.obj$data,aes(x=plot.obj$y,fill=as.factor(plot.obj$x)))+geom_density(alpha=.75)+xlab(input$ExamineNodeY)				
			#	p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeY)+facet_grid(eval(parse(text=facet)))
			#	p <- ggplot(ddd, aes(x=y_data,fill=x_data)) + geom_histogram() + xlab(input$ExamineNodeY) 
				print(p)
				print('print!!!')
			} else {
				flm <- paste0('x=',input$ExamineNodeX)
				print(paste('flm here is ',flm))
				facet <- paste0(input$ExamineNodeY,'~.')
				print(paste('2222',flm))
				print(paste('3333',facet))
				p<-ggplot(plot.obj$data,aes(x=plot.obj$x,fill=as.factor(plot.obj$y)))+geom_density(alpha=.75)+xlab(input$ExamineNodeY)			
			#	p<-ggplot(ddd,aes(eval(parse(text=flm))))+geom_histogram()+xlab(input$ExamineNodeX)+facet_grid(eval(parse(text=facet)))
			#	p <- ggplot(ddd, aes(x=x_data,fill=y_data)) + geom_histogram() + xlab(input$ExamineNodeX) 		
				print(p)
				print('print!!!!!!!')
			}
		} 
		print('3333')
		if (input$ChartChoice == "Heat Map" && x_type=='c' && y_type=='c'){
			filled.contour(g_dat2,main="Heat Map",
				xlab=print(input$ExamineNodeX),ylab=print(input$ExamineNodeY))
		}
	}
			print('444')
	if(input$ChartChoice == "Histogram" && x_type=='c'){
		plot(hist(ddd[, input$ExamineNodeX]), main="Histogram", xlab=print(input$ExamineNodeX))
	}	
	
  })

  choose_states<-reactive({
#  	enode <<- get.node.info(nnodes,input$EvidenceNode)
#  	type <<- enode[['type']]
 # 	print(paste0('zheng it is ',type))

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
  	print('enter net plot 1 component')
    #print(input$id)
#		if(input$EnterOrRetract=="Enter"){
#			nAttrs$fillcolor[input$EvidenceNode]<<-"red"
#		}else{
#			nAttrs$fillcolor[input$EvidenceNode]<<-"white"
#		}	
	
		###plot the network, note here it is not updated yet. - zheng zhu
		#graphviz.plot(net)
		graphviz.plot(net.reactive())

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

###new net reactive function###
net.reactive <- reactive({
	if(length(input$ParentNodeList)>0){
	parents <- input$ParentNodeList
	name <- input$NewNodeName
	pnode_spec <- get.parents.info(nnodes,parents)
	networkstring <<- construct.networkstring(name,pnode_spec,networkstring)
	print(paste('network string is ',networkstring))
	net <- model2network(networkstring)
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
		isolate({if(input$NewNodeName!=""){
			##get the input from ui
			name <- input$NewNodeName
			parents <- input$ParentNodeList
			type <- input$ContinuousOrDiscrete
			print(paste('the type is ',type))
			print(paste('the name is ',name))
			##get the parent information
			pnode_spec <- get.parents.info(nnodes,parents)

			##update the network string and compile network
			networkstring <<- construct.networkstring(name,pnode_spec,networkstring)
			print(paste('network string is ',networkstring))
			net <<- model2network(networkstring)

			##set up the new node configure
			conf_str <- conf.new.node(name,type,pnode_spec)
			print(paste('conf str is ',conf_str))
			if(conf_str!=''){
				eval(parse(text=conf_str))
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

#			par2<-sapply(c("",parents),c,"+")
#			par3<-paste(par2[1:length(par2)-1],collapse="")
#			#

#			# Create the new node, and append it to the list, all nodes assumed to have 2 levels!
#			eval(parse(text=paste(
#			input$NewNodeName,"<<-new('GRNode',continuous_outcome=1,mean=1,sd=0.5,notes='a',cptable(~",input$NewNodeName,par3,",values=rep(1,2^(1+length(parents)) ), levels=yn));",
#			"nodesloc<-c(nodesloc,list(",input$NewNodeName,"))")))#

#			print(paste(
#			input$NewNodeName,"<<-new('GRNode',continuous_outcome=1,mean=1,sd=0.5,notes='a',cptable(~",input$NewNodeName,par3,",values=rep(1,2^(1+length(parents)) ), levels=yn));",
#			"nodesloc<-c(nodesloc,list(",input$NewNodeName,"))"))#

#			# print(nodesloc)#

#			plistloc <- compileCPT(lapply(nodesloc,as,"cptable"))
#			
#			print("got here 5")#

#			pnloc  <- grain(plistloc)
#			pncloc <- compile(pnloc, propagate=TRUE)
#			dddloc<-simulate2(pncloc,n=1000)#

#			assign("nodes",nodesloc,.GlobalEnv) 
#			assign("ddd",dddloc,.GlobalEnv) 
#			assign("pnc",pncloc,.GlobalEnv)  
#			assign("pn",pnloc,.GlobalEnv)  
#			assign("plist",plistloc,.GlobalEnv)         			
		}
		graphviz.plot(net)})
		print("Render Build Plot") 	
  })


  get.name.reactive<-reactive({
  	var.opts <- namel(colnames(ddd))
  	if(input$CreateNode){
  		var.opts <- namel(colnames(ddd))
  		}
  	var.opts
  	})

output$EnterParam <- renderUI({
	input$CreateNode
	isolate({
		if(input$ContinuousOrDiscrete=='c'&&length(input$ParentNodeList)>0){
			print(paste0('node is c'))



			}else if(input$ContinuousOrDiscrete=='d'&&length(input$ParentNodeList)>0){
				print(paste0('node is d'))
				pnode_spec <- get.parents.info(nnodes,parents)
				ptypes <- sapply(pnodes_spec,function(x) x$type)
				if(length(input$ParentNodeList)==2){
					if(ptypes[1]=='d'&&ptypes[2]=='d'){
						
					}
				}

			}else{
				print('')
			}
		})	

	})


output$selectUI1 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI1 is working ',colnames(ddd)))
	selectInput("ExamineNodeX",label="Examine",choices = get.name.reactive(),selected="IntGearing")
#	selectInput("ExamineNodeX",label="Examine",choices = var.opts, selected="IntGearing")
})#

output$selectUI2 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI2 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("ExamineNodeY",label="Examine",choices = get.name.reactive(), selected="vintage")
})

output$selectUI3 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI2 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("EvidenceNode",label="Select Node",choices = get.name.reactive(), selected="hpi")
})

# Code to deal with entering evidence on Enter Evidence page, need to consider the binary or continuous case separately - zheng
  output$ChooseState <- renderUI({
  	print('enter choose state component')
  	#selectInput("selection","Please do your selection",choose_states
  	print(paste0('evidence node is ',input$EvidenceNode))
  	enode <<- get.node.info(nnodes,input$EvidenceNode)
  	data <- with(ddd, get(input$EvidenceNode))
  	print('finish choose state soon! ')
  	switch(choose_states(),
  		"c" = c(sliderInput(inputId = "mincimumValue",
                  label = "Start of the data range",
                  min = round(min(data),3),
                  max = round(max(data),3),
                  value = round(median(data),3),
                  step = round((max(data)-min(data))/200,3)
      ),
sliderInput(inputId = "maximumValue",
                  label = "end of the data range",
                  min = round(min(data),3),
                  max = round(max(data),3),
                  value = round(median(data),3),
                  step = round((max(data)-min(data))/200,3)
      )
    ),
      "d" = selectInput("EvidenceChoice",label="Choose State",
   choices=enode[['values']])

  		)
  })

###new added
	output$evidences <- renderText({
		input$Update
		isolate({
		if(input$EnterOrRetract == 'Enter')
		{	

			isExist <<- FALSE
			if(length(evi_list)>0){
				for(i in 1:length(evi_list)){
  					if(evi_list[[i]]$name==input$EvidenceNode){
      					isExist <<- TRUE
  					}
				}
			}
			if(length(input$EvidenceNode)>0&&choose_states()=="c"&&!isExist){
				name_value_pairs <- list('name'=input$EvidenceNode,'type'='c',value=list(input$mincimumValue,input$maximumValue))
				evi_list[[length(evi_list)+1]] <<- name_value_pairs
			}else if(length(input$EvidenceNode)>0&&choose_states()=="d"&&!isExist){
				name_value_pairs <- list('name'=input$EvidenceNode,'type'='d',value=list(input$EvidenceChoice))
				evi_list[[length(evi_list)+1]] <<- name_value_pairs
			}
		}else {
			index_to_remove <- -999
			for(i in 1:length(evi_list)){
  				if(evi_list[[i]]$name==input$EvidenceNode){
      				index_to_remove<-i
  				}
			}
			if(index_to_remove>0){
  				evi_list[[index_to_remove]]<<-NULL
			}
		}
	#	unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"==",x$value),paste0(x$name,'=',x$value[[1]],'->',x$value[[2]]))))
		unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"==",x$value),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
#		print(evi_list)
#		evi_list
#		switch(choose_states(),
#			"c" = paste0(input$EvidenceNode,'=',input$mincimumValue,':',input$maximumValue),
#			"d" =  paste0(input$EvidenceNode,'=',input$EvidenceChoice))
		})
		})

	output$inference <- renderText({
		input$Inference
		isolate({
			type <- get.node.info(nnodes,input$InterestNode)[['type']]
			print(paste0("**********",input$InterestNode))
			evi_string <<- ''
			print(paste0('doing inference now with type ',type))
			if(type == 'c'){
				evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
				print('entering c branch!!!')
				for(i in 1:length(evi_vector)){
					if(str_count(evi_vector[i],' ')==0){
						evi_string <- paste0(evi_string,'&',evi_vector[i])
					}else{
						temp <- str_replace(evi_vector[i],' ','&')
						evi_string <- paste0(evi_string,'&',temp)
					}
				}
				evi_string <<- str_sub(evi_string,2)
				print(paste0("the evi_string is ",evi_string))
				result <- cpdist(cgfit,input$InterestNode,eval(parse(text=evi_string)))
				paste0("output is ",round(mean(result[,1]),4), " sd is ",round(sd(result[,1]),4))
			}else if(type == 'd'){
#				
				if(length(evi_list)>0){#to make sure that there is at least one evidence in the list
				evi_vector <- unlist(lapply(evi_list,function(x) ifelse(x$type=='d',paste0(x$name,"=='",x$value,"'"),paste(paste0(x$name,'>',x$value[[1]]),paste0(x$name,'<',x$value[[2]]),collapse='&'))))
				print('entering d branch!!!')
				for(i in 1:length(evi_vector)){
					if(str_count(evi_vector[i],' ')==0){
						evi_string <- paste0(evi_string,'&',evi_vector[i])
					}else{
						temp <- str_replace(evi_vector[i],' ','&')
						evi_string <- paste0(evi_string,'&',temp)
					}
				}
				evi_string <<- str_sub(evi_string,2)
				v <- get.node.info(nnodes,input$InterestNode)$values[1]
				temp_interest <<- paste0(input$InterestNode,"=='",v,"'")
				result <- cpquery(cgfit,eval(parse(text=temp_interest)),eval(parse(text=evi_string)))
				print('output for d node is ',result)
				paste0("output for D node is of being ",v, " is around ",round(result,3))
				}#
			}else{
				paste0("")
			}
			
			})#
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

output$selectUI4 <- renderUI({
#	nodes.name <- get.name(nnodes)
#	print('enter select  UI1')
	var.opts <- namel(colnames(ddd))
	print(paste0('select UI4 is working ',colnames(ddd)))
#	selectInput("ExamineNodeY",label="Examine",choices = var.opts, selected="vintage")
	selectInput("InterestNode",label="Node of Interests",choices = get.name.reactive(), selected="DefRate")
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

