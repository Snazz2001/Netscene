require(methods)
require(bnlearn)
require(ggplot2)
###define two type of nodes, continous node and discrete node###
setClass("GRNode_c",
         representation(name="character",model="list",values="vector",parents="vector",children="vector"))
setClass("GRNode_d",
         representation(name="character",model="list",values="vector",parents="vector",children="vector"))

networkstring="[hpi][ltv][dtv|hpi:ltv][vintage|ltv][BoeIR][IntGearing|BoeIR:ltv][Unemp][exog|Unemp:IntGearing][maturity][DefRate|maturity:exog]"

###fit the network based on the configuration###
fit.net.z=function(nnodes,net){
print('calling fit.net.z function')
net_spec <- list()
for(i in 1:length(nnodes)){
  net_spec[[nnodes[[i]]@name]]<-nnodes[[i]]@model$model
}
print('done fit.net.z!')
  cgfit <- custom.fit(net,dist=net_spec)	
}

###find the model based on name###
get.model=function(nnodes,name){
	print('calling get model method')
	for(i in 1:length(nnodes)){
		if(nnodes[[i]]@name==name){
			node <- nnodes[[i]]
		}
	}
	print('done with get model method')
	node
}

###get all names in the network###
get.name=function(nnodes){
		print('calling get name method')
   names <- c()
   for(i in 1:length(nnodes))
   {
   	names<-c(names,nnodes[[i]]@name)
   }
   print('done with get name method')
   names
}

###get node type from networks, why not add additonal attribution in class with type?###
get.node.info=function(nnodes,name){
	#Store all node information in nnodes list
	#
	if(length(name)==0){
		name = 'hpi'
	}

	print('calling get node info method')	
	type <- 'c'
	values <- c()
	print(paste('name is',name))
	for(i in 1:length(nnodes)){
		if(name==nnodes[[i]]@name){
			print(paste0('nnodes name is ',nnodes[[i]]@name))
			if(class(nnodes[[i]])[1]=='GRNode_d'){
				type <- 'd'
			}else{
				type <- 'c'
			}
			values <- nnodes[[i]]@values
		}
	}
	 print('done with get node info method')
	list(name=name,type=type,values=values)
}

###extract the parent node information based on its name###
get.parents.info=function(nnodes,parentlist){
	#nanme is the name for new node
	#type is the type for new node, either c or d
	#parentlist is the parents node for the new node, user specify it by name
print('calling get parents info info method')
print(paste0('parentlis is ',parentlist))
	###node has maximum two parents###
	if(length(parentlist)>2){
		return(NA)
	}
	nodes_spec <- list()
	###extract the name,type and values of parents node
	for(n in parentlist){
		nodes_spec[[length(nodes_spec)+1]]<-get.node.info(nnodes,n)	
	}
		 print('done with get parents info info method')
	nodes_spec
	#types = sapply(nodes_spec,function(x) x$type)
	#values = sapply(nodes_spec,function(x) x$values)

	#set.up.network(name,parentlist)

	#set.up.model(name,type,nodes_spec)
}

###To build the update network string, support add new node only###
construct.networkstring=function(name,nodes_spec,networkstring){
print('calling get construct.networkstring method')	
	#nodes_spec is the info for parents node
	names<-sapply(nodes_spec,function(x) x$name)
	pname<-paste0(names,collapse=':')
	node_conf<-paste0('[',name,'|',pname,']',collapse="")

	###To prevent the duplicated specification of the same node configuration###
	if(!grepl(node_conf,networkstring,fixed=TRUE))
	{
		networkstring<-paste0(networkstring,node_conf)
	}
	print('done with get construct. method')
	networkstring
}

###set up the conf string for
conf.new.node=function(name,type,nodes_spec,weights=c()){##need to provide the new model conf, use default setting now.
	###extract the parents info
	#nodes_spec is the info for parents node
	print('calling conf.new.node method')
	pnames <- sapply(nodes_spec,function(x) x$name)
	ptypes <- sapply(nodes_spec,function(x) x$type)
	pvalues <- sapply(nodes_spec,function(x) x$values)
	print(paste0('in conf new node function, the weights are ',weights))
	###if new node type is c### 
	if(type=='c'){
		if(length(ptypes)==2){
			if(ptypes[1]=='c'&&ptypes[2]=='c'){
				conf_str <- set.cccmodel.string(name,pnames,vec=weights)		
				}else if(ptypes[1]=='c'&&ptypes[2]=='d'){
					conf_str <- set.ccdmodel.string(name,pnames,vec=weights[1:4],sd=weights[5:6])
					}else if(ptypes[1]=='d'&&ptypes[2]=='c'){
						conf_str <- set.cdcmodel.string(name,pnames,vec=weights[1:4],sd=weights[5:6])
					}else{
						conf_str <- ''
					}
		}else if(length(ptypes)==1){
			###Need add new function for 1 parent
			if(ptypes=='c'){
				conf_str <- set.ccmodel.string(name,pnames,vec=weights)
			}else {
				conf_str <- set.cdmodel.string(name,pnames,vec=weights)
			}

		}

	}else{
		if(length(ptypes)==2){
		if(ptypes[1]=='d'&&ptypes[2]=='d'){
			conf_str <- set.dddmodel.string(name,pnames,vec=weights)
			}else{
				###it is not valid configuration###
				conf_str <- ''
			}
			}else if(length(ptypes)==1){
				###need add new function for 1 parent
				if(ptypes=='d'){
					conf_str <- set.ddmodel.string(name,pnames,vec=weights)
				}
			}
	}
	print('done with conf.new.node method')
	conf_str
}


set.cdmodel.string = function(name,pnames,vec=c(1.1,2.2,1,1)){
	print('calling set cd model ')
	model_name_str <- paste0(name,'_model')
	vec_str <- paste0('c(',paste(vec[1:2],collapse=','),')')
	sd_str <- paste0('c(',paste(vec[3:4],collapse=','),')')
	list_str <- paste0("list(coef=matrix(",vec_str,", ncol = 2, dimnames = list(c('(Intercept)'),",pnames,"=c('LOW','HIGH'))),sd=",sd_str,")")
	print('done setting cd model')
	conf_str <- paste(model_name_str,list_str,sep='=')
}


##list(coef=c("(Intercept)"=3,"ltv"=1.6),sd=1.5)
set.ccmodel.string = function(name,pnames,vec=c(1,1.5,2)) {
	print('calling set cc model')
	model_name_str <- paste0(name,'_model')
	weight_str <- paste0("'(Intercept)'=",vec[1],",'",pnames,"'=",vec[2])
	list_str <- paste(paste0('list(coef=c(',weight_str,')'),paste0('sd=',vec[3],")"),sep=',')
	print('done set cc model')
	conf_str <- paste(model_name_str,list_str,sep='=')
}

set.ddmodel.string=function(name,pnames,vec=c(0.2,0.8,0.6,0.4)){
	print('calling set dd model')
	model_name_str <- paste0(name,'_model')
	vec_str <- paste0('c(',paste(vec,collapse=','),')')
	mat_str <- paste0("matrix(",vec_str,", ncol = 2, dimnames = list(",pnames,"=c('LOW','HIGH'),",name,"=c('LOW','HIGH')))")
	print('done setting dd model')	
	conf_str <- paste(model_name_str,mat_str,sep='=')
}


###
set.cccmodel.string=function(name,pnames,vec=c(3,1,-1,1.5)){
	print('calling set model string 1')
	model_name_str <- paste0(name,'_model')
	weight_str <- paste(paste0("'(Intercept)'=",vec[1]),paste0("'",pnames[1],"'=",vec[2]),paste0("'",pnames[2],"'=",vec[3]),sep=',')
	list_str <- paste(paste0('list(coef=c(',weight_str,')'),paste0('sd=',vec[4],")"),sep=',')##new added last ")"
	print(paste0('in set ccc, the mat_list_str is ',list_str))
	conf_str <- paste(model_name_str,list_str,sep='=')
	print('done set model string 1')
	conf_str
}

#dtv_model = list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
#                              dimnames = list(c("(Intercept)", "ltv"), NULL)),
#                sd = c(0.3, 0.6))
set.cdcmodel.string=function(name,pnames,vec=c(1.2,2.3,3.4,3.5),sd=c(0.3,0.5)){
	print('calling set model string 2')
	model_name_str <- paste0(name,'_model')
	vec_str <- paste0('c(',paste(vec,collapse=','),')')
	sd_str <- paste0('c(',paste(sd,collapse=','),')')
	mat_str <- paste0("list(coef=matrix(",vec_str,", ncol = 2, dimnames = list(c('(Intercept)','",pnames[1],"'),NULL)),sd=",sd_str,")")
	print(paste0('in set cdc, the mat_str is ',mat_str))
	conf_str <- paste(model_name_str,mat_str,sep='=')
	print('done set model string 2')
	conf_str
}

set.ccdmodel.string=function(name,pnames,vec=c(1.2,2.3,3.4,3.5),sd=c(0.3,0.5)){
	print('calling set model string 3')
	model_name_str <- paste0(name,'_model')

	vec_str <- paste0('c(',paste(vec,collapse=','),')')
	sd_str <- paste0('c(',paste(sd,collapse=','),')')

	mat_str <- paste0("list(coef=matrix(",vec_str,", ncol = 2, dimnames = list(c('(Intercept)','",pnames[2],"'),NULL)),sd=",sd_str,")")
	print(paste0('in set ccd, the mat_str is ',mat_str))
	conf_str <- paste(model_name_str,mat_str,sep='=')
	print('done set model string 3')
	conf_str
}

set.dccmodel.string=function(name,pnames,vec=c(1.2,2.3,3.4,3.5),sd=c(0.3,0.5)){###not in use??
	print('calling set model string 4')
	model_name_str <- paste0(name,'_model')
	vec_str <- paste0('c(',paste(vec,collapse=','),')')
	sd_str <- paste0('c(',paste(sd,collapse=','),')')
	mat_str <- paste0("list(coef=matrix(",vec_str,", ncol = 2, dimnames = list(c('(Intercept)','",pnames[2],"'),NULL)),sd=",sd_str,")")
	print(paste0('in set dcc, the mat_str is ',mat_str))	
	conf_str <- paste(model_name_str,mat_str,sep='=')
	print('done set model string 4')
	conf_str
}

#cptT <- matrix(c(0.05, 0.95, 0.01, 0.99), 
#               ncol=2, dimnames=list("T"=yn, "A"=yn))
set.dddmodel.string=function(name,pnames,vec=c(0.8, 0.2, 0.5, 0.5, 0.1, 0.9, 0.7, 0.3)){
	print('calling set model string 5')
	model_name_str <- paste0(name,'_model')
	vec_str <- paste0('c(',paste(vec,collapse=','),')')	
	cpt_str <- paste0('array(',vec_str,",dim=c(2,2,2),dimnames=list(",pnames[1],"=c('LOW','HIGH'),",pnames[2],"=c('LOW','HIGH'),",name,"=c('LOW','HIGH')))")	
#This can be some issue here	cpt_str <- paste0('array(',vec,",dim=c(2,2,2),dimnames=list(",pnames[1],"=",eval(parse(text=paste0(pnames[1],'_model'))),",",pnames[2],"=",eval(parse(text=paste0(pnames[2],'_model'))),",",name,"=c('HIGH','LOW')))")	
	print(paste0('in set ddd, the cpt_str is ',cpt_str))	
	conf_str <- paste(model_name_str,cpt_str,sep='=')
	print('done set model string 5')
	conf_str
}

#BoeIR = new("GRNode_d",name="BoeIR",model=list(model=BoeIR_model),values=c("UP","DOWN"),parents=c(NA,NA),children=c(NA,NA))
#IntGearing = new("GRNode_c",name="IntGearing",model=list(model=IntGearing_model),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))
newnode.class.string=function(name,type){
	print('calling newnode class string method')
	print(paste0('type is ',type))
	if(type=='d'){
		class_str <- paste0(name,"=new('GRNode_d',name='",name,"',model=list(model=",paste0(name,'_model'),"),values=c('LOW','HIGH'),parents=c(NA,NA),children=c(NA,NA))")
		}else{
		class_str <- paste0(name,"=new('GRNode_c',name='",name,"',model=list(model=",paste0(name,'_model'),"),values=c(-10000,10000),parents=c(NA,NA),children=c(NA,NA))")	
		}
		print(paste0('done newnode class string method with class_str is ',class_str))
		class_str
}

###update the nnodes list###
#update.nodes.list=function(nnodes,name){
#	nnodes[[length(nnodes)+1]]<-eval(parse(text=name))
#	nnodes
#}

###update the nnodes list###
update.nodes.list=function(nnodes,name){
	print('calling update nodes list ')
	nnodes[[length(nnodes)+1]]<-name
	print('done update nodes list')
	nnodes
}


###inference the data###
query.node=function(cgfit,node,value='',events){
	print('calling query node ')
	node.info <- get.node.info(nnodes,node)
	if(node.info$type=='d'){
		events_string <- ifelse(events=='','TRUE',events)
		query_str <- paste0("result<-cpquery(cgfit,(",node,"=='",value,"'),",events_string,")")
		}else{
		query_str <- paste0("result<-cpdist(cgfit,'",node,"',",events,")")
		}
	print('done query node')
	query_str
}



