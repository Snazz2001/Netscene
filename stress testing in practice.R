library(devtools)
install_github("0xdata/h2o/R/ensemble/h2oEnsemble-package")
install.packages('tidyr')
require(dplyr)
library('googleVis')
library(reshape2)

setwd('C:\\Projects\\BayesianNetwork\\Netscene')
library(shiny)
runApp()

b<-cpdist(cgfit,c("DTI_2","LTV_2","Spread_2","BoERates_2","Income_2","Inflation_2"),(Defaults_2>20))
library(MASS)
parcoord(b)

lay1<-cpdist(cgfit,get.parents.by.childname('Defaults_2'),Defaults_2>20)
range(lay1[,3])
lay11<-lapply(lay1,function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),20)))

lay11_df<-as.data.frame(lay11)
lay11_count_df<-as.data.frame(table(lay11_df))

levels(lay11$DTI_2)<-seq(1:length(levels(lay11$DTI_2)))
lay11$DTI_2<-as.numeric(as.character(lay11$DTI_2))

levels(lay11$LTV_2)<-seq(1:length(levels(lay11$LTV_2)))
lay11$LTV_2<-as.numeric(as.character(lay11$LTV_2))

levels(lay11$Spread_2)<-seq(1:length(levels(lay11$Spread_2)))
lay11$Spread_2<-as.numeric(as.character(lay11$Spread_2))

test<-table(lay11$DTI_2,lay11$LTV_2)
test3d<-melt(test)
testdf<-as.data.frame(test)
colnames(testdf)<-c("DTI_2","LTV_2","Freq")
colnames(testdf)<-c("x","y","z")
colnames(test3d)<-c("x","y","z")

v<-ggplot(test3d,aes(x,y,z=z))
v+geom_tile(aes(fill=z))+stat_contour()
py$ggplotly()

v<-ggplot(testdf,aes(x,y,z=z))
v+geom_tile(aes(fill=z))+stat_contour()
testdf$z <-testdf$z+50
v<-ggplot(testdf,aes(x,y,z=z))
v+geom_tile(aes(fill=z))+stat_contour()

lay11<-as.data.frame(lay11)
table(lay11[,1])
lay11.count<-as.data.frame(table(lay11))
cc<-lay11.count[which.max(lay11.count$Freq),]
val<-cc[1]
val = as.character(val[1,1])
val = gsub("(","",val,fix=TRUE)
val = gsub("]","",val,fix=TRUE)
val = unlist(str_split(val,','))
val = as.numeric(val)
?cut
b_bin<-lapply(b,function(x) cut(x,10,labels=1:10))
b_bin<-lapply(b,function(x) cut(x,10))
b_bin_df<-as.data.frame(b_bin)
head(b_bin_df)
parcoord(b_bin_df)
b_bin_df_int<-lapply(b_bin_df,function(x) as.numeric(x))
b_bin_df_int<-as.data.frame(b_bin_df_int)
parcoord(b_bin_df_int)
summary(b_bin_df_int)
?lapply
#cut the features
get.parents.by.childname('Defaults_2')
for(i in get.parents.by.childname('Defaults_2')){
  parents <- get.parents.by.childname(i)
  if(length(parents)>0){#if there exist parents
    for(p in parents){
      if(!p %in% visit_list){
        unvisit_list[[length(unvisit_list)+1]]<-p
      }
    }
  }
}

target<-'Defaults_2'
visit_list()
minV<-10.5
maxV<-30.2
best_conf<-list()
allNames<-namel(colnames(ddd))
allNames[['Defaults_2']] <- NULL
visit_list[[length(visit_list)+1]] <- target
best_conf[[length(best_conf)+1]] <- paste0('(',10.5,',',30.2,']')
allButOne <- unlist(lapply(allNames,function(x) x[[1]]))
names(allButOne) <- NULL
allButOne

evi_string <- paste0('(',target,'>',minV,'&',target,'<',maxV,')')
querynodes <- paste(allButOne,collapse = '","')
eval_string <- paste0('cpdist(cgfit,c("',querynodes,'"),',evi_string,')')
result <- eval(parse(text=eval_string))
head(result)
result_bin<<-lapply(result,function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
result_bin_df <- as.data.frame(result_bin)
head(result_bin_df)
result_bin_df_part <- result_bin_df[,colnames(result_bin_df) %in% get.parents.by.childname(target)]
head(result_bin_df_part)
result_count_df <-as.data.frame(table(result_bin_df_part))
bestConfName<-c()
bestConfName<-c(bestConfName,colnames(result_bin_df_part))
bestConfName
bestSet<-c()
bestSet<-c(bestSet,result_count_df[which.max(result_count_df$Freq),])

head(lay11)
b_bin_df<-lay11
orders.plot<-data.frame()
for(i in 2:ncol(b_bin_df)){
  ord.cache<-b_bin_df %>%
    group_by(b_bin_df[,i-1],b_bin_df[,i]) %>%
    summarise(n=n())
  colnames(ord.cache)[1:2]<-c('from','to')
  
  ord.cache$from<-paste(ord.cache$from,'(',colnames(b_bin_df)[i-1],')',sep='')
  ord.cache$to<-paste(ord.cache$to,'(',colnames(b_bin_df)[i],')',sep='')
  orders.plot<-rbind(orders.plot,ord.cache)
}

plot(gvisSankey(orders.plot,from="from",to="to",weight='n',options=list(height=900,width=1800,sankey="{link:{color:{fill:'lightblue'}}}")))

for(i in unvisit_list){
  print(i)
}


for(i in 1:length(ids)){
  k <- ids[i]
  df.cache<-df %>% filter(clientId==k) %>% 
  mutate(n.ord=paste('ord',c(1:clients$n[clients$clientId==k]),sep=''))
  orders<-rbind(orders,df.cache)
}

orders.plot<-data.frame()
for(i in 2:ncol(orders)){
  ord.cache<-orders %>%
    group_by(orders[,i-1],orders[,i]) %>%
    summarise(n=n())
  colnames(ord.cache)[1:2]<-c('from','to')
  
  ord.cache$from<-paste(ord.cache$from,'(',i-1,')',sep='')
  ord.cache$to<-paste(ord.cache$to,'(',i,')',sep='')
  orders.plot<-rbind(orders.plot,ord.cache)
}

plot(gvisSankey(orders.plot,from="from",to="to",weight='n',options=list(height=900,width=1800,sankey="{link:{color:{fill:'lightblue'}}}")))
data<-read.csv(file="DTI_DTV.csv",header=TRUE)
str(data)
reg<-lm(Default~DTV+DTI,data=data)
reg
plot(reg)

setwd('C:\\R code\\rstudio')
library(tidyr)
library(dplyr)
messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50)
)

messy %>%
  gather(drug, heartrate, a:b)


set.seed(10)
messy <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)

messy

tidier <- messy %>%
  gather(key, time, -id, -trt)

tidier %>% head(8)

tidy <- tidier %>% separate(key, into = c("location","time"),sep="\\.")
tidy

install.packages('nycflights13')
library(nycflights13)
flights
flights[,1:16]
class(flights)
filter(flights,dest=='IAH')
select(flights,year:day,carrier,tailnum)
select(flights,-(year:day))
select(flights,starts_with("arr"))
arrange(flights,desc(arr_delay))
mutate(flights,speed=distance/air_time*60)

by_day <- group_by(flights,year,month,day)
by_day
summarise(by_day,delay=mean(dep_delay,na.rm=TRUE))

hourly_delay <- flights %>% filter(!is.na(dep_delay)) %>%
  group_by(day,hour) %>% summarise(delay=mean(dep_delay),n=n())%>% filter(n>10)
hourly_delay

translate_sql(month>1L,flights)
dc <- c("IAD","DCA")
translate_sql(dest %in% dc,flights)

browseVignettes(package="dplyr")

install.packages('ggvis')
library(ggvis)
daily <- flights %>%
  filter(origin=="EWR") %>%
  group_by(year,month,day) %>%
  summarise(delay=mean(dep_delay,na.rm=T),
            cancelled=mean(is.na(dep_delay)))

daily_weather <- weather %>%
  filter(origin=="EWR") %>%
  group_by(year,month,day) %>%
  summarise(temp = mean(temp,na.rm=TRUE),
            wind = mean(wind_speed,na.rm=TRUE),
            precip = sum(precip,na.rm=TRUE))

both <- daily %>%
  inner_join(daily_weather) %>%
  ungroup() %>%
  mutate(date=as.Date(ISOdate(year,month,day)))

both

both %>% ggvis(x=~temp,y=~delay) %>% layer_points() %>% layer_smooths()
both %>% ggvis(x=~temp,y=~delay,fill=~precip) %>% layer_points() %>% layer_smooths()
both %>% ggvis(~delay) %>% layer_histograms()

mtcars %>% ggvis(x=~wt,y=~mpg) %>% layer_points()
mtcars %>% ggvis(x=~wt,y=~mpg,fill:="red",stroke:="green") %>% layer_points()
mtcars %>% ggvis(~wt, ~mpg, size := 300, opacity := 0.4) %>% layer_points()
mtcars %>% ggvis(~wt, ~mpg, shape := "cross") %>% layer_points()

mtcars %>% 
  ggvis(~wt, ~mpg, 
        size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>% 
  layer_points()

mtcars %>% ggvis(~wt, ~mpg) %>% 
  layer_points() %>% 
  add_tooltip(function(df) df$wt)

df <- data.frame(x = 1:10, y = runif(10))
df %>% ggvis(~x, ~y) %>% layer_paths()

df <- data.frame(x = 3:1, y = c(1, 3, 2), label = c("a", "b", "c"))
df %>% ggvis(~x, ~y, text := ~label) %>% layer_text()
df %>% ggvis(~x, ~y, text := ~label) %>% layer_text(fontSize := 50)
df %>% ggvis(~x, ~y, text := ~label) %>% layer_text(angle := 45)

t <- seq(0, 2 * pi, length = 20)
df <- data.frame(x = sin(t), y = cos(t))
df %>% ggvis(~x, ~y) %>% layer_paths()
#df %>% ggvis(~x, ~y) %>% layer_lines()
#df %>% ggvis(~x, ~y) %>% arrange(x) %>% layer_paths()
#mtcars %>% ggvis(~mpg) %>% layer_histograms()
mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths()
mtcars %>% 
  ggvis(~wt, ~mpg) %>% 
  layer_smooths() %>% 
  layer_points()

mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_smooths(span = 1) %>%
  layer_smooths(span = 0.3, stroke := "red")

both %>% ggvis(~delay) %>% layer_histograms(binwidth=input_slider(1,10,value=5))
both %>% ggvis(~delay,~precip) %>% layer_points(opacity:=input_slider(0,1))

all_values<-function(x){
  if(is.null(x)) return(NULL)
  paste0(names(x),": ",format(x),collapse="<br />")
}

cocaine %>%  ggvis(x=~weight,y=~price) %>% layer_points(size=~potency,opacity:=0.2)%>%
  add_tooltip(all_values,"hover")

faithful %>%
  ggvis(x=~waiting) %>%
  layer_histograms(fill:="#999999",binwidth=input_slider(min=1,max=20,value=11))


####inference####
target<-'Defaults_2'
minValue<-10.3
maxValue<-40
evi_string <- paste0('(',target,'>',minValue,'&',target,'<',maxValue,')')
evi_string
allNames<-namel(colnames(ddd))
allNames
allNames[[target]]<-NULL
allNames
#names(allButOne)
allButOne <- unlist(lapply(allNames,function(x) x[[1]]))
names(allButOne) <- NULL
evi_string
querynodes <- paste(allButOne,collapse = '","')
querynodes <- get.parents.by.childname(target)
querynodes <- paste(querynodes,collapse = '","')
eval_string <- paste0('cpdist(cgfit,c("',querynodes,'"),',evi_string,')')
eval_string
result <- eval(parse(text=eval_string))
result
result_bin<-lapply(result,function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x))/2,0),5)))
result_bin <- as.data.frame(result_bin)
result_bin
result_count_df <- as.data.frame(table(result_bin))
aaa<-result_count_df[which.max(result_count_df$Freq),]
aaanames<-colnames(aaa)

aaalist<-list(names=aaanames,values=unlist(lapply(aaa,function(x) x[[1]])))


print(result_count_df[which.max(result_count_df$Freq),])

###reverse testing###

child_parents_list <- list()
for(i in allButOne){
  parents<-get.parents.by.childname(i)
  if(length(parents)>0){
    child_parents_list[[i]] <- parents
  }
}
visit_list<-list()
best_conf<-list()
inference_from_child <- list()
evi_string <- paste0('(',target,'>',minV,'&',target,'<',maxV,')')
querynodes <- paste(allButOne,collapse = '","')
#	querynodes <- get.parents.by.childname(input$TargetNode)
#	querynodes <- paste(querynodes,collapse = '","')
#	querynodes <- paste0('c("',querynodes,'")')
eval_string <- paste0('cpdist(cgfit,c("',querynodes,'"),',evi_string,')')
print('in best conf')
print(eval_string)
result <- eval(parse(text=eval_string))
print(head(result))
#   discretize the parents node for plotting purpose
result_bin<<-lapply(result[,colnames(result) %in% get.parents.by.childname(target)],function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
#   discretize the all nodes for test purpose
result_bin_full<-lapply(result,function(x) cut(x,ifelse((diff(range(x))/10)>1,round(diff(range(x)),0),10)))
result_bin_df_full <- as.data.frame(result_bin_full)
#   First get the parents node bin distribution
result_bin_df <- result_bin_df_full
result_bin_df <- result_bin_df[,colnames(result_bin_df) %in% get.parents.by.childname(target)]	
#	result_count_df <- as.data.frame(table(result_bin)) Do try to run the following code, machine will crashed!
result_count_df <- as.data.frame(table(result_bin_df))
print(head(result_count_df))
bestConfName <- colnames(result_bin_df)
bestSet <- result_count_df[which.max(result_count_df$Freq),]
#   store the visit_list and best configuration for the visit_list
for(i in bestConfName){
  if(i!='Freq'&&!(i %in% visit_list)){
    visit_list[[length(visit_list)+1]] <- i
    #			best_conf[[length(best_conf)+1]] <- as.character(bestSet[[i]])
    best_conf[[i]] <- as.character(bestSet[[i]])
    parents <- get.parents.by.childname(i)
    if(length(parents)>0){
      inference_from_child[[length(inference_from_child)+1]] <- i ##the next search start from i
    }
  }
}#
total <- 0
while(length(visit_list)<length(allButOne)&&total<10){#if we have not go through all nodes and we got some child nodes.
  if(length(inference_from_child)>0){
  inference_from_child_bak <- inference_from_child##copy by value, not reference???
  print(inference_from_child)
  print('*****************')
  for(ii in 1:length(inference_from_child_bak)){#get a candidate node
    seed_node <- inference_from_child_bak[[ii]]
    print(paste0('@@@ processing seed node :',seed_node))
    remove_index <- -1
    for(i in 1:length(inference_from_child)){#update the candidate list pool to remove the selected node.
      if(inference_from_child[[i]]==seed_node){
        remove_index <- i
      }
    }
    if(remove_index>0){
      inference_from_child[[remove_index]] <- NULL
    }
    
    parents <- child_parents_list[[seed_node]]
    interested_parents <- parents
    result_bin_df <- result_bin_df_full
    
    if(!all(parents %in% visit_list)){
      
    for(p in parents){
      if(p %in% visit_list){
        interested_parents <- interested_parents[!interested_parents %in% p]#if parent is in visited list, add it to constraints and remove from interested parents node
        print(p)
        result_bin_df <- subset(result_bin_df,result_bin_df[,p]==best_conf[[p]])
      }
    }
    
    result_bin_df <- subset(result_bin_df,result_bin_df[,seed_node]==best_conf[[seed_node]])##this can be problematic
    result_bin_df <- subset(result_bin_df,select = interested_parents)#result_bin_df[,interested_parents] it drop the data frame format
    print(paste0('$$$ inference parent nodes:',as.character(interested_parents)))
    result_count_df <- as.data.frame(table(result_bin_df))
    bestConfName <- colnames(result_bin_df)
    bestSet <- result_count_df[which.max(result_count_df$Freq),]
    index <- 1
    for(j in bestConfName){
      if(j!='Freq'&&!(j %in% visit_list)){
        visit_list[[length(visit_list)+1]] <- j
        #				best_conf[[length(best_conf)+1]] <- as.character(bestSet[[i]])
        if(!is.null(bestSet[[j]])){#2 or more ways table 
          best_conf[[j]] <- as.character(bestSet[[j]])
        }else{#1 way table
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
  total <- total + 1
  }else{#if all precede nodes are in visit list, we look to the child node 
    candidate_list<-list()
    #to extract the candidate list to inference
    for(vis_nodes in visit_list){
      children <- get.children.by.parentname(vis_nodes)
      if(!all(children %in% visit_list)){#if children not in the visit list
        interested_children <- children[!(children %in% visit_list)]
        for(ic in interested_children){
          candidate_list[[length(candidate_list)+1]] <- ic        
        }
      }
      parents_can <- get.parents.by.childname(vis_nodes)
      if(!all(parents_can %in% visit_list)){
        interested_parent <- parents_can[!(parents_can %in% visit_list)]
        for(ic in interested_parent){
          candidate_list[[length(candidate_list)+1]] <- ic
        }
      }
    }
    result_bin_df <- result_bin_df_full
    for(query_node in candidate_list){
      parents <- child_parent_list[[query_node]]
      observed_parents <- parents[(parents %in% visit_list)]
      for(op in observed_parents){
        result_bin_df <- subset(result_bin_df,result_bin_df[,op]==best_conf[[op]])
        
      }
      
    }
    
    
    
  }
}

