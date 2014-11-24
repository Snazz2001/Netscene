#require("shiny")
#setwd("C:\\Users\\4most\\Dropbox\\Mark Somers\\Denev")
#runApp("Netscene")

source("C:\\Projects\\BayesianNetwork\\Netscene1110\\Netscene\\server.R")
require(googleVis)
require(Rgraphviz)
require(graph)
require(grid)
require(ggplot2)

#priorities
#Saving and loading
#Numbering and categorising (tags?)
## User controls, Institution controls

#options(shiny.reactlog=TRUE)



shinyUI(
navbarPage("GraphRisk",id=TRUE,
    
#  tabPanel(value=1,"Save/Load Network",
#sidebarLayout(
# 	sidebarPanel(
#		img(src ="4most_logo.gif", height = 52, width = 52, align = "left"),
#		br(),
#		br(),
#		h3("Load Network"),
#		br(),
#		br(),
#	  #    fileInput('file1', 'Choose GraphRisk Repository',
#      #          accept=c('.Rdata')),
#		br(),
#		br(),
#		h3("Save Network"),
#		br(),
#		br(),
#	#	downloadButton('downloadData', 'Download'),
#		br(),
#		br()
#),
#    mainPanel( plotOutput("contents")
#)
#))
#,
  tabPanel(value=2,"Build Network",
	sidebarLayout(
 		sidebarPanel(
			img(src ="4most_logo.gif", height = 52, width = 52, align = "left"),
			h3("GraphRisk Build"),
#			br(),br(),
			h4("Create Node"),
#			br(),
            textInput("NewNodeName", label="Name:", value = ""),
			h5("Parent Node"),
#			br(),
			uiOutput("ParentsUI"),

			checkboxGroupInput("ParentNodeList","Parents",choices = node_names,inline = TRUE),###use get.name method to get the names for every nodes in the network,need double check it.
			radioButtons("ContinuousOrDiscrete","Type",c("Continuous"="c","Binary"="d")),#
#	#		submitButton("CreateNode"),br()
			actionButton("CreateNode","Create Node"),
			uiOutput("EnterParam"),
			actionButton("UpdateNetWork","Update Network")
	#	textOutput("txt")
		),
    # Show a plot of the generated distribution
    	mainPanel(
			plotOutput("netPlot2")
    	)
    )
),#

tabPanel(value=3,"Distributions",
      sidebarLayout(
 		sidebarPanel(
		img(src ="4most_logo.gif", height = 52, width = 52, align = "left"),
		h3("GraphRisk Distribution"),
		br(),br(),
		h4("Select Plot"),
		selectInput("ChartChoice",label="Chart Type", choices = c("Scatter Plot","Heat Map","Histogram"), selected="Scatter Plot"),
		h4("Choose Node x-axis"),
		#uiOutput("ExamineNodeX"),
		uiOutput("selectUI1"),
#   		selectInput("ExamineNodeX",label="Examine",choices = node_names, selected="IntGearing"),
		h4("Choose Node y-axis"),
		uiOutput("selectUI2"),		
#   		selectInput("ExamineNodeY",label="Examine",choices = node_names, selected="vintage"),
		br()#,
		#actionButton("Draw","let's go")
	#	submitButton("Draw")#
		),
    mainPanel(
	plotOutput("distPlot")
    )
)
)
,  

  tabPanel(value=4,"Evidence",#
    sidebarLayout(
 	sidebarPanel(
 		tags$form(
		img(src ="4most_logo.gif", height = 52, width = 52, align = "left"),
		h3("GraphRisk Evidence"),
#		br(),br(),
		h4("Enter Evidence"),
		htmlOutput("DescriptionButton"),
#		br(),
  	#	selectInput("EvidenceNode",label="Select Node",choices = get.name(nnodes), selected="hpi"),
		uiOutput("selectUI3"),
		uiOutput("ChooseState"),
		radioButtons("EnterOrRetract","Action",c("Enter","Retract")),
		actionButton("Update","Update Evidence")

	#	,br(),
		),
		tags$form(
		h4("Please input the node of interests:"),
	#	radioButtons("EnterOrRetract","Action",c("Enter","Retract"))
	#	,
		uiOutput("selectUI4"),
		actionButton("Inference","Do inference")
		#submitButton("Submit")#,br(),
		#textOutput("txt")
			)
		),
    # Show a plot of the generated distribution
    mainPanel(
	plotOutput("netPlot1"),
	textOutput("evidences"),
	textOutput("inference")
	#plotOutput("inference")
    )
    )
)
  ,
###del from here###
  
  tabPanel(value=5,"Component 3")

))


