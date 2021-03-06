library(shiny)
library(shinydashboard)
library(data.table)
library(adagio)
library(ggplot2)
#Get the connected list
setwd("/home/manish/Shiny Tutorial/")
#Add elements to your app as arguments to fluidPage()
syndicatedt<-fread("samplefile.txt")

ui <- dashboardPage(
  skin="red",
  
  dashboardHeader(title = "Loan Syndication"),
  
  dashboardSidebar(sidebarMenu(
    
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Data Setup",tabName = "datasetup", icon = icon("database"))
    ,menuItem("Generate Syndicate",tabName = "generatesyndicate", icon = icon("money"))
    ,menuItem("Lenders Summary",tabName = "lendsumm", icon = icon("bank"))
    ,menuItem("Deal Summary",tabName = "dealsumm", icon = icon("bar-chart-o")))),
  
  dashboardBody(    
    tabItems(tabItem(tabName = "datasetup",fluidPage())
             
             ,tabItem(tabName = "generatesyndicate"
                      ,fluidPage(wellPanel(style = "background-color: #ffe6ff;"
                                           ,fluidRow(column(6,selectInput('issuer', 'Select the Issuer', sort(unique(syndicatedt$Issuer))
                                                       , multiple=FALSE))
                                                     ,column(3,uiOutput("LaunchDate"))
                                                     ,column(3,uiOutput("DealAmount"))))
                                 
                                 ,wellPanel(style = "background-color: #e6ffff;"
                                            ,fluidRow(column(6,sliderInput(inputId = "contminmax"
                                                       ,label = "Limit max and min contribution"
                                                       ,value = c(0.03,0.3), min = 0, max = 1))
                                                      ,column(6,sliderInput(inputId = "deccap"
                                                        ,label = "Maximum decline probability allowed"
                                                        ,value = 0.8   ,min = 0 ,max = 1))))
                                 ,wellPanel(style = "background-color: #ffffcc;"
                                            ,fluidRow(column(6,uiOutput("include1"))
                                                     ,column(6,uiOutput("exclude1"))
                                            ,fluidRow(column(6,uiOutput("printmsg"))
                                                     ,column(6,uiOutput("knapres1")))))
                                 ,mainPanel(column(6,plotOutput("plot1"))
                                           ,column(6,plotOutput("plot2")),width = 12)))
             
             ,tabItem(tabName = "lendsumm",fluidPage())
             
             ,tabItem(tabName = "dealsumm"  ,fluidPage())
             
             )
    )
  )

server <- function(input, output) {
  
  lendlist<-reactive(syndicatedt[Issuer==input$issuer 
                        & Launch_Date==input$LaunchDt 
                        & Deal_Amount==as.numeric(input$DealAmt),Lender,])
  
  output$include1<-renderUI({

            selectInput('include2', 'Fix Lenders', lendlist(), multiple=TRUE)
  })
  
  output$exclude1 <- renderUI({
    
  cols2 <- setdiff(lendlist(),input$include2)
    
  selectInput("exclude2", "Lender Exclusion",  choices = cols2,multiple = TRUE )
  })
  
  output$LaunchDate<-renderUI({
    
  selectInput("LaunchDt", "Launch Date",  choices =sort(unique(syndicatedt[Issuer==input$issuer,Launch_Date,])),multiple = FALSE)
    
  })
  
  output$DealAmount<-renderUI({
    selectInput("DealAmt", "Deal Amount"
                ,choices =sort(unique(syndicatedt[Issuer == input$issuer  & Launch_Date == input$LaunchDt ,Deal_Amount,]))
                ,multiple = FALSE)
     })


  
  #Once the deal is selected run the knapsack based on the data
 
  #Lets apply the filter on the min and max proportions
  #Subset the data
  
  subst<-reactive({syndicatedt[,':='(cont_prop_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                      ,as.numeric(input$contminmax[2])
                                                      ,cont_prop)
                                     ,DollarAmt_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                      ,round(as.numeric(input$contminmax[2])*Deal_Amount,0)
                                                      ,DollarAmt)
                                     ),][Issuer==input$issuer 
                                         & Launch_Date==input$LaunchDt 
                                         & Deal_Amount==as.numeric(input$DealAmt)
                                         & cont_prop>=input$contminmax[1]
                                         & dec_prob<=input$deccap
                                         & !(Lender %in% c(input$exclude2))]
    })
  
  #Now further subset based on the included lenders
  subst2<-reactive({syndicatedt[,':='(cont_prop_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                         ,as.numeric(input$contminmax[2])
                                                         ,cont_prop)
                                      ,DollarAmt_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                          ,round(as.numeric(input$contminmax[2])*Deal_Amount,0)
                                                          ,DollarAmt)
                                        ),][Lender %in% input$include2
                                            & Issuer==input$issuer 
                                            & Launch_Date==input$LaunchDt 
                                            & Deal_Amount==as.numeric(input$DealAmt)]})

  cond1<-reactive({(as.numeric(input$DealAmt)-sum(subst2()$DollarAmt_1))<=0})
  cond2<-reactive({(as.numeric(input$DealAmt)-sum(subst2()$DollarAmt_1))})
  
  #If any fiex lender is selected
  
  subst1<-reactive({if(length(input$include2)>0){syndicatedt[,':='(cont_prop_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                        ,as.numeric(input$contminmax[2])
                                                        ,cont_prop)
                                     ,DollarAmt_1=ifelse(cont_prop>=as.numeric(input$contminmax[2])
                                                         ,round(as.numeric(input$contminmax[2])*Deal_Amount,0)
                                                         ,DollarAmt) )
                                ,][Issuer==input$issuer 
                                    & Launch_Date==input$LaunchDt 
                                    & Deal_Amount==as.numeric(input$DealAmt)
                                    & cont_prop>=input$contminmax[1]
                                    & dec_prob<=input$deccap
                                    & DollarAmt_1<=cond2()
                                    & !(Lender %in% c(input$exclude2,input$include2))]
                                      }})
  
  knapres<-reactive({
    if(cond1() | length(input$include2)==0){

   knapsack(w =subst()$DollarAmt_1
                                ,p =subst()$dec_prob*subst()$cont_prop_1/(1-subst()$cont_prop_1)
                                ,cap =as.numeric(input$DealAmt))
  }else{

    knapsack(w =subst1()$DollarAmt_1
                                ,p =subst1()$dec_prob*subst1()$cont_prop_1/(1-subst1()$cont_prop_1)
                                ,cap =cond2())
  }
  })
  
  #output$knapres1<-renderPrint(paste0(knapres()))
  output$knapres1<-renderPrint(paste0(knapres()))
  
  #Generate the final result set
  
  finalres<-reactive({
    if(cond1() | length(input$include2)==0){
      
       setorder(subst()[knapres()$indices,c('Lender','dec_prob','DollarAmt','cont_prop','cont_prop_1','DollarAmt_1'),with=FALSE]
                ,DollarAmt_1)
      
    }else{
      
      setorder(rbind(subst2()[,c('Lender','dec_prob','DollarAmt','cont_prop','cont_prop_1','DollarAmt_1'),with=FALSE]
            ,subst1()[knapres()$indices,c('Lender','dec_prob','DollarAmt','cont_prop','cont_prop_1','DollarAmt_1'),with=FALSE])
            ,DollarAmt_1)
      
    }
  })
  
  
  output$printmsg<-renderPrint(paste0(sum(finalres()$DollarAmt_1),'---',sum(subst2()$DollarAmt_1),'----'
                                      ,knapres()$capacity))
  
  #Now append this data if incase there are fixed lenders
  
  d1<-reactive({ggplot(finalres(), aes(x=factor(Lender,levels = Lender), y=DollarAmt_1)) +
      geom_bar(stat="sum",aes(fill=Lender),show.legend = FALSE) +
      geom_text(aes(label=paste0(finalres()$DollarAmt_1,',',paste0(round(finalres()$cont_prop_1*100,1),'%')))
                , position=position_stack(vjust=0.5))+
      coord_flip()+scale_y_continuous(limits=c(0,max(finalres()$DollarAmt_1)+10))+
      theme_light()+ylab("Contribution Allocated")+xlab("Lender")
      })

  d2<-reactive({ggplot(finalres(), aes(x=factor(Lender,levels = Lender), y=dec_prob)) +
      geom_bar(stat="sum",aes(fill=Lender),show.legend = FALSE) +
      geom_text(aes(label=paste0(round(finalres()$dec_prob*100,1),'%'))
                , position=position_stack(vjust=0.5))+
      coord_flip()+scale_y_continuous(limits=c(0,1))+
      theme_light()+ylab("Decline Probability")+xlab("Lender")
  })
  
  output$plot1<-renderPlot({d1()})
  output$plot2<-renderPlot({d2()})
  
  
}
    
shinyApp(ui = ui, server = server)    
