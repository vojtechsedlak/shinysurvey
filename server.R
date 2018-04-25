library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggthemes)
library(scales)

source("functions.R")
survey <- getSurvey()


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  questions <- unique(survey$Question) 
  countries <- survey %>%
    count(Country) %>%
    filter(n>1000) %>%
    select(Country)
  
  # populate dropdown of questions
  output$questionDropdown <- renderUI({
    selectInput("question",label=NULL,questions,width="100%")
  })
  
  # populate filter dropdown
  output$selectorDropdown <- renderUI({
    options <- c("None","Age","Tech Knowledge","Facebook Users and Non-users","Country")
    selectInput("filterSelector", label=NULL,
                options)
  })
  
  # populate individual filter dropdown
  output$inSelectorDropdown <- renderUI({
    if (is.null(input$filterSelector)) {
      return()
    } else if (input$filterSelector == "None") {
      return()  
    } else {
      options <- survey %>%
        mutate(Vars=get(input$filterSelector)) %>%
        count(Vars) %>%
        filter(n>10000)
      options_list <- unique(options$Vars)
      selectInput("inFilterSelector", label="Select a filter:",
                  sort(options_list)) 
    }
  })
  
  output$message <- renderUI({
    if (is.null(input$question)) {
      return(NULL)
    } else if (input$filterSelector != "None" & is.null(input$inFilterSelector)) {
      return(NULL)
    } else {
      if(checkValidity()) {
        return(NULL)
      } else {
        div(class="box box-solid box-warning",style="padding:20px;",
          p("There isn't enough data for the filters you selected. Try changing the filtering criteria.")
        )
      } 
    }
  })
  
  # populate compare dropdown
  output$compareDropdown <- renderUI({
    selectInput("compareSelector",label=NULL,c("None","Age","Tech Knowledge","Facebook Users and Non-users","Frequency of Use"),selected="None")
  })
  
  valid <- reactive(
    checkValidity()
  )
  
  # find what kind of chart we need
  chartType <- reactive(
    survey %>%
      filter(Question==input$question) %>%
      select(Chart) %>%
      head(1)
  )

  # check if enough responses are gathered
  checkValidity <- function() {
    data <- raw() 
    if (input$compareSelector != "None") {
      totals <- data %>%
        group_by(Answer,Vars) %>%  
        summarize(count=sum(n)) %>%
        mutate(valid=ifelse(count>10,TRUE,FALSE)) 
    } else {
      totals <- data %>%
        group_by(Answer) %>%  
        summarize(count=sum(n)) %>%
        mutate(valid=ifelse(count>10,TRUE,FALSE))
    }
    valid <<- all(totals$valid)
    return(all(totals$valid))
  }
  
  # return only if enough responses exist  
  data <- reactive(
    if (is.null(input$question)) {
      return(NULL)
    } else if (input$filterSelector != "None" & is.null(input$inFilterSelector)) {
      return(NULL)
    } else if (checkValidity()) {
      raw()
    } else {
      return(NULL)
    }
  )
  
  # applyg filtering logic, if needed
  raw <- reactive(
    if (is.null(input$question)) {
      return(NULL)
    } else if (input$filterSelector != "None" & is.null(input$inFilterSelector)) {
      return(NULL)
    } else {
      
      #initial filtering
      if (input$filterSelector != "None") {
        filtered <- survey %>%
          filter(Answer != "Unknown") %>%
          filter(Question==input$question,get(input$filterSelector)==input$inFilterSelector)
      } else {
        filtered <- survey %>%
          filter(Answer != "Unknown") %>%
          filter(Question==input$question)
      }
      
      # check if results are to be compared
      if (input$compareSelector != "None") {
       data <- filtered %>%
          count(Answer,Vars=get(input$compareSelector))
        
        totals <- data %>%
          group_by(Vars) %>%
          summarize(total=sum(n))
        
        data <- left_join(data,totals) %>%
          filter(total > 20) %>%
          mutate(share=round((n/total)*100,digits=2)) 
      } else {
        data <- filtered %>%
          count(Answer) %>%
          mutate(share=n/sum(n)*100)
      }
    }
  )
  
  output$Question <- renderText({
    if (is.null(input$question)) {
      return()
    } else {
      input$question
    }
  })
  
  # render plot output
  output$answerPlot <- renderPlot({
    if (is.null(data())) {
      return()
    } else {
      
      data <- data()
      
      chart <- ggplot(data)
      if (chartType() == "Histogram") {
        if (input$compareSelector != "None" && input$compareSelector != "Country") {
          chart <- chart +
            geom_bar(aes(x=as.integer(Answer),y=share,fill=Vars),position="dodge",stat="identity") +
            scale_x_continuous()
        } else {
          chart <- chart +
            geom_bar(aes(x=as.integer(Answer),y=share),fill="#1FBEC3",position="dodge",stat="identity") +
            scale_x_continuous()
        }
      } else {
        if (input$compareSelector != "None" && input$compareSelector != "Country") {
          chart <- chart +
            geom_bar(aes(x=Answer,y=share,fill=Vars),position="dodge",stat="identity") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
        } else {
          chart <- chart +
            geom_bar(aes(x=Answer,y=share),fill="#1FBEC3",position="dodge",stat="identity") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
        }
      }     
      
      # common chart elements
      chart <- chart + 
        labs(y="Share (in %)", x="Answer") +
        theme(legend.background=element_rect(color="#357CA5",size=0.5,linetype = "solid")) +
        theme(legend.position="top") +
        theme_hc(base_size = 18,base_family="Helvetica") +
        guides(fill=guide_legend(nrow=4,byrow=TRUE,title="Categories",title.position = "top"))
      chart
    }
  })
  
  # render sample sizes table
  output$totalSamples <- renderTable({
    if (is.null(data())) {
      return()
    } else {
      if (input$compareSelector != "None" && input$compareSelector != "Country") {
        data <- data()
        data %>%
          group_by(Categories = Vars) %>%
          summarize(Sample=sum(n),Share=str_c(round(Sample/sum(data$n)*100,digits=2),"%"))
      } else {
        data() %>%
          summarize(Sample=sum(n))
      } 
    }
  })
  
  # render table with answers
  output$answerTable <- renderTable({
    if (is.null(data())) {
      return()
    } else {
      if (input$compareSelector != "None" && input$compareSelector != "Country") {
        data() %>%
          select(-n,-total) %>%
          mutate(share=percent(share/100)) %>%
          spread(Vars,share)
      } else {
        data() %>%
          mutate(n=format(n,format="d",big.mark = ","),share=percent(share/100))
      }
    }
  })
  
  # render table with answers
  output$simpleTable <- renderTable({
    if (is.null(data())) {
      return()
    } 
    data() %>%
      mutate(n=format(n,format="d", big.mark=","),share=percent(share/100))
  })
  
  # render table with answers
  output$valueTable <- renderTable({
    if (is.null(data())) {
      return()
    } else {
      if (input$compareSelector != "None" && input$compareSelector != "Country") {
        data() %>%
          select(-share,-total) %>%
          mutate(n=format(n,format="d",big.mark=",")) %>%
          spread(Vars,n)
      } else {
        return()
      }
    }
  })
  
  # handle download
  output$download <- downloadHandler(
    filename = function() {
      "facebook_survey.csv"
    },
    content = function(con) {
      data <- getDownloadableSurvey()
      write.csv(data, con)
    }
  )
  
}