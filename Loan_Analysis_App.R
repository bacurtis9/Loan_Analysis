library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(scales)


# Helper Function to reorder columns to so the stacked bar and legend make sense
reorder_levels <- function(col){
  unique_values <- sort(c(unique(col)))
  
  if(sum(unique_values == "other") ){
  unique_values <- unique_values[unique_values != "other"] %>%
    append("other")
  }
  
  suppressWarnings({
    sorter = as.numeric(gsub(".*?([0-9]+).*", "\\1", 
                             gsub(",.*$", "", unique_values)))
    
    unique_values = unique_values[order(sorter)]
  })
  return(unique_values)
  
}


# Function to create selector input
selector <- function(inputId,label,col){
  
  unique_values = reorder_levels(col)
    
  
    pickerInput(
       inputId = inputId,
       label = label, 
       choices = unique_values,
       options = list(
         `actions-box` = TRUE), 
       selected = unique_values[1:length(unique_values)],
       multiple = TRUE
          
  )
}


#function to create Sideways Bar charts
create_sidways_bar <- function(data, title, xlabel, x, fill){
  
  data %>% 
    rename("fill" = fill) %>%
    group_by(loan_condition, fill)%>%
    summarise(combined_tot = sum(get(x)))%>%
    
    ggplot(aes(fill = factor(fill, levels = rev(reorder_levels(fill))),
               y = combined_tot,
               x = loan_condition
    )) + 
    geom_bar(position="fill", stat="identity") +
    coord_flip() + 
    guides(fill = guide_legend(title = title, reverse=TRUE, nrow = 1), color = "none") +
    theme_bw() +   
    theme(legend.position = "bottom", 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),         
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray"),
          axis.text = element_text(size = 10)) +
    labs(title = title, y = xlabel, x = "") +
    scale_y_continuous(expand = expansion(mult = c(0, .02)), labels = percent)
  
}


# Reding in Data
data <- read_csv("Data/loan_final.csv")


# Defining the header
header <- dashboardHeader(title = "Credit Risk Dashboard", titleWidth = "250"
                          )


# Defining the sidebar
sidebar <- dashboardSidebar(    
  width = "250",
  sidebarMenu(
    menuItem("Origination Dollars", icon = icon("chart-bar"), tabName = "loan_amount"),
    menuItem("Origination Units", icon = icon("chart-bar"), tabName = "units"),
    menuItem("Unpaid Principle Balance", icon = icon("chart-bar"), tabName = "upb")
  )
  
  
)


# Defining the body
body <- dashboardBody(
  tags$script(HTML("$('body').addClass('fixed');")),
  
  # creates 2 rows of selectors
  fluidRow(
    column(3, selector("cur_ost", "Currently Outstanding", data$cur_ost_ind)),
    column(3, selector("loan_con", "Loan Condition", data$loan_condition)),
    column(3, selector("dti", "Debt to Income Ratio", data$dti_group)),
    column(3, selector("income", "Income Group", data$income_group)),
    column(3, selector("home", "Home Ownership", data$home_ownership)),
    column(3, selector("purpose1", "Loan Purpose", data$purpose)),
    column(3, selector("term1", "Loan Term", data$term)),
    column(3, selector("interest", "Interest Payments", data$interest_payments))
  ),
  
  #creates 1 row of 3 bar vertical bar charts
  fluidRow(
    column(4, plotOutput("tot_loan_amount_bar", height = "300px")),
    column(4, plotOutput("num_acct_bar", height = "300px")),
    column(4, plotOutput("tot_upb_bar", height = "300px"))
    ),
  
  #defiines items that are unique for each menu
  tabItems(
    tabItem(tabName = "loan_amount",
            
            # creates 6 bar charts for first menu
            h2("Origination Dollars"),
            br(),
            plotOutput("dti_loan_amount_bar", height = "200px"),
            br(),
            plotOutput("income_loan_amount_bar", height = "200px"),
            br(),
            plotOutput("home_own_loan_amount_bar", height = "200px"),
            br(),
            plotOutput("purpose_loan_amount_bar", height = "200px"),
            br(),
            plotOutput("loan_term_loan_amount_bar", height = "200px"),
            br(),
            plotOutput("interest_loan_amount_bar", height = "200px")
            
            ),
    
    tabItem(tabName = "units",
            
            # creates 6 bar charts for second menu
            h2("Origination Units"),
            br(),
            plotOutput("dti_units_bar", height = "200px"),
            br(),
            plotOutput("income_units_bar", height = "200px"),
            br(),
            plotOutput("home_own_units_bar", height = "200px"),
            br(),
            plotOutput("purpose_units_bar", height = "200px"),
            br(),
            plotOutput("loan_term_units_bar", height = "200px"),
            br(),
            plotOutput("interest_units_bar", height = "200px")
            
    ),
  
    tabItem(tabName = "upb",
     
            # creates 6 bar charts for third menu     
            h2("Unpaid Principle Balance"),
            br(),
            plotOutput("dti_upb_bar", height = "200px"),
            br(),
            plotOutput("income_upb_bar", height = "200px"),
            br(),
            plotOutput("home_own_upb_bar", height = "200px"),
            br(),
            plotOutput("purpose_upb_bar", height = "200px"),
            br(),
            plotOutput("loan_term_upb_bar", height = "200px"),
            br(),
            plotOutput("interest_upb_bar", height = "200px")
  
    )
  )
)
# end of body


# Combines header sidebar and body into UI
ui <- dashboardPage(header, sidebar, body)
  

# creates Server
server <- function(input, output){

  
  #Creates Function that filters based on selectors
  agg_data_filt <- reactive({
  
   df <- data %>%
      filter(cur_ost_ind %in% input$cur_ost,
             loan_condition %in% input$loan_con,
             dti_group %in% input$dti,
             income_group %in% input$income,
             home_ownership %in% input$home,
             purpose %in% input$purpose1,
             term %in% input$term1,
             interest_payments %in% input$interest
      )
   return(df)
  
  })
  
    
  # create third vertical Bar chart
  output$tot_upb_bar <- renderPlot({
      
    #groups data
    agg_data_filt() %>%
      group_by(loan_condition) %>%
      summarise(tot_upb = sum(tot_upb)) %>%
      
      # Create chart
      ggplot(aes(y = tot_upb,x = loan_condition)) + 
      geom_bar(stat = "identity", color="#00BFC4", fill = "#00BFC4") +
      theme_bw()+   
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),         
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray"),
            axis.text = element_text(size = 10)) +
      labs(title = "Unpaid Principal Balance", y = "", x = "") +
      scale_y_continuous(expand = c(0,0), 
                         labels = dollar_format(scale = 1/1000000, suffix = ("M"), largest_with_cents = 1e+02)) +
      geom_text(aes(label = dollar(tot_upb, scale = 1/1000000, suffix = ("M"), largest_with_cents = 1e+02)), 
                vjust = 1.5, colour = "#5A5A5A", fontface = "bold")
  })
    
  # create second vertical Bar chart
  output$num_acct_bar <-renderPlot({
      
    # groups data
    agg_data_filt() %>%
      group_by(loan_condition) %>%
      summarise(tot_units = sum(tot_units)) %>%
    
    # create chart
      ggplot(aes(y = tot_units,x = loan_condition)) + 
      geom_bar(stat = "identity", color = "#00BFC4", fill = "#00BFC4") +
      theme_bw()+
      theme(legend.position = "bottom",
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray"),
            axis.text = element_text(size = 10)) +
      labs(title = "Origination Units", y = "", x = "") +
      scale_y_continuous(expand = c(0,0), 
                         labels = label_comma() ) +
      geom_text(aes(label = dollar(tot_units, prefix = "", largest_with_cents = 1e+00)), 
                vjust = 1.5, colour = "#5A5A5A", fontface = "bold")
    
  })
  
  
  # create first vertical bar chart 
  output$tot_loan_amount_bar <- renderPlot({
    
    # group data
    agg_data_filt() %>%
      group_by(loan_condition) %>%
      summarise(tot_loan_amount = sum(tot_loan_amount)) %>%
      
      #create chart
      ggplot(aes(y = tot_loan_amount,x = loan_condition)) + 
      geom_bar(stat = "identity", color="#00BFC4", fill = "#00BFC4") +
      theme_bw()+   
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),         
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray"),
            axis.text = element_text(size = 10)) +
      labs(title = "Origination Dollars", y = "", x = "") +
      scale_y_continuous(expand = c(0,0), 
                         labels = dollar_format(scale = 1/1000000, suffix = ("M"), largest_with_cents = 1e+02)) +
      geom_text(aes(label = dollar(tot_loan_amount, scale = 1/1000000, suffix = ("M"), largest_with_cents = 1e+02)), 
                vjust = 1.5, colour = "#5A5A5A", fontface = "bold")
  })
  
  
  #create 6 bar charts for first menu item
  
  output$dti_loan_amount_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Debt to Income Ratio", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "dti_group")
    
  })
  
  
  output$income_loan_amount_bar <-renderPlot({
    agg_data_filt() %>%
      create_sidways_bar(title = "Income", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "income_group")
    
  })
  
  
  output$home_own_loan_amount_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Home Ownership", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "home_ownership")
  })
  
  
  output$purpose_loan_amount_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Purpose", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "purpose")
    
    
  }) 
  
  
  output$loan_term_loan_amount_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Term", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "term")
    
    
  })
  
  
  output$interest_loan_amount_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Interest Payment Group", xlabel = "Origination Dollars",
                         x = "tot_loan_amount", fill = "interest_payments")
    
    
  }) 
  
  
  #create 6 bar charts for second menu item
  
  output$dti_units_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Debt to Income Ratio", xlabel = "Origination Units",
                         x = "tot_units", fill = "dti_group")
    
  })
  
  
  output$income_units_bar <-renderPlot({
    agg_data_filt() %>%
      create_sidways_bar(title = "Income", xlabel = "Origination Units",
                         x = "tot_units", fill = "income_group")
    
  })
  
  
  output$home_own_units_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Home Ownership", xlabel = "Origination Units",
                         x = "tot_units", fill = "home_ownership")
  })
  
  
  output$purpose_units_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Purpose", xlabel = "Origination Units",
                         x = "tot_units", fill = "purpose")
    
    
  }) 
  
  
  output$loan_term_units_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Term", xlabel = "Origination Units",
                         x = "tot_units", fill = "term")
    
    
  })
  
  
  output$interest_units_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Interest Payment Group", xlabel = "Origination Units",
                         x = "tot_units", fill = "interest_payments")
    
    
  }) 
  
  
  #create 6 bar charts for third menu item
  
  output$dti_upb_bar <-renderPlot({
  
    agg_data_filt() %>%
      create_sidways_bar(title = "Debt to Income Ratio", xlabel = "% of Unpaid Principal Balance",
                       x = "tot_upb", fill = "dti_group")
  
  })

  
  output$income_upb_bar <-renderPlot({
    agg_data_filt() %>%
      create_sidways_bar(title = "Income", xlabel = "% of Unpaid Principal Balance",
                       x = "tot_upb", fill = "income_group")

  })
  
  
  output$home_own_upb_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Home Ownership", xlabel = "% of Unpaid Principal Balance",
                         x = "tot_upb", fill = "home_ownership")
  })
  
   
  output$purpose_upb_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Purpose", xlabel = "% of Unpaid Principal Balance",
                         x = "tot_upb", fill = "purpose")
    
   
  }) 
  
  
  output$loan_term_upb_bar <-renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Loan Term", xlabel = "% of Unpaid Principal Balance",
                         x = "tot_upb", fill = "term")
    
    
  })
  
  
  output$interest_upb_bar <- renderPlot({
    
    agg_data_filt() %>%
      create_sidways_bar(title = "Interest Payment Group", xlabel = "% of Unpaid Principal Balance",
                         x = "tot_upb", fill = "interest_payments")
    
    
  }) 
    
}





shinyApp(ui, server)
