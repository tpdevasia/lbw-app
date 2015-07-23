library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("County Health Rankings"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      
      helpText( 
          p("This application shows the county percentile ranks for different modeling assumptions. Counties are ranked based on the estimated number of low birth weight births per 1000 births. Data was gathered from http://www.countyhealthrankings.org/rankings/data.")
      ),
      
      selectInput("region", 
                  label = "Choose a state",
                  choices = c("All states", "Illinois",
                              "Iowa", "Michigan", "Minnesota", "Wisconsin"),
                  selected = "All states"),

      selectInput("model", 
            label = "Choose a model",
            choices = c("fit1_overall: County, categorical (overall rank)", 
                        "fit2_overall: County, random effect (overall rank)",
                        "fit3_overall: County, State (overall rank)", 
                        "fit4_overall: County, Geographic (overall rank)", 
                        "fit5_overall: County, State, Geographic (overall rank)", 
                        "fit1_within: County, categorical (within-state rank)", 
                        "fit2_within: County, random effect (within-state rank)", 
                        "fit3_within: County, State (within-state rank)", 
                        "fit4_within: County, Geographic (within-state rank)", 
                        "fit5_within: County, State, Geographic (within-state rank)", 
                        "fit2_state: County, random effect (state specific)", 
                        "fit4_state: County, Geographic (state specific)"),
            selected = "fit1_overall: County, categorical (overall rank)"),
      
      sliderInput("slider", 
                  label = ("Enter number of counties to output into tables and plots"),
                  min = 1, 
                  max = 50, 
                  value = 10)
      ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Map", plotOutput("plot", width="100%")), 
                  tabPanel("Table, Top", tableOutput("table_top")),
                  tabPanel("Credible Interval Plot, Top", plotOutput("ci_top")),
                  tabPanel("Table, Bottom", tableOutput("table_bottom")),
                  tabPanel("Credible Interval Plot, Bottom", plotOutput("ci_bottom"))
      )
    )
  ))
)
