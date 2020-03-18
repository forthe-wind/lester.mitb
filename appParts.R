
world <- function() {
  tagList(
    div(class = "container",
        shiny::HTML("<center><h2>Worldview</h2></center>"),
        shiny::HTML("<center><h4>Happiness index choropleth map from 2011-2018</h4></center><br></br>")
    ),
    fluidRow(
      column(12, align = "center",
             shiny::HTML("<h4>Year</h4>")
      )
    ),  
    fluidRow(
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}", ".irs-grid-text { font-size: 15pt; }"),
      column(10, align="right",
          sliderInput(inputId='year', label=NULL, value=2018,  min=2011, max=2018, step=1,
                      sep="", width="75%", animate= animationOptions(loop=TRUE, playButton = icon('play'),
                                       pauseButton =  icon('pause')))
      ),
      column(2, align="left",
             shiny::HTML("<br></br><p>Hit the play button,</p>"),
             shiny::HTML("<p>grab a cuppa,</p>"),
             shiny::HTML("<p>and sit and watch passively.</p>")
      )
    ),
    fluidRow(plotlyOutput("map"))
  )
}


densit <- function() {
  tagList(
    div(class = "container",
        shiny::HTML("<center><h2>Happiness Density</h2></center>"),
        shiny::HTML("<center><h4>Happiness index and potential contributing factors</h4></center>"),
        shiny::HTML("<center><h4>Compare across all countries</h4></center><br></br>")
    ),
    fluidRow(
      column(6, align="center",
          selectInput("xvar", label="Potential contributing factors",
                       choices = c('Log GDP per capita' = 'Mean.GDP',
                                   'GINI coefficient' = 'GINI',
                                   'Arable land (% land area)' = 'Arable.land',
                                   'Industry (% GDP)' = 'Industry',
                                   'Internet penetrance (% of population)' = 'Internet',
                                   'Healthy life expectancy at birth' = 'Life.expectancy',
                                   'Birth rate (per 1,000 people)' = 'Birth.rate', 
                                   'Death rate (per 1,000 people)' = 'Death.rate', 
                                   'Infant mortality (per 1,000 live births)' = 'Mortality.infant',
                                   'Population density (per km)' = 'Pop.density',
                                   'Net migration' = 'Migration',
                                   'Positive affect' = 'Pos.affect',
                                   'Negative affect' = 'Neg.affect',
                                   'Social support' = 'Social.support',
                                   'Generosity' = 'Generosity',
                                   'Freedom to make life choices' = 'Freedom',
                                   'Perception of corruption' = 'Corruption',
                                   'Confidence in government' = 'Confidence.goverment',
                                   'Government\'s delivery' = 'Delivery',
                                   'Democracy' = 'Democratic')
          )
      ),
      column(6, align="center",
            radioButtons(inputId = 'radio', label='Colour coding', 
                         choices=list('World Bank income group' = 'WB.Group', 
                                      'Geographical region' = 'Region'),
                         selected='WB.Group')
      )
    ),
    fluidRow(girafeOutput("bubble"))
    
#    fluidRow(wellPanel(
#      plotOutput("bubble"))
#    )
  )
}



wealth <- function() {
  tagList(
    div(class = "container",
        shiny::HTML("<center><h2>Regions and Income Groups</h2></center>"),
        shiny::HTML("<center><h4>Happiness index and potential contributing factors</h4></center>"),
        shiny::HTML("<center><h4>Compare across World Bank income groups and geographical regions</h4></center><br></br>")
    ),
    fluidRow(
      column(8, align="center",
             wellPanel(plotOutput('scatter'))
      ),
      column(4, align="center",
             selectInput("xvar_sc", label="Potential contributing factors",
                         choices = c('Log GDP per capita' = 'Mean.GDP',
                                     'GINI coefficient' = 'GINI',
                                     'Arable land (% land area)' = 'Arable.land',
                                     'Industry (% GDP)' = 'Industry',
                                     'Internet penetrance (% of population)' = 'Internet',
                                     'Healthy life expectancy at birth' = 'Life.expectancy',
                                     'Birth rate (per 1,000 people)' = 'Birth.rate', 
                                     'Death rate (per 1,000 people)' = 'Death.rate', 
                                     'Infant mortality (per 1,000 live births)' = 'Mortality.infant',
                                     'Population density (per km?)' = 'Pop.density',
                                     'Net migration' = 'Migration',
                                     'Positive affect' = 'Pos.affect',
                                     'Negative affect' = 'Neg.affect',
                                     'Social support' = 'Social.support',
                                     'Generosity' = 'Generosity',
                                     'Freedom to make life choices' = 'Freedom',
                                     'Perception of corruption' = 'Corruption',
                                     'Confidence in government' = 'Confidence.goverment',
                                     'Government\'s delivery' = 'Delivery',
                                     'Democracy' = 'Democratic')
             ),
             radioButtons(inputId = 'radio_sc', 
                          label="Grouping by",
                          choices=list('World Bank income group' = 'WB.Group', 
                                       'Geographical region' = 'Region'),
                          selected='WB.Group'),
             div(style = "text-align: center; font-weight:bold; background-color:lightblue",
                 'R-square'),
             tableOutput('table_1'),
             div(style = "text-align: center; font-weight:bold; background-color:lightblue",
                 'Significance tests'),
             formattableOutput('table_2')
      )
    ),
    fluidRow(
      column(8, align='center', offset=2,
             div(style = "text-align: center; font-weight:bold; background-color:lightgreen",
                 'Covariate trend (slope) analysis'),
             formattableOutput('table_3')),

    )
  )
}

country <- function(){

  tagList(
    div(class = "container",
        shiny::HTML("<center><h2>Predicting Happiness</h2></center><br></br>"),
    ),
    
    fluidRow(
      column(12, align = "left",
             shiny::HTML("<h4>Tweak the levels of contributing factors for a predicted happiness index</h4>")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput("income"),
        uiOutput("life"),
        uiOutput("birth"),
        uiOutput("infant"),
        uiOutput("gdp"),
        uiOutput("delivery"),
        uiOutput("freedom"),
        uiOutput("generosity"),
        uiOutput("social")
      ),
      mainPanel(
        fluidRow(        
          column(9, align='center', wellPanel(
            uiOutput("list_country"),
            plotlyOutput("gauge", height="200px"))
            ),
          column(3, align = "left",
                 shiny::HTML("<h5><b>Legend</b></h5>"),
                 shiny::HTML("<h5>White bar: 2018 happiness index</h5>"),
                 shiny::HTML("<h5>Cantril ladder areas:</h5>"),
                 shiny::HTML("<h5>Suffering in red</h5>"),
                 shiny::HTML("<h5>Struggling in orange</h5>"),
                 shiny::HTML("<h5>Thriving in green</h5>")
            ),
        plotOutput("predict")
      )
    )
  )
  )
}
