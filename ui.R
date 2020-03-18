library(shiny)
library(leaflet)
library(plotly)
library(tidyverse)
library(ggiraph)
library(formattable)
library(shinyWidgets)


source('appParts.R')


shinyUI(navbarPage(title = "hAPPiness",
                   fluid = TRUE,
                   inverse = TRUE,
                   position = "fixed-top",
                   header = tags$style("body {padding-top: 50px}"),
                   
                   tabPanel("Home", 
                            div(style = "margin-left:-15px;margin-right:-15px;margin-top:-15px;", img(src="header.jpg", width="100%")),
                            
                            #shinyjs::useShinyjs(),
                            
                            # Title
                            fluidRow(
                                column(12, align = "center",
                                       shiny::HTML("<br><h1>hAPPiness</h1>"),
                                       shiny::HTML("<h3>Making sense of happiness around the world</h3>")
                                      )
                                    ),
                            
                            # Break
                            fluidRow(style = "height:30px;"),
                            
                            # Why
                            fluidRow(
                                column(8, offset = 2, align = "justify",
                                       shiny::HTML("<center> <h2>Why care about happiness?</h2> </center>"),
                                       shiny::HTML("<h3>Sustainable happiness (defined as subjective well-being) leads 
                                                   to better physical and mental health, longevity, resilience, work 
                                                   performance, and pro-social behaviours. Individuals and governments 
                                                   alike therefore have a vested interest in increasing happiness. 
                                                   However, happiness research and surveys have revealed that the causes 
                                                   of happiness differ not only at the individual level, but across 
                                                   cultures and national boundaries. It follows that a one-size-fits-all 
                                                   approach to increasing happiness is not viable. Policymakers need to 
                                                   identify and address key contributors to happiness if they intend to 
                                                   increase happiness in target countries.</h3>")
                                      )
                                    ),
                            
                            # Break
                            fluidRow(style = "height:30px;"),
                            
                            # How
                            fluidRow(
                                column(8, offset = 2, align = "justify",
                                      shiny::HTML("<center> <h2>How can we measure happiness?</h2> </center>"),
                                      shiny::HTML("<h3>We can derive a happiness index using the Cantril Self-Anchoring 
                                                  Striving Scale (also known as the Cantril ladder), which is a validated 
                                                  well-being evaluation scale that asks respondents to rate their own current 
                                                  lives on a scale of 0 to 10 with 0 being the worst possible life they can 
                                                  imagine and 10 being the best. The Cantril ladder has been used widely by 
                                                  researchers and employed by Gallup for their research initiatives, including 
                                                  the Gallup World Poll.</h3>")
                                      )
                                    ),
                            
                            # Break
                            fluidRow(style = "height:30px;"),
                            
                            # What
                            fluidRow(
                                column(8, offset = 2, align = "justify",
                                      shiny::HTML("<center> <h2>What will I get here?</h2> </center>"),
                                      shiny::HTML("<h3>An interactive app for you to examine happiness trends both 
                                                  globally and in a country-specific fashion. Insights may be gleaned 
                                                  by exploring the relationships between potential contributing factors, 
                                                  such as development indicators, and happiness.</h3>")
                                      )
                                    ),
                            
                            # Break
                            fluidRow(style = "height:30px;"),
                            
                            # References
                            fluidRow(
                                column(8, offset = 2, align = "Left",
                                      shiny::HTML("<h3>References</h3>"),
                                      shiny::HTML("<h4>
                                                  Diener, E., & Chan, M. Y. (2011). Happy People Live Longer: Subjective Well-Being Contributes to Health and Longevity. Applied Psychology: Health and Well-Being, 3(1), 1-43. https://doi.org/10.1111/j.1758-0854.2010.01045.x</br>
                                                  <br>Oswald, A. J., Proto, E., & Sgroi, D. (2015). Happiness and Productivity. Journal of Labor Economics, 33(4), 789-822. https://doi.org/10.1086/681096</br>
                                                  <br>Cohn, M. A., Fredrickson, B. L., Brown, S. L., Mikels, J. A., & Conway, A. M. (2009). Happiness unpacked: Positive emotions increase life satisfaction by building resilience. Emotion, 9(3), 361-368. https://doi.org/10.1037/a0015952</br>
                                                  <br>Diener, E., Helliwell, J. F., & Kahneman, D. (2010). International differences in well-being. Oxford University Press.</br>
                                                  <br>Hadley Cantril. (1965). The pattern of human concerns. Rutgers University Press.</br>
                                                  <br>Gallup. (2019). Understanding How Gallup Uses the Cantril Scale. Retrieved from: https://news.gallup.com/poll/122453/understanding-gallup-uses-cantril-scale.aspx</br>
                                                  <br></br></h4>")
                                      )
                                    ),
                            ),
                   
                   navbarMenu('Global',
                              tabPanel('Worldview',
                                       world()),
                              tabPanel('Happiness Density',
                                       densit()),
                              tabPanel('Regions and Income Groups',
                                       wealth())
                            ),
                  
                   tabPanel('Country',
                            country()),
                   
                   tabPanel('Team',
                            
                            # Title
                            fluidRow(
                              column(12, align = "center",
                                     shiny::HTML("<h2>hAPPy team</h2>"),
                              )
                            ),

                            # Phing Chian
                            fluidRow(
                                column(6, align="right", style="padding:40px;",
                                       div(img(src="sad.png", width="100")),
                                       ),
                                column(6, align = "left", style="padding:50px;",
                                       shiny::HTML("<h3>Chai Phing Chian</h3>")
                                       )
                                    ),
                            
                            # Jinfu
                            fluidRow(
                                column(6, align="right", style="padding:40px;",
                                       div(img(src="sad.png", width="100")),
                                ),
                                column(6, align = "left", style="padding:50px;",
                                     shiny::HTML("<h3>Hu Jinfu</h3>")
                              )
                            ),
                            
                            # Robyn
                            fluidRow(
                                column(6, align="right", style="padding:40px;",
                                       div(img(src="sad.png", width="100")),
                                ),
                                column(6, align = "left", style="padding:50px;",
                                     shiny::HTML("<h3>Lim Su May, Robyn</h3>")
                              )
                            ),
                            
                            # Lester
                            fluidRow(
                                column(6, align="right", style="padding:40px;",
                                       div(img(src="sad.png", width="100")),
                                ),
                                column(6, align = "left", style="padding:50px;",
                                     shiny::HTML("<h3>Sun Yufei, Lester</h3>")
                              )
                            ),
                            
                            # Tang Ee Mei
                            fluidRow(
                                column(6, align="right", style="padding:40px;",
                                       div(img(src="sad.png", width="100")),
                                ),
                                column(6, align = "left", style="padding:50px;",
                                     shiny::HTML("<h3>Tang Ee Mei</h3>")
                              )
                            )
                            
                          )
                    )
        )