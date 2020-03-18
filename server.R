library(shiny)
library(rstatix)
library(maps)
library(dplyr)
library(plotly)
library(viridis)
library(ggiraph)
library(plyr)
library(ggpubr)
library(emmeans)
library(formattable)
library(shinyWidgets)


dt <- read.csv("merged_dataset_grouped.csv")
dt <- dt[dt$Year >= 2011,]
dt <- dt[!duplicated(dt[,c('Country', 'Year')]),]

region_long = list(c("ASIA (EX. NEAR EAST)", "NEAR EAST", "C.W. OF IND. STATES", "OCEANIA"),
               c("BALTICS", "EASTERN EUROPE", "WESTERN EUROPE", "NORTHERN AMERICA"),
               c("NORTHERN AFRICA", "SUB-SAHARAN AFRICA"),
               c("LATIN AMER. & CARIB"))
region_short = c("Asia Pacific", "Europe & North America", "Africa", "South America")
for(i in 1:4){
    levels(dt$Region)[levels(dt$Region) %in% region_long[[i]]] = region_short[i]
}
dt$Region[is.na(dt$Region)] <- "Europe"
dt$WB.Group <- factor(dt$WB.Group, levels = c("High", "Upper-middle", "Lower-middle", "Low"))

dt1 = reshape(dt, idvar="Country", timevar="Year", v.names="Life.Ladder", direction="wide")
dt1[, 33:40] = (is.na(dt1[,33:40]))*rowMeans(dt1[,33:40], na.rm=TRUE)[row(dt1[,33:40])] + 
    replace(dt1[,33:40], is.na(dt1[,33:40]), 0)
colnames(dt1)[33:40] = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) 
dt1 = dt1 %>% gather(Year, Life.Ladder, 33:40)


dt2 <- ddply(dt, ~Country + Region + Mean.GDP + WB.Group + GINI + Arable.land + Birth.rate +
                Death.rate + Internet + Industry + Mortality.infant + Migration + Pop.density, 
             Population=mean(Population, na.rm=TRUE), 
             Life.Ladder=mean(Life.Ladder, na.rm=TRUE), 
             Social.support=mean(Social.support, na.rm=TRUE), 
             Life.expectancy=mean(Life.expectancy, na.rm=TRUE), 
             Freedom=mean(Freedom, na.rm=TRUE), 
             Generosity=mean(Generosity, na.rm=TRUE), 
             Corruption=mean(Corruption, na.rm=TRUE), 
             Pos.affect=mean(Pos.affect, na.rm=TRUE), 
             Neg.affect=mean(Neg.affect, na.rm=TRUE), 
             Confidence.goverment=mean(Confidence.goverment, na.rm=TRUE), 
             Democratic=mean(Democratic, na.rm=TRUE), 
             Delivery=mean(Delivery, na.rm=TRUE), 
             summarise)
dt2 <- dt2[!is.na(dt2$Region),]
dt2 <- dt2[!duplicated(dt2$Country),]
country_list = unique(dt2$Country)


longname <- list('Life.Ladder' = 'Happiness index',
          'Mean.GDP' = 'Log GDP per capita',
          'GINI' =  'GINI coefficient',
          'Arable.land' = 'Arable land',
          'Birth.rate' = 'Birth rate', 
          'Death.rate' = 'Death rate', 
          'Internet' = 'Internet penetrance',
          'Industry' = 'Industry',
          'Mortality.infant' = 'Infant mortality',
          'Pop.density' = 'Population density',
          'Migration' = 'Net migration',
          'Social.support' = 'Social support',
          'Life.expectancy' = 'Healthy life expectancy at birth',
          'Freedom' = 'Freedom to make life choices',
          'Generosity' = 'Generosity',
          'Corruption' = 'Perception of corruption',
          'Pos.affect' = 'Positive affect',
          'Neg.affect' = 'Negative affect',
          'Confidence.goverment' = 'Confidence in government',
          'Delivery' = 'Government\'s delivery',
          'Democratic' = 'Democracy',
          'WB.Group' = "World Bank Income Group\n",
          'Region' = 'Geographical Region\n'
          )


shinyServer(function(input, output) {

    output$list_country <- renderUI({
        selectInput("country", "Choose a country:", 
                    choice = country_list,
                    selected = "Singapore")
    })
       
    output$income <- renderUI({
        sliderTextInput('income', "Income group:",
                        choice = c("Low", "Lower-middle", "Upper-middle", "High"),
                        selected = dt2[dt2$Country == input$country, "WB.Group"])
    }) 
    
    output$life <- renderUI({
        sliderInput('life', "Healthy life expectancy at birth:",
                    min=0, max=120, round=TRUE, ticks=FALSE,
                    value=dt2[dt2$Country == input$country, "Life.expectancy"])
    }) 
    
    output$birth <- renderUI({
        sliderInput('birth', "Birth rate (per 1,000 people):",
                    min=0, max=50, round=FALSE, ticks=FALSE,
                    value=dt2[dt2$Country == input$country, "Birth.rate"])
    }) 
    
    output$infant <- renderUI({
        sliderInput('infant', "Infant mortality (per 1,000 live births):",
                    min=0, max=100, round=FALSE, ticks=FALSE,
                    value=dt2[dt2$Country == input$country, "Mortality.infant"])
    }) 
    
    output$gdp <- renderUI({
        sliderInput('gdp', "Log GDP per capita:",
                    min=0, max=15, round=FALSE, ticks=FALSE, step=0.1,
                    value=dt2[dt2$Country == input$country, "Mean.GDP"])
    }) 
    
    output$delivery <- renderUI({
        sliderInput('delivery', "Government's delivery:",
                    min=-3.0, max=3.0, round=FALSE, ticks=FALSE, step=0.01,
                    value=dt2[dt2$Country == input$country, "Delivery"])
    }) 
    
    output$freedom <- renderUI({
        sliderInput('freedom', "Freedom to make life choices:",
                    min=0, max=1.0, round=FALSE, ticks=FALSE,
                    value=dt2[dt2$Country == input$country, "Freedom"])
    }) 
    
    output$generosity <- renderUI({
        sliderInput('generosity', "Generosity:",
                    min=-1.0, max=1.0, round=FALSE, ticks=FALSE, step=0.01,
                    value=dt2[dt2$Country == input$country, "Generosity"])
    }) 
    
    output$social <- renderUI({
        sliderInput('social', "Social support:",
                    min=0, max=1.0, round=FALSE, ticks=FALSE, step=0.01,
                    value=dt2[dt2$Country == input$country, "Social.support"])
    }) 
    
       
    output$map <- renderPlotly({
        dt1 = dt1[dt1$Year == input$year, ]
        HIndex <- dt1$Life.Ladder
        countries <- dt1$Country
        Pop <- dt1$Population/10e6
        
        # specify map projection/options
        g <- list(
            showframe = TRUE,
            showcoastlines = FALSE,
            projection = list(type = 'robinson')
        )
        
        pal <- c("red3", "darkorange1", "green")
        plot_geo(dt1) %>%
            add_trace(z = ~HIndex, locations = ~Code, showscale = TRUE, 
                      color = ~HIndex, colors = pal, hoverinfo = 'text',
                      text = paste(countries, 
                                    "\nHappiness Index: ", round(HIndex, 2), 
                                    "\nPopulation (M): ", round(Pop,2))) %>%
            colorbar(title = 'Index',
                     limits = c(1.95,8.05),
                     x=0.95,
                     len=1) %>%
            layout(geo = g, 
                   autosize = TRUE, height="700")
    })
    
    
    output$bubble <- renderGirafe({
        xvar = input$xvar
        gg_scatter <- ggplot(dt2, aes_string(x = xvar, y = "Life.Ladder")) + 
            geom_point_interactive(aes_string(colour = input$radio, 
                                              size = "Population", 
                                              tooltip="Country"), 
                                   alpha = .5) +
            scale_size_continuous(name = "Population Size\n", 
                                  breaks = c(10**6,10**7,10**8,10**9), 
                                  labels = c("1 million","10 million","100 million","1 billion"), 
                                  range = c(3, 40))  + 
            labs(colour=longname[input$radio]) +
            guides(colour = guide_legend(override.aes = list(size=10))) +
            ylab("Hppiness Index") +
            xlab(longname[xvar]) +
            ggtitle(paste0('Happiness Index by ', str_to_title(longname[xvar]), ' for All Countries')) +
            theme_classic(base_size=20)
        girafe(ggobj = gg_scatter,
               width_svg = 20,
               height_svg = 10,
               options = list(opts_selection(type = "single", only_shiny = TRUE)) )
    })
    
    output$scatter <- renderPlot({
        xvar = input$xvar_sc
        radio_sc = input$radio_sc
        ggplot(dt2, aes_string(x=xvar, y="Life.Ladder")) +
            geom_point() +
            geom_smooth(method=lm , color="navy", fill="lightskyblue2", se=TRUE, level=.95) +
            theme_classic(base_size=20) +
            facet_wrap(as.formula(paste("~",radio_sc)), nrow=1) +
            labs(y="Happiness Index", 
                 x=longname[xvar], 
                 title = paste0("Happiness Index by ", str_to_title(longname[xvar]), 
                                "\nand grouped by ", longname[radio_sc])) +
            theme(legend.position='none', 
                  plot.title = element_text(hjust=0.5, size = 20, face = "bold"))            
    })
    
    output$table_1 <- renderTable({
        xvar = input$xvar_sc
        radio_sc = input$radio_sc
        dt3 <- dt %>% rstatix::select(radio_sc, "Life.Ladder", xvar)
        dt3 <- dt3[complete.cases(dt3), ]
        formula_1 <- as.formula(paste("Life.Ladder",
                                      xvar,
                                      sep = "~"))
        
        r_square <- list()
        for (i in levels(dt3[,radio_sc])) {
            dt_temp <- dt3[dt3[radio_sc] == i,]
            m <- lm(formula_1, data=dt_temp)
            r_square[[i]] = summary(m)$r.square
        }
        r_square_df = data.frame(r_square)
        names(r_square_df) <- levels(dt3[,radio_sc])
        r_square_df
    }, 
    digits=4, spacing='xs', align='c')

    
    output$table_2 <- renderFormattable({
        xvar = input$xvar_sc
        radio_sc = input$radio_sc
        dt3 <- dt %>% rstatix::select(radio_sc, Life.Ladder, xvar)
        dt3 <- dt3[complete.cases(dt3), ]
        
        formula_2 <- as.formula(paste("Life.Ladder",
                                      paste(c(xvar, radio_sc), collapse="*"),
                                      sep = "~"))
        formula_3 <- as.formula(paste("Life.Ladder",
                                      paste(c(xvar, radio_sc), collapse="+"),
                                      sep = "~"))
        formula_4 <- as.formula(paste(".resid", radio_sc, sep = "~"))
        
        res <- dt3 %>% anova_test(formula_2)
        homogeneity <- res$p[3]
        model <- lm(formula_3, data=dt3)
        model.metrics <- augment(model) %>%
            rstatix::select(-.hat, -.sigma, -.fitted, -.se.fit)
        shapiro = shapiro_test(model.metrics$.resid)
        normality = shapiro$p.value
        variance = (model.metrics %>% levene_test(formula_4))$p
        outlier = nrow(model.metrics %>% filter(abs(.std.resid)>3) %>% as.data.frame())
        
        assumptions = data.frame('Assumption' = c('Homogeneity',
                                                  'Normality',
                                                  'Variance',
                                                  'Outliers'),
                                 'p-value' = c(homogeneity,
                                               normality,
                                               variance,
                                               outlier))
        colnames(assumptions) <- c('Assumption', 'p-value')
        formattable(assumptions, align = 'c', list(
            area(row=1:3, col=2) ~ formatter("span", 
                                style = x ~ ifelse(x <= 0.05, 
                                                 style(color='red', font.weight='bold'), 
                                                 style(color='green')),
                                x ~ sprintf("%.3f", x)),
            area(row=4, col=2) ~ formatter("span", 
                                style = x ~ ifelse(x > 0.05, 
                                                  style(color='red', font.weight='bold'),
                                                  style(color='green')),
                                x ~ sprintf("%.0f", x))
        ))
    })
    
    output$table_3 <- renderFormattable({
        xvar = input$xvar_sc
        radio_sc = input$radio_sc
        dt3 <- dt %>% rstatix::select(radio_sc, Life.Ladder, xvar)
        dt3 <- dt3[complete.cases(dt3), ]
        formula_5 <- as.formula(paste("Life.Ladder",
                                      paste(c(xvar, radio_sc), collapse="*"),
                                      sep = "~"))
        formula_6 <- as.formula(paste("pairwise", radio_sc, sep = "~"))
        model <- lm(formula_5, data = dt3)
        trend_table = emtrends(model, formula_6, var = xvar)
        contrast = data.frame(trend_table$contrasts)
        contrast$df <- NULL
        contrast$estimate = digits(contrast$estimate, digits=3)
        contrast$p.value = digits(contrast$p.value, digits=3)
        colnames(contrast) <- c('Group difference', 'Estimate', 'Standard error', 't-ratio', 'p-value')
        formattable(contrast, format='f', digits=2, align = 'l', list(
            Estimate = color_tile("white", "orange"),
            "p-value" = formatter('span', style = x ~ ifelse(x <=0.05, 
                                                           style(color='red', font.weight='bold'), 
                                                           style(color='green')))
        ))
    })
    
    output$gauge <- renderPlotly({
        input_data <- data.frame("Social.support" = input$social,
                                 "WB.Group" = input$income,
                                 "Birth.rate" = input$birth,
                                 "Mortality.infant" = input$infant,
                                 "GDP" = input$gdp,
                                 "Delivery" = input$delivery,
                                 "Freedom" = input$freedom,
                                 "Generosity" = input$generosity,
                                 "Life.expectancy" = input$life)
        
        model <- lm(Life.Ladder ~ Social.support
                    + WB.Group
                    + Life.expectancy * WB.Group
                    + Birth.rate * WB.Group
                    + Birth.rate
                    + Mortality.infant * WB.Group
                    + Mortality.infant
                    + GDP * WB.Group
                    + Delivery * WB.Group
                    + Freedom
                    + Generosity,
                    data = dt)
        
        prediction <- predict(model, input_data)
        current_happiness <- dt[dt$Year==2018 & dt$Country==input$country, 
                                "Life.Ladder"]
        #current_happiness <- dt2[dt2$Country==input$country, "Life.Ladder"]
        gauge <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = prediction,
#            title = "Happiness Index",
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = current_happiness),
            gauge = list(
                axis =list(range = list(NULL, 10)),
                bar = list(color = "black"),
                steps = list(
                    list(range = c(0, 4), color = "red"),
                    list(range = c(4, 7), color = "orange"),
                    list(range = c(7, 10), color = "green")),
                threshold = list(
                    line = list(color = "white", width = 5),
                    thickness = 0.75,
                    value = current_happiness))) %>%
            layout(margin = list(l=20,r=30))
        
        gauge
        
    })
    
    
    
    output$predict <- renderPlot({
        input_data <- data.frame("Social.support" = input$social,
                                 "WB.Group" = input$income,
                                 "Birth.rate" = input$birth,
                                 "Mortality.infant" = input$infant,
                                 "GDP" = input$gdp,
                                 "Delivery" = input$delivery,
                                 "Freedom" = input$freedom,
                                 "Generosity" = input$generosity,
                                 "Life.expectancy" = input$life)
        
        model <- lm(Life.Ladder ~ Social.support
                    + WB.Group
                    + Life.expectancy * WB.Group
                    + Birth.rate * WB.Group
                    + Birth.rate
                    + Mortality.infant * WB.Group
                    + Mortality.infant
                    + GDP * WB.Group
                    + Delivery * WB.Group
                    + Freedom
                    + Generosity,
                    data = dt)
        
        prediction <- predict(model, input_data, interval="prediction")
        
        happiness = dt[dt$Country==input$country, c('Year', 'Life.Ladder')]
        happiness$ymin = happiness$Life.Ladder
        happiness$ymax = happiness$Life.Ladder
        happiness[nrow(happiness) + 1,] = c(2020, prediction[1:3])
        ggplot(happiness, aes(x=Year, y=Life.Ladder)) + 
            geom_ribbon(aes(x=Year, ymin=ymin, ymax=ymax,
                            fill="95% prediction interval"), alpha=0.4) +
            geom_point(aes(colour=input$country), size=3) +
            geom_line(aes(colour=input$country), size=1) +
            theme_linedraw(base_size=20) +
            labs(title=paste0("Historical and Predicted Happiness Index for ", 
                              input$country)) +
            scale_fill_manual(name="", values=c('aquamarine2')) +
            scale_color_manual(name="", values=c('aquamarine4')) +
            ylab("Happiness Index") +
            theme(legend.title = element_blank(), legend.position = 'bottom') +
            scale_x_continuous(breaks=c(2011:2020),
                             labels=c("2011","2012","2013","2014", 
                                     "2015","2016","2017","2018",
                                      "", "Predicted"))
    })
    
        
})

