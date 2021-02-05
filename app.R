library(shiny)
library(tidyverse)
library(dplyr)
library(zoo)
library(forecast)
library(lubridate)
library(shinyWidgets)
library(gridExtra)
case_data = rio::import("https://hub.mph.in.gov/dataset/6b57a4f2-b754-4f79-a46b-cff93e37d851/resource/46b310b9-2f29-4a51-90dc-3886d9cf4ac1/download/covid_report.xlsx")
death_data = rio::import("https://hub.mph.in.gov/dataset/6bcfb11c-6b9e-44b2-be7f-a2910d28949a/resource/7661f008-81b5-4ff2-8e46-f59ad5aad456/download/covid_report_death_date_agegrp.xlsx")
bed_data = rio::import("https://hub.mph.in.gov/dataset/4d31808a-85da-4a48-9a76-a273e0beadb3/resource/0c00f7b6-05b0-4ebe-8722-ccf33e1a314f/download/covid_report_bedvent_date.xlsx")


#groups data by date, shortens values we are using
case_count = case_data %>%
  group_by(DATE) %>%
  summarize(cases = sum(COVID_COUNT))

death_count = death_data %>%
  group_by(date) %>%
  summarize(deaths = sum(covid_deaths))

bed_count = bed_data %>%
  group_by(DATE) %>%
  summarize(beds = sum(BEDS_ICU_OCCUPIED_COVID_19))

vent_count = bed_data %>%
  group_by(DATE) %>%
  summarize(vents = sum(VENTS_ALL_USE_COVID_19))

# length of cases for keeping track of new data
case_length <- length(case_count$cases) + 1
death_length <- length(death_count$deaths) + 1
bed_length <- length(bed_count$beds) + 1
vent_length <- length(vent_count$vents) + 1


# change date to year-month-day, cuts off 00:00:00 and changes ch to date
case_count$DATE = as.Date(ymd(case_count$DATE))
death_count$date = as.Date(ymd(death_count$date))
bed_count$DATE = as.Date(ymd(bed_count$DATE))
vent_count$DATE = as.Date(ymd(vent_count$DATE))


# use zoo data structure for time series
case_ts=zoo(case_count$cases)
death_ts=zoo(death_count$deaths)
bed_ts=zoo(bed_count$beds)
vent_ts=zoo(vent_count$vents)


# automatic estimation of arima
fit_cases <- auto.arima(case_ts)
fit_deaths <- auto.arima(death_ts)
fit_beds <- auto.arima(bed_ts)
fit_vents <- auto.arima(vent_ts)

fore_cases <- forecast(fit_cases, h = 7, level =.99)
fore_deaths <- forecast(fit_deaths, h = 7, level =.99)
fore_beds <- forecast(fit_beds, h = 7, level =.99)
fore_vents <- forecast(fit_vents, h =7, level =.99)

#data frame for various forecast data
case_low <- floor(sum(fore_cases$lower))
case_high <- floor(sum(fore_cases$upper))
deaths_low <- floor(sum(fore_deaths$lower))
deaths_high <- floor(sum(fore_deaths$upper))
beds_low <- floor(min(fore_beds$lower))
beds_high <- floor(max(fore_beds$upper))
vents_low <- floor(min(fore_vents$lower))
vents_high <- floor(max(fore_vents$upper))

covid_frame <- data.frame(case_low, case_high, deaths_low, deaths_high,
                          beds_low, beds_high, vents_low, vents_high)

# 95% CI for 7 days
fore_cases2 <- forecast(fit_cases, h = 7, level =.95)
fore_deaths2 <- forecast(fit_deaths, h = 7, level=.95)
fore_beds2 <- forecast(fit_beds, h = 7, level = .95)
fore_vents2 <- forecast(fit_vents, h = 7, level = .95)


#adding dates for forecast, don't know a better way to do this bleh
#just change the first date to the next date after the data is pulled
#this data is old so it will need to be changed, the second date is +7 days
first_date = Sys.Date()
second_date = first_date + 6
date_cases = as.Date(first_date):as.Date(second_date)

#this part doesn't change at all, its some weird hard code from stack overflow
new_case_dates = data.frame(DATE = as.Date(date_cases, origin = "1970-01-01"),
                            cases = NA)
new_death_dates = data.frame(date = as.Date(date_cases, origin = "1970-01-01"),
                             deaths = NA)
new_bed_dates = data.frame(DATE = as.Date(date_cases, origin = "1970-01-01"),
                           beds = NA)
new_vent_dates = data.frame(DATE = as.Date(date_cases, origin = "1970-01-01"),
                            vents = NA)
case_count <- rbind(case_count, new_case_dates)
death_count <- rbind(death_count, new_death_dates)
bed_count <- rbind(bed_count, new_bed_dates)
vent_count <- rbind(vent_count, new_vent_dates)

# takes new length for date vectors
case_length2 <- length(case_count$cases)
death_length2 <- length(death_count$deaths)
bed_length2 <- length(bed_count$beds)
vent_length2 <- length(vent_count$vents)


# extract mean and CI for the forecasts
#be sure to use new dates for the date vector at the end
#233:239 is oct 26-nov 1
fore_case_data=data.frame(mean=as.numeric(fore_cases$mean),
                          up95=as.numeric(fore_cases$upper[,1]),
                          low95=as.numeric(fore_cases$lower[,1]),
                          date=(case_count$DATE[case_length:case_length2]))

fore_death_data=data.frame(mean=as.numeric(fore_deaths$mean),
                           up95=as.numeric(fore_deaths$upper[,1]),
                           low95=as.numeric(fore_deaths$lower[,1]),
                           date=(death_count$date[death_length:death_length2]))

fore_bed_data=data.frame(mean=as.numeric(fore_beds$mean),
                         up95=as.numeric(fore_beds$upper[,1]),
                         low95=as.numeric(fore_beds$lower[,1]),
                         date=(bed_count$DATE[bed_length:bed_length2]))

fore_vent_data=data.frame(mean=as.numeric(fore_vents$mean),
                          up95=as.numeric(fore_vents$upper[,1]),
                          low95=as.numeric(fore_vents$lower[,1]),
                          date=(vent_count$DATE[vent_length:vent_length2]))


#95% confidence levels
#again change the values, may make this more streamlined later
fore_case_data2=data.frame(mean=as.numeric(fore_cases2$mean),
                           up95=as.numeric(fore_cases2$upper[,1]),
                           low95=as.numeric(fore_cases2$lower[,1]),
                           date=(case_count$DATE[case_length:case_length2]))

fore_death_data2=data.frame(mean=as.numeric(fore_deaths2$mean),
                            up95=as.numeric(fore_deaths2$upper[,1]),
                            low95=as.numeric(fore_deaths2$lower[,1]),
                            date=(death_count$date[death_length:death_length2]))

fore_bed_data2=data.frame(mean=as.numeric(fore_beds2$mean),
                          up95=as.numeric(fore_beds2$upper[,1]),
                          low95=as.numeric(fore_beds2$lower[,1]),
                          date=(bed_count$DATE[bed_length:bed_length2]))

fore_vent_data2=data.frame(mean=as.numeric(fore_vents2$mean),
                           up95=as.numeric(fore_vents2$upper[,1]),
                           low95=as.numeric(fore_vents2$lower[,1]),
                           date=(vent_count$DATE[vent_length:vent_length2]))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Indiana Covid Dashboard"),
    
    # Main panel for displaying outputs ----
  mainPanel(
      tabsetPanel(
        tabPanel("Cases",
                 fluidRow(
                   plotOutput("distPlot1"),
                   verbatimTextOutput("summary")
                 )),
        tabPanel("Deaths",
                 fluidRow(
                   plotOutput("distPlot2")
                 )),
        tabPanel("Beds",
                 fluidRow(
                   plotOutput("distPlot3")
                 )),
        tabPanel("Vents",
                 fluidRow(
                   plotOutput("distPlot4")
                 ))
      )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot1 <- renderPlot({
    ggplot(case_count[2:case_length2,])+
      xlab("")+ylab("cases")+theme_bw()+
      geom_ribbon(data=fore_case_data,aes(x=date,ymin=low95,ymax=up95),alpha=.3,fill="red")+
      geom_ribbon(data=fore_case_data2,aes(x=date,ymin=low95,ymax=up95),alpha=.5,fill="red")+
      geom_line(data=case_count[2:case_length2,], aes(x=DATE, y=cases),alpha=.7,size=1)+
      #geom_line(data=fore_case_data,aes(x=date,y=mean),color="red",size=.5,alpha=1)+
      theme(axis.text.x=element_text(angle=60, hjust=1),text = element_text(size=20))
  })
  output$distPlot2 <- renderPlot({
    ggplot(death_count[2:death_length2,])+
      xlab("")+ylab("deaths")+theme_bw()+
      geom_ribbon(data=fore_death_data,aes(x=date,ymin=low95,ymax=up95),alpha=.3,fill="red")+
      geom_ribbon(data=fore_death_data2,aes(x=date,ymin=low95,ymax=up95),alpha=.5,fill="red")+
      geom_line(data=death_count[2:death_length2,], aes(x=date, y=deaths),alpha=.7,size=1)+
      geom_line(data=fore_death_data,aes(x=date,y=mean),color="red",size=.5,alpha=1)+
      theme(axis.text.x=element_text(angle=60, hjust=1),text = element_text(size=20))
  })
  output$distPlot3 <- renderPlot({
    ggplot(bed_count[2:bed_length2,])+
      xlab("")+ylab("beds")+theme_bw()+
      geom_ribbon(data=fore_bed_data,aes(x=date,ymin=low95,ymax=up95),alpha=.3,fill="red")+
      geom_ribbon(data=fore_bed_data2,aes(x=date,ymin=low95,ymax=up95),alpha=.5,fill="red")+
      geom_line(data=bed_count[2:bed_length2,], aes(x=DATE, y=beds),alpha=.7,size=1)+
      geom_line(data=fore_bed_data,aes(x=date,y=mean),color="red",size=.5,alpha=1)+
      theme(axis.text.x=element_text(angle=60, hjust=1),text = element_text(size=20))
  })
  output$distPlot4 <- renderPlot({
    ggplot(vent_count[2:vent_length2,])+
      xlab("")+ylab("vents")+theme_bw()+
      geom_ribbon(data=fore_vent_data,aes(x=date,ymin=low95,ymax=up95),alpha=.3,fill="red")+
      geom_ribbon(data=fore_vent_data2,aes(x=date,ymin=low95,ymax=up95),alpha=.5,fill="red")+
      geom_line(data=vent_count[2:bed_length2,], aes(x=DATE, y=vents),alpha=.7,size=1)+
      geom_line(data=fore_vent_data,aes(x=date,y=mean),color="red",size=.5,alpha=1)+
      theme(axis.text.x=element_text(angle=60, hjust=1),text = element_text(size=20))
  })
  output$summary <- renderPrint({
    covid_frame
  })
  
}
#grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, widths = c(3,3))
# Create Shiny app ----
shinyApp(ui = ui, server = server)