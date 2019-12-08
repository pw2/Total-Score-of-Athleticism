### TotalScore of Athleticism - Shiny App

# Load packages
library(tidyverse)
library(reshape)
library(stringr)
library(shiny)

# simulate data
set.seed(3344)
athlete <- as.factor(1:30)
cmj <- c(round(rnorm(n = 10, mean = 30, sd = 4), 1), round(rnorm(n = 10, mean = 24, sd = 4), 1), round(rnorm(n = 10, mean = 33, sd = 2), 1))
sprint_40 <- c(round(rnorm(n = 10, mean = 4.5, sd = .1), 2), round(rnorm(n = 10, mean = 4.9, sd = .2), 2), round(rnorm(n = 10, mean = 5, sd = .2), 2))
bench <- c(round(rnorm(n = 10, mean = 20, sd = 4), 1), round(rnorm(n = 10, mean = 12, sd = 4), 1), round(rnorm(n = 10, mean = 30, sd = 4), 1))
five_ten_five <- c(round(rnorm(n = 10, mean = 6.4, sd = .2), 2), round(rnorm(n = 10, mean = 6.7, sd = .2), 2), round(rnorm(n = 10, mean = 7.5, sd = .4), 2))
df <- data.frame(athlete, cmj, sprint_40, bench, five_ten_five)

# z-score function

z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

##### Data Pre-Processing #####
###############################

# calculate the z-score
df <- df %>%
  mutate_if(is.numeric, list(z = z_score))

df$sprint_40_z <- df$sprint_40_z * -1
df$five_ten_five_z <- df$five_ten_five_z * -1

# calculate TSA_z
df$TSA_z <- apply(df[, 6:9], MARGIN = 1, FUN = mean)

# Change data from a wide to long format
df_long <- df %>% 
  melt(., id = "athlete", measure.vars = c("cmj_z", "sprint_40_z", "bench_z", "five_ten_five_z", "TSA_z"))

# remove the _z
df_long$Test <- str_sub(df_long$variable, end = -3)

# Add indicator value
df_long <- df_long %>% mutate("indicator" = ifelse(value > 0, "above avg", "below avg"))

##### Shiny App #####
#####################

## User Interface

athlete <- as.vector(unique(df_long$athlete))

ui <- fluidPage(
  
  titlePanel("Performance Testing Results"),
  
  selectInput(
    input = "athlete",
    label = "athlete",
    choices = athlete,
    selected = "1"
  ),
  
  plotOutput(outputId = "tsa.plot",
             width = "60%")
)

## server

server <- function(input, output){
  
  dat <- reactive({
    dataset <- subset(df_long, athlete == input$athlete)
    dataset
  })
  
  output$tsa.plot <- renderPlot({
    d <- dat()
    
    athlete.plot <- ggplot(data = d, aes(x = Test, y = value)) +
      geom_rect(aes(ymin = -1, ymax = 1), xmin = 0, xmax = Inf, fill = "light grey", alpha = 0.3) +
      geom_col(aes(fill = indicator), alpha = 0.8) +
      scale_fill_manual(values = c("green", "red")) +
      theme_light() +
      theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, vjust = 1, hjust = 1), 
            axis.text.y = element_text(face = "bold", size = 12),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", face = "bold"),
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 15)) +
      labs(x = "", y = "z-score of performance") +
      ggtitle("Test Performance", subtitle = "Player Performance Standardized to the Team") +
      ylim(-3, 3)
    
    print(athlete.plot)
  })
}


## Run the app
shinyApp(ui = ui, server = server)

