library(readr)
library(tidyverse)

load_data <- function() {
  winequality_red <- read_delim("~/data-analysis2/winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
                      mutate(wine_colour = 'red')
  winequality_white <- read_delim("~/data-analysis2/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
                        mutate(wine_colour = 'white')
  
  return(rbind(winequality_red, winequality_white))
}

introduce_nas <- function(df, nas_percentage, column) {
  df[sample(1:nrow(df), floor((nas_percentage/100) * nrow(df))), column] <- NA
  return(df)
}

plot_histograms <- function(df) {
  # Histograms
  print(
    df %>%
      select(`fixed acidity`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `fixed acidity`)) +
      geom_histogram(binwidth=1, colour="black", fill="white") +
      xlim(0, 20) 
  )

  print(
    df %>%
      select(`volatile acidity`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `volatile acidity`)) +
      geom_histogram(binwidth=0.1, colour="black", fill="white") 
  )
  
  print(
    df %>%
      select(`citric acid`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `citric acid`)) +
      geom_histogram(binwidth=0.1, colour="black", fill="white") 
  )
  
  print(
    df %>%
      select(`residual sugar`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `residual sugar`)) +
      geom_histogram(binwidth=1, colour="black", fill="white") +
      xlim(0, 40) 
  )
  
  print(
    df %>%
      select(`chlorides`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `chlorides`)) +
      geom_histogram(binwidth=0.05, colour="black", fill="white")
  )
  
  print(
    df %>%
      select(`free sulfur dioxide`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `free sulfur dioxide`)) +
      geom_histogram(binwidth=10, colour="black", fill="white")
  )
  
  print(
    df %>%
      select(`total sulfur dioxide`) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = `total sulfur dioxide`)) +
      geom_histogram(binwidth=10, colour="black", fill="white")
  )
  
  print(
    df %>%
      select(density) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = density)) +
      geom_histogram(binwidth=0.001, colour="black", fill="white")
  )
  
  print(
    df %>%
      select(pH) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = pH)) +
      geom_histogram(binwidth=0.1, colour="black", fill="white")
  )
  
  print(
    df %>%
      select(sulphates) %>%
      mutate(count = n()) %>%  
      ggplot(aes(x = sulphates)) +
      geom_histogram(binwidth=0.1, colour="black", fill="white")
  )
}

plot_boxplots <- function(df) {
  print(
    df %>%
      select(wine_colour, `fixed acidity`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `fixed acidity`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `volatile acidity`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `volatile acidity`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `citric acid`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `citric acid`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `residual sugar`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `residual sugar`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, chlorides) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, chlorides)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `volatile acidity`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `volatile acidity`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `free sulfur dioxide`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `free sulfur dioxide`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, `total sulfur dioxide`) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, `total sulfur dioxide`)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, density) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, density)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, pH) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, pH)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, sulphates) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, sulphates)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, alcohol) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, alcohol)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
  
  print(
    df %>%
      select(wine_colour, quality) %>%
      group_by(wine_colour) %>%
      ggplot(aes(wine_colour, quality)) +
      geom_boxplot(aes(fill = wine_colour)) 
  )
}

plot_joyplots <- function(df) {
  print(ggplot(df, aes(x = `fixed acidity`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = `volatile acidity`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = `citric acid`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = `residual sugar`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = chlorides, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = `free sulfur dioxide`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = `total sulfur dioxide`, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = density, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = pH, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = sulphates, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = alcohol, y = wine_colour)) + geom_joy())
  print(ggplot(df, aes(x = quality, y = wine_colour)) + geom_joy())  
}

other_plots <- function(df) {
  print(
    pairs(~ quality + alcohol, data=df)
  )
  
  print(
    pairs(~ quality + sulphates, data=df)
  )
  
  print(
    pairs(~ quality + pH, data=df)
  )
  
  print(
    pairs(~ quality + density, data=df)
  )
  
  print(
    pairs(~ quality + `total sulfur dioxide`, data=df)
  )
  
  print(
    pairs(~ quality + `free sulfur dioxide`, data=df)
  )
  
  print(
    pairs(~ quality + chlorides, data=df)
  )
  
  print(
    pairs(~ quality + `residual sugar`, data=df)
  )
  
  print(
    pairs(~ quality + `citric acid`, data=df)
  )
  
  print(
    pairs(~ quality + `volatile acidity`, data=df)
  )
  
  print(
    pairs(~ quality + `fixed acidity`, data=df)
  )
  
  df$quality <- as.factor(df$quality)
  
  print(
    df %>%
      select(quality, `fixed acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `fixed acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `volatile acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `volatile acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `citric acid`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `citric acid`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `residual sugar`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `residual sugar`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, chlorides) %>%
      group_by(quality) %>%
      ggplot(aes(quality, chlorides)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `volatile acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `volatile acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `free sulfur dioxide`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `free sulfur dioxide`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `total sulfur dioxide`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `total sulfur dioxide`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, density) %>%
      group_by(quality) %>%
      ggplot(aes(quality, density)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, pH) %>%
      group_by(quality) %>%
      ggplot(aes(quality, pH)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, sulphates) %>%
      group_by(quality) %>%
      ggplot(aes(quality, sulphates)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, alcohol) %>%
      group_by(quality) %>%
      ggplot(aes(quality, alcohol)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
}

boxplots_quality_colour_wine <- function(df, colour) {
  df$quality <- as.factor(df$quality)
  
  df <- df %>% filter(wine_colour == colour)
  
  print(
    df %>%
      select(quality, `fixed acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `fixed acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `volatile acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `volatile acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `citric acid`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `citric acid`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `residual sugar`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `residual sugar`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, chlorides) %>%
      group_by(quality) %>%
      ggplot(aes(quality, chlorides)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `volatile acidity`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `volatile acidity`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `free sulfur dioxide`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `free sulfur dioxide`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, `total sulfur dioxide`) %>%
      group_by(quality) %>%
      ggplot(aes(quality, `total sulfur dioxide`)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, density) %>%
      group_by(quality) %>%
      ggplot(aes(quality, density)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, pH) %>%
      group_by(quality) %>%
      ggplot(aes(quality, pH)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, sulphates) %>%
      group_by(quality) %>%
      ggplot(aes(quality, sulphates)) +
      geom_boxplot(aes(fill = quality)) 
  )
  
  print(
    df %>%
      select(quality, alcohol) %>%
      group_by(quality) %>%
      ggplot(aes(quality, alcohol)) +
      geom_boxplot(aes(fill = quality)) 
  )  
}
