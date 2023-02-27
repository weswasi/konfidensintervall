library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- tagList(
  includeCSS(path = "www/css/styles.css"), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Konfidensintervall - Kriminologiska Institutionen", 
        windowTitle = "Konfidensintervall"
      ),
    ),
    br(),
    fluidPage(
      
      # Application title
      sidebarLayout(
        sidebarPanel(
          sliderInput("n_experiments",
                      "Antal stickprov:",
                      min = 1,
                      max = 100,
                      step = 1,
                      value = 1),
          sliderInput("n_samples",
                      "Stickprovsstorlek:",
                      min = 20,
                      max = 1000,
                      step = 20,
                      value = 100),
          sliderInput("alpha",
                      "Konfidensintervall (%):",
                      min = 5,
                      max = 100,
                      step = 5,
                      value = 95),
          br(),
          tags$a(
            href="https://github.com/weswasi/konfidensintervall/", 
            tags$img(src="https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                     width="40",
                     height="35"))
        ),
        mainPanel(
          plotOutput('IntervalPlot', height = '700px')
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$IntervalPlot <- renderPlot({
    
    n_experiments = input$n_experiments
    n_samples = input$n_samples
    alpha = 1 - (input$alpha / 100)
    
    # create a synthetic population
    n_population <- 100000
    population <- rbinom(n_population, 1, 0.5)
    true_proportion <- mean(population)
    
    
    sample_props <- vector('numeric', n_experiments)
    upper_cis <- vector('numeric', n_experiments)
    lower_cis <- vector('numeric', n_experiments)
    contains_true_prop <- 0
    for (i in 1:n_experiments) {
      # sample from the population with replacement.
      sample <- sample(population, n_samples, replace = FALSE)
      
      # sample proportion
      sample_prop <- mean(sample)
      
      # margin of error
      sample_std <- (sqrt(sample_prop * (1 - sample_prop)) / sqrt(n_samples))
      z_value <- qnorm(alpha/2, lower.tail = FALSE)
      margin_of_error <- z_value * sample_std
      
      # store values
      sample_props[i] <- sample_prop
      upper_cis[i] <- sample_prop + margin_of_error
      lower_cis[i] <- sample_prop - margin_of_error
      if (true_proportion >= lower_cis[i] && true_proportion <= upper_cis[i]) {
        contains_true_prop <- contains_true_prop + (1/n_experiments)
      }
    }
    
    confidence_text <- paste0(input$alpha, "%-igt")
    title_text <- paste(confidence_text, "konfidensintervall", sep = " ")
    sample_text <- paste0("Stickprovsstorlek per stickprov: ", input$n_samples)
    subtitle_text <- sample_text
    subtitle_text <- paste0(sample_text, '\n', 'Hur många procent av stickproven som innehåller det sanna populationsvärdet: ',
                            round(contains_true_prop, 2) * 100, '%')
    
    p <- tibble(
      sample_prop = sample_props,
      upper_ci = upper_cis,
      lower_ci = lower_cis) %>%
      mutate(bound = ifelse(lower_cis < 0.5 & upper_cis > 0.5, 0, 1)) %>%
      mutate(experiment_number = as.factor(row_number())) %>%
      ggplot(aes(x = experiment_number, y = sample_prop)) +
      geom_point(size = 3, color = 'steelblue') +
      geom_linerange(aes(ymin = lower_ci, ymax = upper_ci, color = bound)) +
      geom_hline(aes(yintercept = true_proportion),
                 linetype = 'dashed', linewidth = 1) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_colour_gradient2(mid = "black" , high = "red") +
      coord_flip() +
      ggtitle(title_text, subtitle = subtitle_text) +
      ylab('Populationsvärdet') +
      xlab('Stickprov') +
      theme(plot.title = element_text(face = 'bold', size = 15),
            plot.subtitle = element_text( size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line.y = element_blank(),
            legend.position = "none")
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)