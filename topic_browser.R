
library(pacman)

pacman::p_load(shiny,
               ggplot2,
               shinythemes,
               shinyWidgets,
               shinydashboard,
               quanteda,
               seededlda,
               dplyr,
               tidyr
)

load(file = "outputs.RData")

slda1_topics <- slda1_topics <- terms(slda1, 50) %>% as.data.frame()

topic_names <- colnames(slda1_topics) %>% sort()

terms_lists <- slda1_topics %>%
  mutate(Rank = row_number()) %>% pivot_longer(col=-Rank, names_to = "Topic", 
                                               values_to = "Terms")

topic_proportions_long <- topic_proportions %>% 
  mutate(ProjectReference = row.names(.)) %>% pivot_longer(col=-ProjectReference,
                                                           names_to = "Topic",
                                                           values_to = "Proportion")


# UI

header <-  dashboardHeader(title = "AHRC Grants Topic Browser",
                           titleWidth = "30%",
                           tags$li(class = "dropdown", 
                                   actionButton("about_btn", "About",
                                                class = "btn btn-secondary",
                                                style = "margin-top: 5px; margin-right: 5px;")
                           )
)

sidebar <- dashboardSidebar(
  radioButtons(inputId = "topic", label = "Select Topic",
               choices = topic_names, selected = NULL)
  
)

body <- dashboardBody(
  titlePanel(
    textOutput("chosen_topic")
    ),
  fluidRow(
    box(
      title = "Topic Proportions by Value of the Award*",
      width = 6,
      plotOutput("topic_per_band"),
      tags$h5("*Award values have been adjusted for inflation using 2023 CPI base year.")
    ),
    box(
      title = "Topic Proportions by Start Year",
      width = 6,
      plotOutput("selected_plot"),
      checkboxInput("checkbox", "Weighted by value of the award")
    )
  ),  
  
  fluidRow(
    column(
      width = 3,
      tags$h4("Most Frequent Words"),
      tableOutput("terms")
    ),
    column(
      width = 8,
      tags$h4("Most Relevant Projects"),
      uiOutput("textboxes")
    )
    
  )
)  



ui <- dashboardPage(header, sidebar, body)




# Server
server <- function(input, output) {
  
  observeEvent(input$about_btn, {
    showModal(
      modalDialog(
        title = "About this Data Project",
        HTML("
        <p>The AHRC Grants Topic Browser is built upon the findings of topic analysis conducted on research grant applications that have been awarded funding by the Arts and Humanities Research Council (AHRC) between 2013 and 2023. For a detailed description of the methodology, please refer to the project's <a href=https://github.com/kuslitsanna/AHRC_awards>GitHub repository</a>.</p>
        
        <h4>Topics</h4>
        <p>The 32 topics identified here have been generated using a combination of unsupervised and semi-supervised machine learning techniques (LDA and seeded LDA) in a heuristic manner. The goal was to arrive at a classification of the documents in the corpus that is both statistically robust and intuitively meaningful to a human observer. I have labeled the emerging topics based on my interpretation of the documents identified by the model as most strongly associated with the given category (Most Relevant Projects), and the cluster of terms idenified as having the highest probability of appearing in the associated documents (Most Frequent Words). The topic labels are of necessity imperfect. When selecting them, my aim was to find broad concepts that best capture the semantic overlap within each category.</p>
        
        <h4>Data Source</h4>
        <p>The data analysed here is sourced from publicly available information provided by the UK Research and Innovation (UKRI) available at <a href=https://gtr.ukri.org/>Gateway to Research (GtR)</a>. The analysis focused on research grant applications, excluding studentships, fellowships, and training grants awarded by the AHRC.</p>
        
        <h4>Credits</h4>
        <p>
        Author: <a href=https://github.com/kuslitsanna>Anna Kuslits</a>
        </p>
        <p>
        Acknowledgments: The analysis was performed using the <a href=http://quanteda.io/>quanteda</a> and <a href=https://koheiw.github.io/seededlda/>seededLDA</a> R packages, developed by Kenneth Benoit and Kohei Watanabe at the LSE Data Science Institute. In visualising the results and designing the dashboard, I drew inspiration from <a href=https://dsl.richmond.edu/dispatch/introduction>Mining the Dispatch</a>, created by Robert K. Nelson and the Digital Scholarship Lab at the University of Richmond.
        </p>
      "),
        footer = modalButton("Close")
      )
    )
  })
  
  output$chosen_topic <- renderText({
    selected_topic <- input$topic
    selected_topic <- gsub("_", " ", selected_topic)  # Remove underscores
    selected_topic <- tools::toTitleCase(selected_topic)  # Capitalize the title
    paste(selected_topic)
  })
  
  
  # Define a reactive expression for filtered_per_band
  filtered_per_band <- reactive({
    topic_proportion_per_band %>% filter(Topic == input$topic)
  })
  
  # Define a reactive expression for filtered_per_year
  filtered_per_year <- reactive({
    topic_proportion_per_year %>% filter(Topic == input$topic)
  })
  # Reactive for filtered_terms
  filtered_terms <- reactive({
    terms_lists %>% filter(Topic==input$topic) %>% select(-Topic)
  })
  
  # Define a reactive expression for filtered_abstracts
  filtered_abstracts <- reactive({
    topic_proportions_long %>% filter(Topic == input$topic) %>%
      arrange(desc(Proportion)) %>%
      inner_join(master_df, by = "ProjectReference") %>%
      filter(Proportion >= 0.1) %>% mutate(Proportion=round(Proportion, 2))
  })
  
  # Render the topic_per_band plot
  output$topic_per_band <- renderPlot({
    ggplot(filtered_per_band(), aes(x = AwardPoundsBand, y = Proportion)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(
        #title = "Topic Proportions by Value of the Award",
        x = "Value of Award (thousand pounds)",
        y = "Topic Proportion"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the selected plot based on the switch value
  selected_plot <- reactive({
    if (input$checkbox) {
      ggplot(filtered_per_year(), aes(x = Year, y = Proportion)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(
          x = "Year",
          y = "Topic Proportion"
        ) +
        theme_minimal()
    } else {
      ggplot(filtered_per_year(), aes(x = Year, y = WeightedProportion)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(
          x = "Year",
          y = "Topic Proportion"
        ) +
        theme_minimal()
    }
  })
  
  # Render the selected plot
  output$selected_plot <- renderPlot({
    selected_plot()
  })
  
  # Render the terms table
  output$terms <- renderTable(
    filtered_terms(),
    colnames = FALSE
  )
  
  # Render the text boxes
  output$textboxes <- renderUI({
    text_boxes <- lapply(1:nrow(filtered_abstracts()), function(i) {
      text <- paste(
        "<b>", "Topic relevance:", "</b>"," ", filtered_abstracts()$Proportion[i]*100, "%", "<br>",
        "<b>","Title:","</b>"," ", filtered_abstracts()$Title[i],"<br>",
        "<b>","Start date:","</b>"," ", filtered_abstracts()$StartDate[i],"<br>",
        "<b>","End date:","</b>"," ", filtered_abstracts()$EndDate[i],"<br>",
        "<b>","Lead organisation:","</b>"," ", filtered_abstracts()$LeadROName[i],"<br>",
        "<b>","PI:","</b>"," ", filtered_abstracts()$PI[i],"<br>",
        "<b>", "Award:","</b>"," £", filtered_abstracts()$AwardPounds[i],"<br>",
        "<a href=", filtered_abstracts()$url[i],">Open in browser</a>", "<br>",
        "<b>","Abstract:","</b>","<br>", filtered_abstracts()$abstract[i],"<br>",
        "<b>","Impact:","</b>","<br>", filtered_abstracts()$impact[i],
        sep=""
      )
      div(
        style = "width: 100%; height: 200px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; margin-bottom: 10px;",
        HTML(text)
      )
    })
    div(text_boxes)
  })
}




# Run the Shiny app
shinyApp(ui, server)

