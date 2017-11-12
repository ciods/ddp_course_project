library(shiny)

# https://ciods.shinyapps.io/firstapp/
# https://ciods.github.io/ddp_course_project/readme.html

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Analysis of the web page stats"),
    p("This app will analyse some basic stats for the URL provided. It will download the web page, and parse the source code in order to provide some counts on the words and letters used in the page. Just enter the URL in the field below and click on Go. Afterwards, you can play with word length settings to filter out which words to include in the analysis."),
    p("Link to github repository:"),
    a("https://github.com/ciods/ddp_course_project", title = "Link to github repository"),
    p("Link to the app documentation:"),
    a("https://ciods.github.io/ddp_course_project/readme.html", title = "app documentation"),
    
    # Sidebar with controls
    sidebarLayout(
        sidebarPanel(
            textInput("url_txt", "Enter URL:", value = "http://"),
            actionButton("goButton", "Go!"),
            br(),br(),
            h4("Settings"),

            sliderInput("word_min", "Min word length:",
                        min = 1, max = 15, value = 3),
            sliderInput("word_max", "Max word length:",
                        min = 1, max = 15, value = 8),
            em("Bonus plot"),
            checkboxInput("show_tags", "html tags", value=FALSE),

            actionButton("stop_btn", "Shutdown the app!")
        ),
        
        # Show our analysis
        mainPanel(
            tags$table(
                tags$tr(
                    tags$td(h4("Total Word count:")),
                    tags$td( tags$b(textOutput("wd_count")) )
                )
            ),
            tags$table(
                tags$tr(
                    tags$td(
                        plotOutput("plot1", width = "350px", height = "300px")
                    ),
                    tags$td(
                        plotOutput("plot2", width = "450px", height = "300px")
                    )
                )
            ),
            plotOutput("plot3", width = "800px", height = "400px")
        )
    )
))
