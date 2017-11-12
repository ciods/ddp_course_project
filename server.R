library(shiny)
library(XML)
library(httr)
library(stringr)

get_html_code <- function(c) {
    html2 = GET(c)
    content2 = content(html2, as="text")
    content2
}

get_html_doc <- function(c) {
    plain.text <- xpathSApply(c, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    paste(plain.text, collapse = " ")
}

get_parsed_html <- function(c) {
    parsedHtml = htmlParse(c, asText = TRUE)
    parsedHtml
}

# Define server logic
shinyServer(function(input, output, session) {
    
    html_code <- eventReactive(input$goButton, {
        code_str <- get_html_code(input$url_txt)
        gsub(" +", " ", code_str)
        
    })
    
    observeEvent(input$word_max,  {
        if (input$word_max < input$word_min) {
            updateSliderInput(session = session, inputId = "word_min", value = input$word_max)
        }
    })
    
    observeEvent(input$word_min,  {
        if (input$word_min > input$word_max) {
            updateSliderInput(session = session, inputId = "word_max", value = input$word_min)
        }
    })
    
    output$wd_count <- renderText({
        min_len <- input$word_min
        max_len <- input$word_max
        
        html_doc <- get_html_doc(get_parsed_html(html_code()))
        html_doc <- gsub(" +", " ", html_doc)
        
        # filter by each word length
        words <- c()
        for (i in strsplit(html_doc, " ")[[1]]) {
            if(nchar(i) >= min_len & nchar(i) <= max_len ) {
                words <- c(words, i)
            }
        }
        
        as.character(length(words))
    })

    # plot 1 (Counts by word length)
    output$plot1 <- renderPlot({
        min_len <- input$word_min
        max_len <- input$word_max
        
        html_doc <- get_html_doc(get_parsed_html(html_code()))
        html_doc <- gsub(" +", " ", html_doc)
        
        # calculate each word length
        ocur <- sapply(strsplit(html_doc, " ")[[1]], nchar)
        # and aggregate in a table
        ocur <- table(ocur)
        cnt <- as.vector(ocur)
        nm <- names(ocur)
        
        # filter columns by min/max
        col_nm <- vector()
        data <- vector()
        for (i in as.integer(nm)) {
            if(i >= min_len & i <= max_len ) {
                data <- c(data, cnt[i])
                col_nm <- c(col_nm, as.character(i))
            }
        }
        
        # draw the bar plot with the specified number of bins
        barplot(data, names.arg = col_nm, col = 'lightblue', border = 'white', xlab="word length", ylab="count", 
                main = "Counts by word length (in characters)")
        
    })
    
    # plot 2 (Counts by consonants)
    output$plot2 <- renderPlot({
        min_len <- input$word_min
        max_len <- input$word_max
        
        html_doc <- toupper(get_html_doc(get_parsed_html(html_code())))
        html_doc <- gsub(" +", " ", html_doc)
        
        # filter by each word length
        str_filtered <- ""
        for (i in strsplit(html_doc, " ")[[1]]) {
            if(nchar(i) >= min_len & nchar(i) <= max_len ) {
                str_filtered <- paste(str_filtered, i)
            }
        }
        
        # filter columns by min/max
        col_nm <- vector()
        data <- vector()
        for (i in LETTERS) {
            data <- c(data, str_count(str_filtered, i))
            col_nm <- c(col_nm, i)
        }
        
        # draw the bar plot
        barplot(data, names.arg = col_nm, col = 'lightblue', border = 'white', xlab="letters", ylab="count", 
                cex.axis = 0.7, cex.names = 0.6, main = "Counts by letters")
    })
    
    # plot 3 (Counts by html tags)
    output$plot3 <- renderPlot({
        if(input$show_tags){
            min_len <- input$word_min
            max_len <- input$word_max
            
            html_code <- tolower(html_code())
            html_code <- gsub("<div ", "<div>", html_code, ignore.case = TRUE)
            html_code <- gsub("<a ", "<a>", html_code, ignore.case = TRUE)
            html_code <- gsub("<form ", "<form>", html_code, ignore.case = TRUE)
            html_code <- gsub("<table ", "<table>", html_code, ignore.case = TRUE)
            html_code <- gsub("<tr ", "<tr>", html_code, ignore.case = TRUE)
            html_code <- gsub("<td ", "<td>", html_code, ignore.case = TRUE)
            html_code <- gsub("<ul ", "<ul>", html_code, ignore.case = TRUE)
            html_code <- gsub("<li ", "<li>", html_code, ignore.case = TRUE)
            html_code <- gsub("<span ", "<span>", html_code, ignore.case = TRUE)
            html_code <- gsub("<p ", "<p>", html_code, ignore.case = TRUE)
            
            tags <- c("<html>","<div>","<a>","<form>","<table>","<tr>","<td>","<ul>","<li>","<h1>","<h2>","<h3>","<script>","<span>","<p>")
            
            # filter columns by min/max
            col_nm <- vector()
            data <- vector()
            for (i in tags) {
                data <- c(data, str_count(html_code, i))
                col_nm <- c(col_nm, i)
            }
            
            # draw the bar plot
            barplot(data, names.arg = col_nm, col = 'lightblue', border = 'white', xlab="html tags", ylab="count", 
                    las = 2, main = "Counts by html tags")
        }
    })
    
    # shutdown the app on click
    observe({
      if(input$stop_btn > 0){
          stopApp()
      }
  })
  
})
