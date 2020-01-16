install.packages("tau")
install.packages("XML")

# ---- Load Libraries to use the string funcions and XML functions ----
library(shiny)
library(dplyr)
library(ggplot2)
library(XML)
library(tau)

# Import the data into a Data-Frame
setwd("/Users/gandara/Documents/master data science paper/UvA")
getwd()

#this function will read the XML file and store it in a DATA FRAME format to later manipulate to count the words
xmldataframe <- xmlToDataFrame("data-science-A.xml")
myFactor1 <- c("1", "2", "3")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Mining Words!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      #To select the file that you want to parse / mine by words
      radioButtons("documentNo",
                   label = "Select a Document to Mine:",
                   choices = myFactor1),
      
      # Input: Slider to select TOP N instances of each word / with more frequency ----
      sliderInput(inputId = "topN",
                  label = "Top N:",
                  min = 1,
                  max = 25,
                  value = 10),
      
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type to calculate the FREQ of the WORD
      textInput(inputId = "wordNo",
                label = "Word lower case:",
                value = "of")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Output: Formatted text for caption ----
      h3(textOutput("wordNo", container = span)),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
    )
  )
)

#establish the font, size, color for the Axis and Title
red.bold.italic.text <- element_text(face = "bold", color = "black", size = 12)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "red", size = 16)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  datasetInput <- reactive({
    
    #take the selected TEXT for the Selected Document in the radio button
    oz <- xmldataframe$text[as.numeric(input$documentNo)]
    #convert the TEXT to lower case to find the word no matter the uppercase
    oz.counts <- textcnt(oz, n=1, method="string", tolower=T)
    #create a data frame with all the words and the number of instances
    oz.counts.df <- data.frame(word = names(oz.counts), count = c(oz.counts))
    
    #in case you want to find an specific word and check the number of ocassions that appear in the TEXT you can use this:
    #oz.counts.df[oz.counts.df$word == "author",2]
    
    #dataset will have the dataframe selected
    dataset <- oz.counts.df  %>% 
      arrange(desc(count)) %>% slice(1:as.numeric(input$topN))
  })
  
  #it will plot the bar chart using the parameter of the TOP N results for that specific dataframe
  output$distPlot <- renderPlot({
    ggplot(data = datasetInput()) +
      geom_bar(colour="black", mapping = aes(x = word, y = count), fill = 5555, stat = "identity", 
               position=position_dodge(),
               size=.3) +                        # Thinner lines
                xlab(paste("TOP ", input$topN)) + ylab("Number of Instances of the WORD") + # Set axis labels
      ggtitle(paste("Document:", xmldataframe$docno[as.numeric(input$documentNo)], " ", 
              xmldataframe$section[as.numeric(input$documentNo)])) +
      theme(title = blue.bold.italic.16.text, axis.title = red.bold.italic.text)
  })
  
  output$wordNo <- renderText({
    paste0("The frequency of the word '", input$wordNo, "' is: ", datasetInput()[datasetInput()$word == input$wordNo,2])
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$topN)
  })
}

# Run app
shinyApp(ui,server)
