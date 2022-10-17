# library(qrcode)
library(shiny)
source("hidden_createPDF_JZ.R")

# UI
  ui<-fluidPage(
    # App title ----
    titlePanel("Generate Barcodes for Label Printing"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      # choose file
      sidebarPanel(
        fluidRow(
          column(width = 6,
         fileInput("labels", 
            "1. Choose a text file of ID codes.", 
            multiple=FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
         column(width = 2,
         radioButtons("sep", "Separator",
            choices = c(Comma = ",", Tab = "\t"),
            selected = ",")),
         column(width = 2,
         checkboxInput("header", "Header in file?", value=TRUE))
         ),
         # Select variables to display ----
         uiOutput("select_column"),
         p(strong("3. Custom text (can add multiple times)")),
         uiOutput("add_text"),
         actionButton("textBn", "Add/append text"),
         actionButton("reset_text", "Reset text"),
         p(strong("4. (Optional) Modify PDF from default values")),
         textInput("filename", "Output PDF file name", value = "LabelsOut"),
         fluidRow(
           column(width = 3,
         selectInput("type","Barcode Type", 
                      choices = list("Linear (1D)" = "linear",
                                     "Matrix (2D)" = "matrix"),
                      multiple = FALSE)),
         column(width = 6,
         numericInput("font_size", 
                       "Font Size (auto-adjusted smaller to fit)", 
                       value = 12, min = 2, max = 100)),
         column(width = 3,
         selectInput(inputId = "fontfamily", 
                     label = "Font to use", 
                     choices = c(
                       "mono" = "mono",
                       "sans"="sans",
                       "serif"= "serif"),
                     multiple=FALSE))
         ),
        selectInput(
          inputId = "label_type",
          label   = "Preset Label Type (you can customize parameters below)",
          c('Avery 5967 (0.5" x 1.75")' = "avery5967", 'Avery 5960 (1.0" x 2.63")' = "avery5960")
        ),
         fluidRow(
           column(width = 3,
         numericInput("page_width", 
                             "Page Width (in)", 
                             value = 8.5, min = 1, max = 20, width=NULL,
                             step = 0.5)),
         column(width = 3,
         numericInput("page_height", 
                             "Page Height (in)", 
                             value = 11, 
                             min = 1, max = 20, width=NULL, step = 0.5)),

           column(width = 3,
         numericInput("width_margin", 
                             "Side margin (in)", 
                             value = 0.3, 
                             min = 0, max = 20, width=NULL, step = 0.05)),
         column(width = 3,
         numericInput("height_margin", 
                             "Top margin (in)", 
                             value = 0.5, min = 0, max = 20, width=NULL, step = 0.05))
         ),
         fluidRow(
           column(width = 3,
         numericInput("label_width", 
                             "Label width (in)", 
                             value = 1.75, min=0, max=100)),
         column(width = 3,
         numericInput("label_height", 
                             "Label height (in)", 
                             value = 0.5, min=0, max=100)),
         column(width = 3,
                numericInput("numrow", 
                             "No. of rows", 
                             value = 20, min = 1, max = 100, width=NULL, step = 1)),
         column(width = 3,
                numericInput("numcol", 
                             "No. of columns", 
                             value = 4, min = 1, max = 100, width=NULL, step = 1))
         ),
         fluidRow(
           column(width = 6,
         numericInput("barcode_height", 
                             "Relative Height of 1D barcode (0-1)", 
                             value = 0.5, min=0, max=1)),
         column(width = 6,
         radioButtons("barcode_at_top", 
                             "1D barcode on top?", 
                             choices = c(Yes = TRUE, No = FALSE),
                             selected = TRUE, inline = TRUE))
         ), width = 6
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        h4(strong("Preview")),
        plotOutput("label_preview", height = "auto", width = "auto"),
        # label preview datatable
        DT::DTOutput("check_make_labels"),
        # code snippet
        br(),
        p(strong("Reproducible Code")),
        tags$body("Not: the 'at_text' parameter need to adjusted."),
        verbatimTextOutput("PDF_code_render"),
        p(strong("5. Make PDF")),
        actionButton("make_pdf", "Make PDF"),
        p(strong("6. Download PDF")),
        tags$body("Wait for 'Done' to show up before downloading PDF file"),
        br(),
        downloadButton('download', 'Download PDF'),
        # status of pdf making
        textOutput("PDF_status"),
        br(),
        br(),
        h4(strong("Help")),
        tags$body("Please check the github page for help:"),
        tags$a("https://github.com/pinbo/barcode_labels", href="https://github.com/pinbo/barcode_labels"),
        br(),
        tags$body("Basically, you just need to upload a comma or tab delimited text file and select the columns to make labels. The main function is below."),
        verbatimTextOutput("function_help"),
        width = 6
      )
    )
  )
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    # pdf making server side
    # check label file
    mydata = reactiveVal()
    Labels_pdf<-reactive({
      req(input$labels)
      df<-read.csv(input$labels$datapath, header=input$header, stringsAsFactors = FALSE, sep = input$sep)
      df$text2add = "" # add an column for text to add
      mydata(df)
      df
    })
    
    # select column to make labels
    # Dynamically generate UI input when data is uploaded ----
    output$select_column <- renderUI({
      selectInput(inputId = "barcode_var", 
                  label = "2. Choose the variable for creating barcode", 
                  choices = names(Labels_pdf()),
                  multiple=FALSE)
    })
    
    # add text
    output$add_text <- renderUI({
      fluidRow(
      column(
        width = 4,
        textInput("prefix",  "Text Prefix", value = "")
      ),
      
      column(
        width = 4,
        selectInput(
          inputId = "variable",
          label   = "Variable to add",
          choices = c("Choose" = "", names(Labels_pdf()))
        )
      ),
      
      column(
        width = 4,
        radioButtons("newline", 
                     "Add line break?", 
                     choices = c(Yes = "\n", No = ""),
                     selected = "\n", inline = T),
      )
      )
    })
    # add more text
    observeEvent(input$textBn, {
      df2 =  mydata()
      df2$text2add <- paste0(df2$text2add, input$prefix, df2[,input$variable],input$newline)
      mydata(df2)
    })
    observeEvent(input$reset_text, {
      df2 =  mydata()
      df2$text2add <- ""
      mydata(df2)
    })
    ## preset labels
    
    observeEvent(input$label_type, {
      if (input$label_type == "avery5967"){
        updateNumericInput(session, "numrow", value = 20)
        updateNumericInput(session, "numcol", value = 4)
        updateNumericInput(session, "page_width", value = 8.5)
        updateNumericInput(session, "page_height", value = 11)
        updateNumericInput(session, "width_margin", value = 0.3)
        updateNumericInput(session, "height_margin", value = 0.5)
        updateNumericInput(session, "label_width", value = 1.75)
        updateNumericInput(session, "label_height", value = 0.5)
      }
     else if (input$label_type == "avery5960"){
       updateNumericInput(session, "numrow", value = 10)
       updateNumericInput(session, "numcol", value = 3)
       updateNumericInput(session, "page_width", value = 8.5)
       updateNumericInput(session, "page_height", value = 11)
       updateNumericInput(session, "width_margin", value = 0.19)
       updateNumericInput(session, "height_margin", value = 0.5)
       updateNumericInput(session, "label_width", value = 2.63)
       updateNumericInput(session, "label_height", value = 1.0)
     }
    }, ignoreInit = TRUE)
    
    
    # preview label file
    # output$check_make_labels<-DT::renderDataTable(
    #   mydata(), 
    #   server = FALSE, 
    #   selection = list(mode = "single", target = "column", selected = 1))
    output$label_preview <- renderImage({
      labeltext = mydata()[1, "text2add"]
      # if (labeltext == "" || is.null(labeltext)) labeltext = Labels_pdf()[1, input$barcode_var]
      tt = mydata()[1,"text2add"]
      nline_text = nchar(gsub("[^\n]", "", tt)) + 1 # number of lines
      text_height = input$label_height * (1 - input$barcode_height)
      Fsz = input$font_size
      if(input$type == "matrix") {
        ## vp for the qrcode within the grid layout
        code_vp <- grid::viewport(
          x=grid::unit(0.02, "npc"), 
          y=grid::unit(0.90, "npc"), 
          width = grid::unit(0.8 * input$label_height, "in"), 
          height = grid::unit(0.8 * input$label_height, "in"), 
          just=c("left", "top"))
        ## vp for the text label within the grid layout
        label_vp <- grid::viewport(
          x=grid::unit(0.95 * input$label_height, "in"),
          y=grid::unit(0.5, "npc"), 
          width = grid::unit(0.7 * input$label_width, "in"), 
          height = grid::unit(0.8 * input$label_height, "in"), 
          just=c("left", "center"))
        # adjust font size
        Fsz = ifelse(Fsz * nline_text > input$label_height *0.8 * 72, text_height *0.8 * 72 / nline_text, Fsz)
        label_plot <- qrcode_make(Labels = Labels_pdf()[1, input$barcode_var], ErrCorr = "H")
      } else {
        code_vp <- grid::viewport(
          x=grid::unit(0.05, "npc"), 
          y=grid::unit(input$barcode_height, "npc"), 
          width = grid::unit(0.9 * input$label_width, "in"), 
          height = grid::unit(input$barcode_height * input$label_height * 0.9, "in"), # *0.9 to leave spaces at bottom
          just=c("left", "top"))
          
          label_vp <- grid::viewport(
           x=grid::unit(0.05, "npc"), 
           y=grid::unit(0.95, "npc"),
           width = grid::unit(0.9 * input$label_width, "in"),
           height = grid::unit(text_height * 0.9, "in"), 
           just = c("left", "top"))
          # if barcode needs to be at top
          if (input$barcode_at_top){
            code_vp <- grid::viewport(
              x=grid::unit(0.05, "npc"), 
              y=grid::unit(0.95, "npc"), 
              width = grid::unit(0.9 * input$label_width, "in"), 
              height = grid::unit(input$barcode_height * input$label_height * 0.8, "in"), # *0.9 to leave spaces
              just=c("left", "top"))
            
            label_vp <- grid::viewport(
              x=grid::unit(0.05, "npc"), 
              y = grid::unit((1 - input$barcode_height), "npc"), 
              width = grid::unit(0.9 *input$label_width, "in"),
              height = grid::unit(text_height * 0.9, "in"), 
              just = c("left", "top"))
          }
          Fsz = ifelse(Fsz * nline_text > text_height * 72, text_height * 72 / nline_text, Fsz)
          label_plot <- code_128_make(Labels = Labels_pdf()[1, input$barcode_var])
      }
      outputfile <- tempfile(fileext=".png")
      grDevices::png(outputfile,
                     width = input$label_width,
                     height = input$label_height,
                     units = "in", res=300, family = input$fontfamily
                     )
      grid::grid.rect()
      grid::pushViewport(code_vp)
      grid::grid.draw(label_plot)
      grid::popViewport()
      grid::pushViewport(label_vp)
      grid::grid.text(label = labeltext, #Labels_pdf()[1, input$barcode_var], 
                      gp = grid::gpar(fontsize = Fsz, lineheight = 0.8))
      grDevices::dev.off()
      list(src = outputfile,
           width = 100 * input$label_width, 
           height = 100 * input$label_height,
           alt = "Label Preview")
    }, deleteFile = TRUE
    )
    # text indicator that pdf finished making
    PDF_done<-eventReactive(input$make_pdf, {
      custom_create_PDF(
        Labels = mydata()[,input$barcode_var],
        name = input$filename, 
        type = input$type, 
        Fsz = input$font_size, 
        numrow = input$numrow, 
        numcol = input$numcol, 
        page_width = input$page_width, 
        page_height = input$page_height, 
        height_margin = input$height_margin, 
        width_margin = input$width_margin, 
        label_width = input$label_width, 
        label_height = input$label_height, 
        # x_space = input$x_space, 
        # y_space = input$y_space,
        replace_label = TRUE,
        alt_text = mydata()[,"text2add"],
        fontfamily =input$fontfamily,
        showborder = FALSE, # whether to show border of labels
        barcode_at_top = input$barcode_at_top,
        barcode_height = input$barcode_height
        )
      status<-"Done!"
      status
    })
    PDF_code_snippet<-reactive({
      noquote(
        paste0("custom_create_PDF(Labels = label_csv[, \'",input$barcode_var, 
               "\'], name = \'", input$filename, "\', 
               type = \'", input$type, 
               "\', Fsz = ", input$font_size, 
               ", numrow = ", input$numrow, 
               ", numcol = ", input$numcol, 
               ", page_width = ", input$page_width, 
               ", page_height = ", input$page_height, 
               ", width_margin = ", input$width_margin, 
               ", height_margin = ", input$height_margin, 
               ", label_width = ", input$label_width, 
               ", label_height = ", input$label_height, 
               # ", x_space = ", input$x_space, 
               # ", y_space = ", input$y_space,
               ", replace_label = TRUE",
               ", alt_text = paste0('Plot: ', label_csv$plot_name, '\nAcc: ', label_csv$accession_name)",
               ", fontfamily = \'", input$fontfamily,
               "\', barcode_at_top = ", input$barcode_at_top,
               ", barcode_height = ", input$barcode_height,
               ")"))
    })
    csv_code_snippet<-reactive({noquote(
      paste0(
        "label_csv <- read.csv( \'", input$labels$name,
        "\', header = ", input$header, ", sep = \'", input$sep,
        "\', stringsAsFactors = FALSE)"))})
    output$PDF_code_render<-renderText({
      paste(csv_code_snippet(), PDF_code_snippet(), sep = "\n")
    })
    
    output$function_help <- renderText({
      'custom_create_PDF <- function(
    Labels = NULL, # vector containing label names used for generating barcodes
    name = "LabelsOut", # pdf output file name
    type = "linear", # "linear" for code128, or "linear2" for extended code128, or "matrix" for QR code
    ErrCorr = "H", #  error correction value for matrix labels only: (L = Low (7%), M = Medium (15%), Q = Quantile (25%), H = High (30%)
    Fsz = 12, # font size, will be adjusted to fit text space
    Across = TRUE, # logical. When TRUE, print labels across rows, left to right. When FALSE, print labels down columns, top to bottom.
    ERows = 0, # number of rows to skip.
    ECols = 0, # number of columns to skip.
    trunc = FALSE, # logical. Text is broken into multiple lines for longer ID codes, to prevent printing off of the label area. Default is TRUE. If trunc = FALSE, and text is larger than the physical label, the text will be shrunk down automatically.
    numrow = 20, # Number of label rows per page
    numcol = 4, # Number of columns per page
    page_width = 8.5, # page width in inch
    page_height = 11, # page height in inch
    width_margin = 0.3, # side margin in inch
    height_margin = 0.5, # top margin in inch
    label_width = 1.75, # label width in inch
    label_height = 0.5, # label height in inch
    x_space = 0, # A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when type = "matrix". 
    y_space = 0.5, # A value between 0 and 1. This sets the distance between the QR code and text of each label. Only applies when type = "matrix". 
    alt_text = NULL, # vector containing alternative names that are printed along with Labels BUT ARE NOT ENCODED in the barcode image.
    replace_label = TRUE, # logical. Replace label text with alt_text.
    fontfamily = "mono", # "mono", "sans", "serif"
    showborder = FALSE, # whether to show border of labels
    barcode_at_top = FALSE, # whether to print barcode at the top of the label
    barcode_height = 0.5 # 0-1, proportion of the label height
    )'
    })
    # label preview
    
    # rendering of pdf indicator
    output$PDF_status<-renderPrint({cat("\n",PDF_done())})
    # download all the files
    output$download <- downloadHandler(
      filename = paste0(input$filename, ".pdf"),
      content = function(file){
        file.copy(paste0(input$filename, ".pdf"), file)
      })

    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    observeEvent(input$done, {
      
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })
  }
  shinyApp(ui = ui, server = server)

