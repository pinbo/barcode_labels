# 2023-08-15: draggable viewport using interact.js
library(shiny)
# devtools::install_github("pinbo/barcodeLabel")
library(barcodeLabel)
# function to plot barcode to an image
plotbarcode = function(text, barcode_type=c("linear", "qr", "dm")){
  if (barcode_type=="linear") code = code_128_make(text)
  else if (barcode_type=="qr")  code = qrcode_make(text, 3)
  else if (barcode_type=="dm") code = dmcode_make(text)
  # else {stop("Barcode type must be linear, qr or dm")}
  else code = grid::textGrob(text)
  grid::grid.draw(code)
}
# UI
ui<-fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "label_design.css")
  ),
  tags$script(src = "https://cdn.jsdelivr.net/npm/interactjs/dist/interact.min.js"),
  tags$script(src = "drag_resize.js"),
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
                            selected = "\t")),
        column(width = 2,
               checkboxInput("header", "Header in file?", value=TRUE))
      ),
      # p(strong("Or input manually if all labels are the same")),
      fluidRow(
        column(width = 8,
               textInput("simpleText", "Or input manually if all labels are the same (use '\\n' for line break)", value = "abc\\n123")),
        column(width = 3,
               numericInput("simpleTextRepeat", 
                            "Number of labels", 
                            value = 4, min = 1))
      ),
      actionButton("createDataset", "Create Dataset"),
      # Select variables to display ----
      uiOutput("select_column"),
      p(strong("3. Custom text (can add multiple times)")),
      uiOutput("add_text"),
      actionButton("textBn", "Add/append text"),
      actionButton("undo_once", "Undo once"),
      actionButton("reset_text", "Reset text"),
      # br(),
      p("New variable preview:"),
      # uiOutput("preview_new_var"),
      textOutput("preview_new_var"),
      actionButton("make_new_var", "Finish"),
      # design label
      p(strong("4. Design label")),
      withTags({
        div(
          div(id="drawing-area", class="grid" ),
        )
      }),
      uiOutput("select_content"),
      p(strong("5. (Optional) Modify PDF from default values")),
      fluidRow(
        column(width = 2,
               textInput("filename", "Output PDF name", value = "LabelsOut")),
        column(width = 2,
               selectInput("type","Barcode Type", 
                           choices = list("Linear (1D)" = "linear",
                                          "QR Code" = "qr",
                                          "Data Matrix" = "dm",
                                          "No barcode (only text)" = "null"),
                           selected = "dm",
                           multiple = FALSE)),
        column(width = 2,
               numericInput("font_size", 
                            "Font Size", 
                            value = 12, min = 2, max = 100)),
        column(width = 2,
               selectInput(inputId = "fontfamily", 
                           label = "Font", 
                           choices = c(
                             "mono" = "mono",
                             "sans"="sans",
                             "serif"= "serif"),
                           multiple=FALSE)),
        column(width = 2,
               textInput("font_col", "Text color", value = "black")),
        column(
          width = 2, selectInput("fontface0", "Font face overall", choices = c(plain=1, bold=2, italic=3, boldItalic=4)))
      ),
      fluidRow(
        column(width = 3,
               selectInput(
                 inputId = "label_type",
                 label   = "Preset Label Type",
                 c('Avery 5967 (0.5" x 1.75")' = "avery5967", 'Avery 5960 (1.0" x 2.63")' = "avery5960", 'Tough Spots 3/8in' = 'Tough-Spots-3/8in')
               )),
        column(width = 3,
               selectInput(
                 inputId = "ecl",
                 label   = "QR Error Correct",
                 c("Low (7%)" = 1, "Medium (15%)" = 2," Quantile (25%)" = 3, "High (30%)"=4), selected = 2
               )),
        column(width = 3,
               #sliderInput("barcode_height", label = "Barcode Height", min = 0, max = 1, value = 1)
               numericInput("barcode_height", 
                            "Barcode Height (0-1)", 
                            value = 1, min=0, max=1)),
        column(width = 3,
               numericInput("barcode_scale", 
                            "Barcode Scale (0-1)", 
                            value = 1, min=0, max=1))
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
        column(width = 3,
               radioButtons("barcode_on_top", 
                            "1D barcode on top?", 
                            choices = c(Yes = TRUE, No = FALSE),
                            selected = TRUE, inline = TRUE)),
        column(
          width = 3,
          radioButtons("text_align", 
                       "Text alignment", 
                       choices = c("left", "center"),
                       selected = "center", inline = TRUE)),
        column(
          width = 3,
          radioButtons("showborder", 
                       "Show border for output?", 
                       choices = c(Yes = TRUE, No = FALSE),
                       selected = FALSE, inline = TRUE)
        ),
        column(
          width = 3,
          radioButtons("border_type", 
                       "Rectangle or circle border?", 
                       choices = c("rectangle", "circle", "both"),
                       selected = "rectangle", inline = TRUE)
        )
      ), 
      fluidRow(
        column(width = 4,
               numericInput("ERow", 
                            "Label print from Row", 
                            value = 1, min=1, max=100)),
        column(width = 4,
               numericInput("ECol", 
                            "Label print from Column", 
                            value = 1, min=1, max=100))
      ),
      width = 8
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h4(strong("Preview")),
      br(),
      plotOutput("label_preview", height = "auto", width = "auto"),
      br(),
      plotOutput("barcode_preview", height = "auto", width = "auto"),
      br(),
      p(strong("6. Make PDF")),
      actionButton("make_pdf", "Make PDF"),
      p(strong("7. Download PDF")),
      tags$body("Wait for 'Done' to show up before downloading PDF file"),
      br(),
      downloadButton('download', 'Download PDF'),
      # status of pdf making
      textOutput("PDF_status"),
      br(),
      br(),
      h4(strong("Help")),
      tags$body("This is a shiny application for R package "),
      tags$a('"barcode_labels"', href="https://github.com/pinbo/barcodeLabel"),
      tags$body(". The Shiny R script is here:"),
      tags$a("https://github.com/pinbo/barcode_labels", href="https://github.com/pinbo/barcode_labels"),
      br(),br(),
      tags$body("Basically, you just need to upload a comma or tab delimited text file and select the columns to make labels. For more complex label layout, please check out the "),
      tags$body(" R package "),
      tags$a('"barcodeLabels"', href="https://github.com/pinbo/barcodeLabel"),
      h5(strong("Notes")),
      tags$ol(
        tags$li("You can customize the label sizes by directly changing the parameters."), 
        tags$li("'Barcode Height' is for whole barcode area (1 means the label height x label height square on the left)"), 
        tags$li("'Barcode Scale' allow you to scale down the barcode plot, so you have more spaces around it."),
        tags$li("Show border: set to YES first to see how your text fit in the label sheets. Please set to NO for the final printing file."),
        tags$li("Please choose 'NO Scaling' or 'Actual size' when printing on a printer.")
      ),
      width = 4
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # pdf making server side
  # check label file
  mydata = reactiveVal()
  rawdata = reactiveVal()
  
  observeEvent(input$labels,{
    df<-read.csv(input$labels$datapath, header=input$header, stringsAsFactors = FALSE, sep = input$sep)
    rawdata(df)
    df$text2add = "" # add an column for text to add
    df$tmp = "" # store last step
    mydata(df)
  })
  
  observeEvent(input$createDataset, {
    if(input$simpleText != "" & input$simpleTextRepeat > 0){
      tt = gsub("\\\\n","\n",input$simpleText) # need this to get the correct line break
      df = data.frame(label=rep(tt, input$simpleTextRepeat))
      rawdata(df)
      df$text2add = "" # add an column for text to add
      df$tmp = "" # store last step
      mydata(df)
    }
  })
  
  
  # select column to make labels
  # Dynamically generate UI input when data is uploaded ----
  output$select_column <- renderUI({
    cc = ncol(rawdata())
    selectInput(inputId = "barcode_var", label = "2. Choose the variable for creating barcode", 
                # choices = names(mydata())[-c(cc-1, cc)], multiple=FALSE) 
                choices = names(rawdata()), multiple=FALSE)
  })
  # for manual layout
  output$select_content <- renderUI({
    df = mydata()
    cc = ncol(rawdata())
    data_choices = as.list(df[1,-c(cc+1, cc+2)])
    tagList(
    fluidRow(
      column(width = 2, 
    selectInput("input_type","Input Type", 
                choices = list("Linear (1D)" = "linear",
                               "QR Code" = "qr",
                               "Data Matrix" = "dm",
                               "Text" = "text"),
                selected = "dm", multiple = FALSE)),
    column(width = 2, 
      selectInput(inputId = "input_var", label = "Input variable", 
                choices = data_choices, multiple=FALSE)),
    column(width = 2, 
           numericInput(inputId = "input_var_size", label = "Font size (pt)",value = 8, min = 1)),
    column(width = 2,
           selectInput(inputId = "input_var_fontfamily", 
                       label = "Font", 
                       choices = c(
                         "sans"="sans-serif",
                         "serif"= "serif",
                         "monospace" = "monospace"),
                       multiple=FALSE)),
    column(width = 2,
           textInput("input_var_color", "Text color", value = "black")),
    column(
      width = 2, selectInput("input_var_fontface", "Font face", choices = c(plain=1, bold=2, italic=3, boldItalic=4)))
    ),
    actionButton("addButton", "Add", onClick="addBox()")
    )
  })
  
  # add text
  output$add_text <- renderUI({
    cc = ncol(rawdata())
    # fluidRow(
    #   column( width = 3, textInput("newvar",  "New variable name", value = "var1")  )
    # )
    fluidRow(
      column( width = 2, textInput("newvar",  "New variable name", value = "var1")  ),
      column( width = 2, textInput("prefix",  "Text Prefix", value = "")  ),
      column( width = 3, selectInput( inputId = "variable", label   = "Variable to add",
                                      choices = c(Choose = "", names(mydata())[-c(cc+1, cc+2)])   )
      ),
      column(
        width = 2, selectInput("fontface", "Font face", choices = c(plain=1, bold=2, italic=3, boldItalic=4))
      ),
      column(
        width = 3, radioButtons("newline", "Add line break?", choices = c(Yes = "\n", No = ""), selected = "", inline = T)
      )
    )
  })
  # add more text
  resetLabelTextInput = function(){
    # reset to all input as default
    updateTextInput(session, "prefix",value = "")
    updateSelectInput(session,"variable", selected="")
    updateRadioButtons(session,"newline",selected = "")
    updateSelectInput(session,"fontface",selected = 1)
  }
  observeEvent(input$textBn, {
    df2 =  mydata()
    fn = as.integer(input$fontface)
    fonts = c("", "**", "*", "***")
    df2$tmp = df2$text2add
    if (input$variable == "")
      df2$text2add <- paste0(df2$text2add, fonts[fn], input$prefix,fonts[fn],input$newline)
    else
      df2$text2add <- paste0(df2$text2add, fonts[fn], input$prefix, df2[,input$variable],fonts[fn],input$newline)
    df2[[input$newvar]]=df2$text2add
    mydata(df2)
    resetLabelTextInput()
  })
  observeEvent(input$reset_text, {
    df2 =  mydata()
    df2$tmp = df2$text2add
    df2$text2add <- ""
    mydata(df2)
    resetLabelTextInput()
  })
  observeEvent(input$undo_once, {
    df2 =  mydata()
    df2$text2add = df2$tmp
    mydata(df2)
    resetLabelTextInput()
  })
  observeEvent(input$make_new_var, {
    df2 =  mydata()
    df2[[input$newvar]]=df2$text2add
    df2$text2add = ""
    df2$tmp = ""
    mydata(df2)
    resetLabelTextInput()
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
    else if (input$label_type == "Tough-Spots-3/8in"){
      updateNumericInput(session, "numrow", value = 16)
      updateNumericInput(session, "numcol", value = 12)
      updateNumericInput(session, "page_width", value = 8.4375)
      updateNumericInput(session, "page_height", value = 11)
      updateNumericInput(session, "width_margin", value = 0.5625)
      updateNumericInput(session, "height_margin", value = 0.58)
      updateNumericInput(session, "label_width", value = 0.375)
      updateNumericInput(session, "label_height", value = 0.375)
      updateNumericInput(session, "font_size", value = 10)
      updateRadioButtons(session, "text_align", selected = "center")
      updateRadioButtons(session, "border_type", selected = "circle")
      updateSelectInput(session, "type", selected = "null")
    }
  }, ignoreInit = TRUE)
  # set text alignment and barcode height based on barcode type selection
  observeEvent(input$type, {
    if (input$type == "linear" | input$type == "null"){
      updateNumericInput(session, "barcode_height", value = 0.5)
      updateNumericInput(session, "barcode_scale", value = 1)
      updateRadioButtons(session, "text_align", selected = "center")
    } else {
      updateNumericInput(session, "barcode_height", value = 1)
      updateRadioButtons(session, "text_align", selected = "left")
    }
  }, ignoreInit = TRUE)
  
  # vp_list and content_list from manually adjusted layout
  # content_list = list()
  # vp_list = list()
  # observeEvent(input$sizeInfo, {
  #   ss = input$sizeInfo
  #   vp_list[[ss$id]] = grid::viewport(x = ss$x, y = ss$y, width = ss$width, height = ss$height, just = c("left", "top"))
  # })
  tmp_label_list = reactive({
    req(input$sizeInfo)
    vp_list = list()
    content_list = list()
    nms = names(input$sizeInfo)
    for (x in nms){
      ss = input$contentInfo[[x]]
      ss2 = input$sizeInfo[[x]]
      if (ss$type == "text") {
        tt = text_array_wrap(mydata()[1,ss$var], 12, ss2$width*input$label_width, ss2$height*input$label_height, input$fontfamily, useMarkdown = T)
        content = tt$text
        Fsz = tt$font_size
        cat("Final Font size used is", Fsz, "\n")
        vp = grid::viewport(x = ss2$x, y = 1-ss2$y, width = ss2$width, height = ss2$height, just = c("left", "top"), gp=grid::gpar(fontsize = Fsz, lineheight = 0.8, fontfamily=ss$fontfamily, fontface=as.numeric(ss$fontface), col=ss$fontcolor))
        vp_list[[x]] = vp
        content_list[[x]] = content
      } else {
        vp = grid::viewport(x = ss2$x, y = 1-ss2$y, width = ss2$width, height = ss2$height, just = c("left", "top"))
        if (ss$type == "dm") content = lapply(as.character(mydata()[1,ss$var]), dmcode_make)
        else if(ss$type == "qr") content = lapply(as.character(mydata()[1,ss$var]), qrcode_make)
        else if(ss$type == "linear") content = lapply(as.character(mydata()[1,ss$var]), code_128_make)
        vp_list[[x]] = vp
        content_list[[x]] = content
      }
    }
    cat("vp_list length:", length(vp_list),"\n")
    cat("content_list length:", length(content_list),"\n")
    print(vp_list)
    list(vp_list=vp_list, content_list=content_list)
  })
  
  # preview label file
  output$label_preview <- renderImage({
    vp_list = tmp_label_list()$vp_list
    content_list = tmp_label_list()$content_list
    outputfile <- tempfile(fileext=".png")
    grDevices::png(outputfile,
                   width = input$label_width,
                   height = input$label_height,
                   units = "in", res=300, family = input$fontfamily
    )
    if (input$border_type == "rectangle") grid::grid.rect()
    else if (input$border_type == "circle") grid::grid.circle(r=grid::unit(min(input$label_width, input$label_height)/2, "inches"))
    else{# both
      grid::grid.rect()
      grid::grid.circle(r=grid::unit(min(input$label_width, input$label_height)/2, "inches"))
    }
    
    if (length(vp_list) > 0){
      for (j in 1:length(vp_list)){
        vp = vp_list[[j]]
        content = content_list[[j]]
        grid::pushViewport(vp)
        if (input$showborder) grid::grid.rect()
        cat("fontsize used is", grid::get.gpar()$fontsize, "\n")
        if (class(content) == "list") {# grob list
          grid::grid.draw(content[[1]])
        } else {# text
          # grid::grid.text(label = content[1])
          if (input$text_align == "left"){
            # grid::grid.text(label = content[1], x = grid::unit(0, "npc"), just = "left")
            richtext(content[1], x=0, hjust=0, useMarkdown=T, gp = grid::get.gpar())
          } else {
            # grid::grid.text(label = content[1])
            richtext(content[1], useMarkdown=T, gp = grid::get.gpar())
          }
        }
        grid::popViewport()
      }
    }
    grDevices::dev.off()
    list(src = outputfile,
         width = 100 * input$label_width, 
         height = 100 * input$label_height,
         alt = "Label Preview")
  }, deleteFile = TRUE
  )

  # new variable preview
  output$preview_new_var<-renderPrint({
    req(mydata())
    cat("\n",mydata()[1,"text2add"],"\n")
  })
  
  ## barcode preview
  output$barcode_preview <- renderImage({
    req(mydata())
    input_type = input$input_type
    input_var = input$input_var
    outputfile <- tempfile(fileext=".png")
    grDevices::png(outputfile, width = 2, height = 2, units = "in", res=300, family = input$fontfamily)
    plotbarcode(input_var, input_type)
    grDevices::dev.off()
    list(src = outputfile,
         id = "barcodeImg",
         width = 100, 
         height = 100,
         alt = "barcode Preview")
  }, deleteFile = TRUE
  )
  
  # text indicator that pdf finished making
  PDF_done<-eventReactive(input$make_pdf, {
    # 1. create simple element layout on each label
    vp_list = list()
    content_list = list()
    nms = names(input$sizeInfo)
    for (x in nms){
      ss = input$contentInfo[[x]]
      ss2 = input$sizeInfo[[x]]
      if (ss$type == "text") {
        tt = text_array_wrap(mydata()[,ss$var], 12, ss2$width*input$label_width, ss2$height*input$label_height, input$fontfamily, useMarkdown = T)
        content = tt$text
        Fsz = tt$font_size
        cat("Final Font size used is", Fsz, "\n")
        vp = grid::viewport(x = ss2$x, y = 1-ss2$y, width = ss2$width, height = ss2$height, just = c("left", "top"), gp=grid::gpar(fontsize = Fsz, lineheight = 0.8, fontfamily=ss$fontfamily, fontface=as.numeric(ss$fontface), col=ss$fontcolor))
        vp_list[[x]] = vp
        content_list[[x]] = content
      } else {
        vp = grid::viewport(x = ss2$x, y = 1-ss2$y, width = ss2$width, height = ss2$height, just = c("left", "top"))
        if (ss$type == "dm") content = lapply(as.character(mydata()[,ss$var]), dmcode_make)
        else if(ss$type == "qr") content = lapply(as.character(mydata()[,ss$var]), qrcode_make)
        else if(ss$type == "linear") content = lapply(as.character(mydata()[,ss$var]), code_128_make)
        vp_list[[x]] = vp
        content_list[[x]] = content
      }
    }
    label_list = list(vp_list=vp_list, content_list=content_list)

    make_custom_label(
      label_number = nrow(mydata()), # how many labels to print
      name = input$filename, # pdf output file name
      fontfamily = input$fontfamily, # "mono", "sans", "serif"
      showborder = input$showborder, # whether to show border of labels
      border_type = input$border_type,
      vp_list = label_list$vp_list,
      content_list = label_list$content_list,
      numrow = input$numrow, 
      numcol = input$numcol, 
      page_width = input$page_width, 
      page_height = input$page_height, 
      height_margin = input$height_margin, 
      width_margin = input$width_margin, 
      label_width = input$label_width, 
      label_height = input$label_height, 
      text_align = input$text_align, # left or center
      useMarkdown = T,
      ECols = input$ECol - 1,
      ERows = input$ERow - 1
    )
    status<-"Done!"
    status
  })
  
  # label preview
  
  # rendering of pdf indicator
  output$PDF_status<-renderPrint({cat("\n",PDF_done())})
  # download all the files
  output$download <- downloadHandler(
    filename = paste0(input$filename, ".pdf"),
    content = function(file){
      file.copy(paste0(input$filename, ".pdf"), file)
    }
  )
  
}
shinyApp(ui = ui, server = server)

