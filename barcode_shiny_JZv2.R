# 2023-08-15: draggable viewport using interact.js
library(shiny)
# devtools::install_github("pinbo/barcodeLabel")
library(barcodeLabel)
# function to plot barcode to an image
plotbarcode = function(text, barcode_type=c("linear", "qr", "dm")){
  if (barcode_type=="linear") code = code_128_make(text)
  else if (barcode_type=="qr")  code = qrcode_make(text, 3)
  else if (barcode_type=="dm") code = dmcode_make(text)
  else code = grid::textGrob(text)
  grid::grid.draw(code)
}
# UI
ui <- fluidPage(
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
        column(width = 6,
               textInput("simpleText", "Or input manually repeated labels", value = "", placeholder = "such as 'abc\\n123' ('\\n' for line break)")),
        column(width = 3,
               numericInput("simpleTextRepeat", 
                            "Number of labels", 
                            value = 4, min = 1)),
        column(width=2, actionButton("createDataset", "Create Dataset", style="background-color: #82e0e8"), style = "margin-top: 25px;" )
      ),
      # actionButton("createDataset", "Create Dataset"),
      # Select variables to display ----
      # uiOutput("select_column"),
      p(strong("2. (Optional) Create new variables")),
      # uiOutput("add_text"),
      fluidRow(
        column( width = 2, textInput("newvar",  "Name", value = "", placeholder="new variable name")  ),
        column( width = 5, textInput("prefix",  "Text", value = "", placeholder="e.g. Row {Row} (choose vars on the right")  ),
        column( width = 3, selectizeInput( inputId = "variable", label   = "Variable to insert",
                                        choices = NULL,  multiple = TRUE,
                                        options = list(maxItems = 1)   )
        ),
        column(
          width = 2, selectInput("fontface", "Font face", choices = c(plain=1, bold=2, italic=3, boldItalic=4))
        ),
        # column(
        #   width = 3, radioButtons("newline", "Add line break?", choices = c(Yes = "\n", No = ""), selected = "", inline = T)
        # )
      ),
      # actionButton("textBn", "Add/append text"),
      # actionButton("undo_once", "Undo once"),
      # actionButton("reset_text", "Reset text"),
      actionButton("make_new_var", "Add", style="background-color: #82e0e8"),
      # p("New variable preview:"),
      # verbatimTextOutput("preview_new_var"),
      fluidRow(
        column(width = 8, p(strong("3. Design label"))),
        column(width = 4, p(strong("Label Preview")))
      ),
      fluidRow(
        column(width = 8, div(id="drawing-area", class="grid")),
        column(width = 4, div(plotOutput("label_preview", height = "auto", width = "auto")))
      ),
      uiOutput("select_content"),
      p(strong("4. (Optional) Modify PDF from default values")),
      fluidRow(
        column(width = 3,
               textInput("filename", "Output PDF name", value = "LabelsOut")),
        column(width = 4,
               selectInput(
                 inputId = "label_type",
                 label   = "Preset Label Type",
                 c('Avery 5967 (0.5" x 1.75")' = "avery5967", 'Avery 5960 (1.0" x 2.63")' = "avery5960", 'Tough Spots 3/8in' = 'Tough-Spots-3/8in')
               )),
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
                           multiple=FALSE))
      ),
      fluidRow(
        column(width = 3,
               numericInput("width_margin", 
                            "Side margin (in)", 
                            value = 0.3, 
                            min = 0, max = 20, width=NULL, step = 0.05)),
        column(width = 3,
               numericInput("height_margin", 
                            "Top margin (in)", 
                            value = 0.5, min = 0, max = 20, width=NULL, step = 0.05)),
        column(width = 3,
               numericInput("page_width", 
                            "Page Width (in)", 
                            value = 8.5, min = 1, max = 20, width=NULL,
                            step = 0.5)),
        column(width = 3,
               numericInput("page_height", 
                            "Page Height (in)", 
                            value = 11, 
                            min = 1, max = 20, width=NULL, step = 0.5))
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
               numericInput("ERow", 
                            "Label print from Row", 
                            value = 1, min=1, max=100)),
        column(width = 3,
               numericInput("ECol", 
                            "Label print from Column", 
                            value = 1, min=1, max=100)),
        column(
          width = 4,
          radioButtons("border_type",
                       "Rectangle or circle border?",
                       choices = c("rectangle", "circle", "both"),
                       selected = "rectangle", inline = TRUE)  )
      ),
      width = 8
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      p(strong("6. Make PDF")),
      actionButton("make_pdf", "Make PDF"),
      p(strong("7. Download PDF")),
      tags$body("Wait for 'Done' to show up before downloading PDF file"),
      br(),
      downloadButton('download', 'Download PDF'),
      # status of pdf making
      textOutput("PDF_status"),
      h4(strong("Reproducible Code")),
      verbatimTextOutput("rcode"),
      br(),
      plotOutput("barcode_preview", height = "auto", width = "auto"),
      br(),
      h4(strong("Help")),
      tags$body("This is a shiny application for R package "),
      tags$a('"barcodeLabel"', href="https://github.com/pinbo/barcodeLabel"),
      tags$body(". The Shiny R script is here:"),
      tags$a("https://github.com/pinbo/barcode_labels", href="https://github.com/pinbo/barcode_labels"),
      br(),br(),
      tags$body("Basically, you just need to upload a comma or tab delimited text file and select the columns to make labels. For more complex label layout, please check out the "),
      tags$body(" R package "),
      tags$a('"barcodeLabels"', href="https://github.com/pinbo/barcodeLabel"),
      h5(strong("Notes")),
      tags$ol(
        tags$li("You can customize the label sizes by directly changing the parameters."), 
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
  
  # for manual layout
  output$select_content <- renderUI({
    df = mydata()
    cc = ncol(rawdata())
    data_choices = as.list(df[1,])[-c(cc+1, cc+2)]
    cat("data_choices are:\n")
    print(data_choices)
    tagList(
    fluidRow(
      column(width = 3, 
    selectInput("input_type","Input Type", 
                choices = list("Linear (1D)" = "linear",
                               "QR Code" = "qr",
                               "Data Matrix" = "dm",
                               "Text" = "text"),
                selected = "dm", multiple = FALSE)),
    column(width = 3, 
      selectInput(inputId = "input_var", label = "Input variable", 
                choices = data_choices, multiple=FALSE)),
    #column(width = 2, numericInput(inputId = "input_var_size", label = "Font size (pt)",value = 8, min = 1)),
    column(width = 3,
           selectInput(inputId = "input_var_fontfamily", 
                       label = "Font", 
                       choices = c(
                         "sans"="sans",
                         "serif"= "sans-serif",
                         "monospace" = "monospace"),
                       multiple=FALSE)),
    column(width = 3,
           textInput("input_var_color", "Text color", value = "black"))
    ), fluidRow(
      column(width = 3, actionButton("addButton", "Add", onClick="addBox()", style="background-color: #82e0e8")
        ),
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
                     selected = FALSE, inline = TRUE) ),
      column(
        width = 3, selectInput("input_var_fontface", "Font face", choices = c(plain=1, bold=2, italic=3, boldItalic=4)))
    )
    )
  })
  
  # add text
  observe({
    cc = ncol(rawdata())
    updateSelectInput(session, "variable",
                      choices = names(mydata()[-c(cc+1, cc+2)])
                      )
  })
  
  # add more text
  resetLabelTextInput = function(){
    # reset to all input as default
    updateTextInput(session, "prefix",value = "")
    updateSelectizeInput(session,"variable", selected=NULL)
    # updateRadioButtons(session,"newline",selected = "")
    updateSelectInput(session,"fontface",selected = 1)
  }
  
  observeEvent(input$variable,{
    fn = as.integer(input$fontface)
    fonts = c("", "**", "*", "***")
    newtxt = paste0(input$prefix, fonts[fn], "{", input$variable, "}", fonts[fn])
    updateTextInput(session, "prefix",value = newtxt)
  }, ignoreInit=T)
  
  # observeEvent(input$textBn, {
  #   df2 =  mydata()
  #   fn = as.integer(input$fontface)
  #   fonts = c("", "**", "*", "***")
  #   df2$tmp = df2$text2add
  #   if (is.null(input$variable))
  #     df2$text2add <- paste0(df2$text2add, fonts[fn], input$prefix,fonts[fn],input$newline)
  #   else
  #     df2$text2add <- paste0(df2$text2add, fonts[fn], input$prefix, df2[,input$variable],fonts[fn],input$newline)
  #   mydata(df2)
  #   resetLabelTextInput()
  # })
  # observeEvent(input$reset_text, {
  #   df2 =  mydata()
  #   df2$tmp = df2$text2add
  #   df2$text2add <- ""
  #   mydata(df2)
  #   resetLabelTextInput()
  # })
  # observeEvent(input$undo_once, {
  #   df2 =  mydata()
  #   df2$text2add = df2$tmp
  #   mydata(df2)
  #   resetLabelTextInput()
  # })
  observeEvent(input$make_new_var, {
    df2 =  mydata()
    ss = input$prefix
    aa = strsplit(ss, "[{}]")[[1]]
    cc = gsub("\\}", "x0x0\\}", ss)
    dd = strsplit(cc, "[{}]")[[1]]
    nn = grep("x0x0", dd)
    ss2 = ""
    for (i in 1:length(aa)){
      if (i %in% nn) ss2 = paste0(ss2, df2[,aa[i]])
      else ss2 = paste0(ss2, aa[i])
    }
    df2[[input$newvar]]=ss2
    mydata(df2)
    resetLabelTextInput()
    updateTextInput(session, "newvar", value = "")
  })
  ## preset labels
  
  observeEvent(input$label_type, {
    if (input$label_type == "avery5967"){
      updateNumericInput(session, "numrow", value = 20)
      updateNumericInput(session, "numcol", value = 4)
      updateNumericInput(session, "page_width", value = 8.5)
      updateNumericInput(session, "page_height", value = 11)
      updateNumericInput(session, "label_width", value = 1.75)
      updateNumericInput(session, "label_height", value = 0.5)
      updateNumericInput(session, "width_margin", value = 0.3)
      updateNumericInput(session, "height_margin", value = 0.5)
    }
    else if (input$label_type == "avery5960"){
      updateNumericInput(session, "numrow", value = 10)
      updateNumericInput(session, "numcol", value = 3)
      updateNumericInput(session, "page_width", value = 8.5)
      updateNumericInput(session, "page_height", value = 11)
      updateNumericInput(session, "label_width", value = 2.63)
      updateNumericInput(session, "label_height", value = 1.0)
      updateNumericInput(session, "width_margin", value = 0.3)
      updateNumericInput(session, "height_margin", value = 0.5)
    }
    else if (input$label_type == "Tough-Spots-3/8in"){
      updateNumericInput(session, "numrow", value = 16)
      updateNumericInput(session, "numcol", value = 12)
      updateNumericInput(session, "page_width", value = 8.4375)
      updateNumericInput(session, "page_height", value = 11)
      updateNumericInput(session, "label_width", value = 0.375)
      updateNumericInput(session, "label_height", value = 0.375)
      updateNumericInput(session, "width_margin", value = 0.3)
      updateNumericInput(session, "height_margin", value = 0.5)
      updateNumericInput(session, "font_size", value = 10)
      updateRadioButtons(session, "text_align", selected = "center")
      updateRadioButtons(session, "border_type", selected = "circle")
      updateSelectInput(session, "type", selected = "null")
    }
  }, ignoreInit = TRUE)

  # label only for the first row for preview
  tmp_label_list = reactive({
    req(input$addButton)
    pdf(file=NULL) # mystrwidth depends on the device.
    vp_list = list()
    content_list = list()
    nms = names(input$sizeInfo)
    for (x in nms){
      ss = input$contentInfo[[x]]
      ss2 = input$sizeInfo[[x]]
      if (ss$type == "text") {
        tt = text_array_wrap(mydata()[1,ss$var], input$font_size, round(ss2$width*input$label_width,3), round(ss2$height*input$label_height,3), ss$fontfamily, useMarkdown = T)
        content = tt$text
        Fsz = tt$font_size
        cat("Final Font size used is", Fsz, "\n")
        vp = grid::viewport(x = round(ss2$x,3), y = round(1-ss2$y,3), width = round(ss2$width,3), height = round(ss2$height,3), just = c("left", "top"), gp=grid::gpar(fontsize = Fsz, lineheight = 0.8, fontfamily=ss$fontfamily, fontface=as.numeric(ss$fontface), col=ss$fontcolor))
        vp_list[[x]] = vp
        content_list[[x]] = content
      } else {
        vp = grid::viewport(x = round(ss2$x,3), y = round(1-ss2$y,3), width = round(ss2$width,3), height = round(ss2$height,3), just = c("left", "top"))
        if (ss$type == "dm") content = lapply(as.character(mydata()[1,ss$var]), dmcode_make)
        else if(ss$type == "qr") content = lapply(as.character(mydata()[1,ss$var]), qrcode_make)
        else if(ss$type == "linear") content = lapply(as.character(mydata()[1,ss$var]), code_128_make)
        vp_list[[x]] = vp
        content_list[[x]] = content
      }
    }
    dev.off()
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
                   units = "in", res=300#, family = input$fontfamily
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
  # output$preview_new_var<-renderText({
  #   req(mydata())
  #   mydata()[1,"text2add"]
  # })
  
  ## barcode preview
  output$barcode_preview <- renderImage({
    req(mydata())
    input_type = input$input_type
    input_var = input$input_var
    outputfile <- tempfile(fileext=".png")
    grDevices::png(outputfile, width = 2, height = 2, units = "in", res=300)#, family = input$fontfamily)
    plotbarcode(input_var, input_type)
    grDevices::dev.off()
    list(src = outputfile,
         id = "barcodeImg",
         width = 100, 
         height = 100,
         alt = "barcode Preview",
         style="display:none") # do not show
  }, deleteFile = TRUE
  )
  
  # text indicator that pdf finished making
  PDF_done<-eventReactive(input$make_pdf, {
    # 1. create simple element layout on each label
    vp_list = list()
    content_list = list()
    nms = names(input$sizeInfo)
    pdf(file=NULL) # mystrwidth() depends on the device
    for (x in nms){
      ss = input$contentInfo[[x]]
      ss2 = input$sizeInfo[[x]]
      if (ss$type == "text") {
        tt = text_array_wrap(mydata()[,ss$var], input$font_size, round(ss2$width*input$label_width,3), round(ss2$height*input$label_height,3), ss$fontfamily, useMarkdown = T)
        content = tt$text
        Fsz = tt$font_size
        cat("Final Font size used is", Fsz, "\n")
        vp = grid::viewport(x = round(ss2$x,3), y = round(1-ss2$y,3), width = round(ss2$width,3), height = round(ss2$height,3), just = c("left", "top"), gp=grid::gpar(fontsize = Fsz, lineheight = 0.8, fontfamily=ss$fontfamily, fontface=as.numeric(ss$fontface), col=ss$fontcolor))
        vp_list[[x]] = vp
        content_list[[x]] = content
      } else {
        vp = grid::viewport(x = round(ss2$x,3), y = round(1-ss2$y,3), width = round(ss2$width,3), height = round(ss2$height,3), just = c("left", "top"))
        if (ss$type == "dm") content = lapply(as.character(mydata()[,ss$var]), dmcode_make)
        else if(ss$type == "qr") content = lapply(as.character(mydata()[,ss$var]), qrcode_make)
        else if(ss$type == "linear") content = lapply(as.character(mydata()[,ss$var]), code_128_make)
        vp_list[[x]] = vp
        content_list[[x]] = content
      }
    }
    dev.off()
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
  
  # rendering of pdf indicator
  output$PDF_status<-renderPrint({cat("\n",PDF_done())})
  # download all the files
  output$download <- downloadHandler(
    filename = paste0(input$filename, ".pdf"),
    content = function(file){
      file.copy(paste0(input$filename, ".pdf"), file)
    }
  )
  
  # R code for reproducing in R
  # 1. read or create dataset
  csv_code_snippet <- reactive({noquote({
    if(length(input$labels$name)==0) paste0(
      "# create simple repeated data\n",
        'tt = "', input$simpleText, '"',
        '\ndf = data.frame(label=rep(tt,', input$simpleTextRepeat, '))'
      )
    else paste0(
      "df <- read.csv( \'", input$labels$name, 
      "\', header = ", input$header, 
      ", sep = '", gsub('\t','\\t', input$sep, fixed=T),
      "', stringsAsFactors = FALSE)"
    )
    })
    })
  
  create_data_snippet <- reactive({noquote(
    paste0(
      'tt = gsub("\\\\n","\n",', input$simpleText, ")",
      'df = data.frame(label=rep(tt,', input$simpleTextRepeat, '))'
    ))})
  
  # get the view port settings and contents
  vp_snippet <- reactive({
    req(input$addButton)
    rr = paste(
      "\n# create layouts on the label",
      "vp_list = list()",
      "content_list = list()",
      "pdf(file=NULL)", sep="\n"
    )
    nms = names(input$sizeInfo)
    for (x in nms){
      ss = input$contentInfo[[x]]
      ss2 = input$sizeInfo[[x]]
      if (ss$type == "text") {
        rr = paste(
          rr,
          paste0('tt = text_array_wrap(df$', ss$var, ',',  input$font_size, ', ', round(ss2$width*input$label_width,3), ',', round(ss2$height*input$label_height,3), ',"', ss$fontfamily, '", useMarkdown = T)'),
          'content = tt$text',
          'Fsz = tt$font_size',
          paste0('vp = grid::viewport(x = ', round(ss2$x,3), ', y = ', round(1-ss2$y,3), ', width = ', round(ss2$width,3), ', height = ', round(ss2$height,3), ', just = c("left", "top"), gp=grid::gpar(fontsize = Fsz, lineheight = 0.8, fontfamily="', ss$fontfamily, '", fontface=', ss$fontface, ', col="', ss$fontcolor,'"))'),
          paste0('vp_list$', x, ' = vp'),
          paste0('content_list$', x, ' = content'), sep="\n"
        )
      } else {
        rr = paste(
          rr,
          paste0('vp = grid::viewport(x = ', round(ss2$x,3), ', y = ', round(1-ss2$y,3), ', width = ', round(ss2$width,3), ', height = ', round(ss2$height,3), ', just = c("left", "top"))'),
          {if (ss$type == "dm") paste0('content = lapply(as.character(', 'df$', ss$var, '), dmcode_make)')
          else if (ss$type == "qr") paste0('content = lapply(as.character(df$', ss$var, '), qrcode_make)')
          else if(ss$type == "linear") paste0('content = lapply(as.character(df$', ss$var, '), code_128_make)')},
          paste0('vp_list$', x, ' = vp'),
          paste0('content_list$', x, ' = content'), sep="\n"
        )
      }
    }
    rr = paste(
      rr,
      'dev.off()',
      'label_list = list(vp_list=vp_list, content_list=content_list)', sep="\n"
    )
    noquote(rr)
  })
  
  # make pdf code
  PDF_code_snippet<-shiny::reactive({
    noquote(
      paste0(
        "\n# Make pdf with labels\n",
        "make_custom_label(",
             "label_number = ", nrow(mydata()),
             ",\nname = '", input$filename,
             "',\nfontfamily = '", input$fontfamily,
             "',\nshowborder = ", input$showborder,
             ",\nborder_type = '", input$border_type,
             "',\nvp_list = label_list$vp_list",
             ",\ncontent_list = label_list$content_list",
             ",\nnumrow = ", input$numrow, 
             ",\nnumcol = ", input$numcol, 
             ",\npage_width = ", input$page_width, 
             ",\npage_height = ", input$page_height, 
             ",\nheight_margin = ", input$height_margin, 
             ",\nwidth_margin = ", input$width_margin, 
             ",\nlabel_width = ", input$label_width, 
             ",\nlabel_height = ", input$label_height, 
             ",\ntext_align = '", input$text_align,
             "',\nuseMarkdown = T",
             ",\nECols = ", input$ECol - 1,
             ",\nERows = ", input$ERow - 1,
             ")"
      )
    )
  })
  
  # library
  loadlib = '# need R library barcodeLabel, to install:\n# devtools::install_github("pinbo/barcodeLabel")\nlibrary(barcodeLabel)'
  output$rcode <- shiny::renderText({
    paste(loadlib, csv_code_snippet(), vp_snippet(), PDF_code_snippet(), sep = "\n")
  })
}
shinyApp(ui = ui, server = server)

