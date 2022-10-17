# Make barcodes and print labels
library(qrcode)
load(file='barcodes.rda')
custom_create_PDF <- function(
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
    ){
  if (length(Labels) == 0) stop("Labels do not exist. Please pass in Labels")
  # what to do depending on class of Label input
  if(class(Labels)[1] %in% c("character", "integer", "numeric", "factor")){
    # treat as vector
    Labels <- Labels
  } else if (class(Labels)[1] == "data.frame") {
    if (any(tolower(names(Labels)) == "label")){
      Labels <- Labels[, "label"]
    } else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  } else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (all(vapply(c(
    numcol, numrow, 
    Fsz, ERows, ECols, 
    trunc, page_width, page_height, 
    height_margin, width_margin, 
    x_space, y_space), is.numeric, logical(1L))) != TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  
  if (length(alt_text) > 0) {
    if(length(alt_text) != length(Labels)) {
      stop("Length of alt-text and Labels not equal.")
    }
    alt_text <- as.factor(alt_text)
  }
  
  # clean up any open graphical devices if function fails
  on.exit(grDevices::graphics.off())
  
  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  
  if(!is.numeric(label_width)){label_width <- width_margin/numcol}
  if(!is.numeric(label_height)){label_height <- height_margin/numrow}
  
  if((type == "linear" | type == "linear2") & label_width / labelLength < 0.03)
    warning("Linear barcodes created will have bar width smaller than 0.03 inches. \n  Increase label width to make them readable by all scanners.")
  
  column_space <- (width_margin - label_width * numcol)/(numcol - 1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 1)
  
  # Viewport Setup
  ## grid for page, the layout is set up so last row and column do not include the spacers for the other columns
  barcode_layout <- grid::grid.layout(numrow, 
        numcol, 
        widths = grid::unit(c(rep(label_width + column_space, numcol-1), label_width), "in"),
        heights = grid::unit(c(rep(label_height + row_space, numrow-1), label_height), "in")
        )
  
  ## line numbers by counting "\n"
  tt = Labels[1]
  if (replace_label) tt = alt_text[1]
  # cat("label text example is", as.character(tt), "\n")
  nline_text = nchar(gsub("[^\n]", "", tt)) + 1 # number of lines
  cat("No. of text lines is", nline_text, ";\n")
  
  ## change viewport and barcode generator depending on qr or 1d barcodes
  if(type == "linear" | type == "linear2"){
    code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), 
            y=grid::unit(barcode_height, "npc"), 
            width = grid::unit(0.9 *label_width, "in"), 
            height = grid::unit(barcode_height * label_height * 0.8, "in"), # *0.9 to leave spaces at bottom
            just=c("left", "top"))
    
    # text_height <- ifelse(Fsz / 72 > label_height * 0.5, label_height * 0.5, Fsz/72)
    text_height = label_height * (1 - barcode_height)
    
    label_vp <- grid::viewport(x=grid::unit(0.05, "npc"), 
             y = grid::unit(0.95, "npc"),
             width = grid::unit(0.9 *label_width, "in"),
             height = grid::unit(text_height * 0.9, "in"), 
             just = c("left", "top"))
    # if barcode needs to be at top
    if (barcode_at_top){
      code_vp <- grid::viewport(
        x=grid::unit(0.05, "npc"), 
        y=grid::unit(0.95, "npc"), 
        width = grid::unit(0.9 *label_width, "in"), 
        height = grid::unit(barcode_height * label_height * 0.8, "in"), # *0.9 to leave spaces
        just=c("left", "top"))
      
      label_vp <- grid::viewport(
       x=grid::unit(0.05, "npc"), 
       y = grid::unit(1-barcode_height, "npc"),
       width = grid::unit(0.9 *label_width, "in"),
       height = grid::unit(text_height * 0.9, "in"), 
       just = c("left", "top"))
    }
    # Fsz <- ifelse(Fsz / 72 > label_height * 0.5, label_height * 72 * 0.5 , Fsz)
    Fsz = ifelse(Fsz * nline_text > text_height * 72, text_height * 72 / nline_text, Fsz)
    cat("\nFont size used is", Fsz, ";\n")
    
    # code function to use
    code_fun = ifelse(type == "linear", code_128_make, code_128_make2)

    label_plots <- sapply(as.character(Labels), code_fun, USE.NAMES = TRUE, simplify = FALSE)
  } else if (type =="matrix"){
    ## vp for the qrcode within the grid layout
    code_vp <- grid::viewport(
      x=grid::unit(0.02, "npc"), 
      y=grid::unit(0.9, "npc"), 
      width = grid::unit(0.8 *label_height, "in"), 
      height = grid::unit(0.8 * label_height, "in"), 
      just=c("left", "top"))
    ## vp for the text label within the grid layout
    label_vp <- grid::viewport(
      x=grid::unit(0.95 * label_height, "in"),
      y=grid::unit(y_space, "npc"), 
      width = grid::unit(0.7 *label_width, "in"), 
      height = grid::unit(0.8 * label_height, "in"), 
      just=c("left", "center"))
    # adjust font size
    Fsz = ifelse(Fsz * nline_text > label_height *0.8 * 72, text_height *0.8 * 72 / nline_text, Fsz)
    cat("Font size used is", Fsz, ";\n")
    # generate qr, most time intensive part
        label_plots <- sapply(as.character(Labels), qrcode_make, ErrCorr = ErrCorr, USE.NAMES = TRUE, simplify = FALSE)
  } else {stop("Barcode type must be linear, linear2 or matrix")}
  
  # since main text label is taken from label_plots, set names of vector
  # then null-out alt text
  if(replace_label){
    names(label_plots) <- alt_text
    alt_text <- NULL
  }
  # generate label positions
  
  if(Across){
    # across = TRUE
    positions <- expand.grid(x = 1:numcol, y = 1:numrow)
  } else {
    # across = FALSE
    positions <- expand.grid(y = 1:numrow, x = 1:numcol)
  }

  # make df of position for each label
  # this extra 5 is so that even if starting position is last cell, there are enough positions generated, hopefully
  duplication <- ceiling(length(Labels) / nrow(positions)) + 5
  
  label_positions <- do.call("rbind", replicate(duplication, positions, simplify = FALSE))
  
  # condition here for col/row skipping
  starting_pos_index <- min(which(label_positions$x == ECols + 1  & label_positions$y == ERows + 1))
  if(ECols > numcol | ERows > numrow){
      warning("Number of rows/columns to skip greater than number of rows/columns on page. Labels will start in top left corner.") 
      starting_pos_index <- 1
  }
  label_positions <- label_positions[seq(starting_pos_index, starting_pos_index + length(Labels)),]
  
  # File Creation

  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, 
                 width = page_width, 
                 height = page_height, 
                 onefile = TRUE, 
                 family = fontfamily) # Standard North American 8.5 x 11
  # fontfamily = c("Palatino", "Courier", "Times", "Helvetica")
  
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  
  for (i in seq(1,length(label_plots))){
    
    # Split label to count characters 
    Xsplt <- names(label_plots[i])
    Xalt <- paste(alt_text[i])
    lab_pos <- label_positions[i,]
    
    if(all(i != 1 & lab_pos == c(1, 1))){
      grid::grid.newpage()
      
      grid::pushViewport(
        grid::viewport(width = grid::unit(page_width, "in"), 
                       height = grid::unit(page_height, "in"))
      )
      # barcode_layout=grid.layout(numrow, numcol, widths = widths, heights = heights)
      grid::pushViewport(bc_vp)
    }
    
    if(trunc == TRUE){
      if(nchar(Xsplt) > 15){
        Xsplt <- paste0(substring(Xsplt, seq(1, nchar(Xsplt), 15), seq(15, nchar(Xsplt)+15-1, 15)), collapse = "\n")
      }
    }
    
    # Add alt_text
    if (length(alt_text) > 0) {
      Xsplt <- paste0(Xsplt,Xalt)
    }

    grid::pushViewport(grid::viewport(layout.pos.row=lab_pos$y, layout.pos.col=lab_pos$x))
    if (showborder) grid::grid.rect() # Junli: need to comment out
    grid::pushViewport(code_vp)
    grid::grid.draw(label_plots[[i]])
    grid::popViewport()
    grid::pushViewport(label_vp)
    
    if(type =="linear" | type == "linear2"){
      grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    }
    
    grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, lineheight = 0.8))
    
    grid::popViewport(2)
    
  }

} #end custom_create_PDF()

#' @rdname custom_create_PDF
#' @export

qrcode_make<-function(Labels, ErrCorr){
  # Create text label
  Xtxt<-gsub("_", "-", Labels)
  if(nchar(Xtxt) <= 1){
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  # Create qrcode
  Xpng <- grid::rasterGrob(
    abs(qrcode::qr_code(paste0(Xtxt), 
                           ecl = ErrCorr) - 1), 
    interpolate = FALSE)
  return(Xpng)
}

#' @rdname custom_create_PDF
#' @export
code_128_make <- function(Labels){
  ## labels is a character string
  ## read in dict 
  Barcodes <- barcodes128
  ## double check Labels
  Labels <- as.character(Labels)
  Labels <- iconv(Labels, from = "utf-8", to = "ascii", sub = "-")
  start_code <- 209
  lab_chars <- unlist(strsplit(Labels, split = ""))
  lab_values <- sapply(lab_chars, function(x) utf8ToInt(x))
  # ascii to code 128 is just a difference of 32, this line keeps clarity
  code_values <- lab_values - 32
  # 104 is the start value for start code b, hardcoded right now
  check_sum <- 104 + sum(code_values * seq(1,length(code_values)))
  check_character <- check_sum %% 103
  Binary_code <- sapply(lab_values, 
                        function(x, Barcodes) Barcodes$Barcode[x == Barcodes$ASCII], 
                        Barcodes = Barcodes)
  ## create quiet zone
  quiet_zone <- paste(c(1:(10)*0),collapse="")
  ## paste together in order: quiet zone, start code binary, binary label, checksum character
  ## stop code, and quiet zone. Barcode for checksum is extracted based on position in Barcodes.
  binary_label <- paste(quiet_zone, 
                        Barcodes$Barcode[Barcodes$ASCII == start_code],
                        paste(Binary_code, collapse=""),
                        Barcodes$Barcode[check_character + 1],
                        "1100011101011",
                        quiet_zone,
                        collapse = "", sep ="")
  ## split binary apart for 
  bar_values <- as.numeric(unlist(strsplit(binary_label, split = "")))
  barcode_bars <- grid::rasterGrob(t(!as.matrix(bar_values)), width = 1, height = 1, interpolate = FALSE)
  return(barcode_bars)
}

#' @rdname custom_create_PDF
#' @export
code_128_make2 <- function(Labels) {
  ## labels is a character string
  ## read in dict
  Barcodes <- barcodes128
  ## double check Labels
  Labels <- enc2utf8(as.character(Labels))
  Labels <- iconv(Labels,
                  from = "utf-8",
                  to = "latin1",
                  sub = "-")
  # start_code <- 209
  lab_chars <- enc2utf8(unlist(strsplit(Labels, split = "")))

  is_number <- lab_chars %in% seq(0, 9)
  len1 <- length(lab_chars)
  tail <- seq(len1, 1)

  count_number <- 0
  count_not_number <- 0

  block_number <- rep(0L, len1)
  block_not_number <- rep(0L, len1)


  for (i in seq(len1, 1)) {
    if (is_number[i]) {
      count_number <- count_number + 1
      count_not_number <- 0L
    } else {
      count_number <- 0L
      count_not_number <- count_not_number + 1
    }
    block_number[i] <- count_number
    block_not_number[i] <- count_not_number
  }
  # print(rbind(lab_chars, is_number, block_number, block_not_number, tail))

  lab_values2 <- c()
  code_values2 <- c()
  last_code_B <- NULL

  i <- 1
  repeat {
    if (block_number[i] == 2L & len1 == 2L |
        block_number[i] > 3 & i == 1L |
        block_number[i] > 3 & tail[i] == block_number[i] |
        block_number[i] > 5) {

      res <- block_number[i] %% 2L
      pairs <- block_number[i] %/% 2L
      if (i==1L) {
        if(res == 1L) {
          # Start code 128B
          lab_values2 <- c(lab_values2, 209)
          code_values2 <- c(code_values2, 104)
          last_code_B <- TRUE
        } else {
          # Start code 128C
          lab_values2 <- c(lab_values2, 210)
          code_values2 <- c(code_values2, 105)
          last_code_B <- FALSE
        }
      }

      if (res == 1L) {

        utf_int1 <- utf8ToInt(lab_chars[[i]])

        if (31 < utf_int1 & utf_int1 < 160){

          lab_values2 <- c(lab_values2, utf_int1)
          code_values2 <- c(code_values2, utf_int1 - 32)

        } else {

          if (utf_int1 < 32) {
            # Shift A
            lab_values2 <- c(lab_values2, 203)
            code_values2 <- c(code_values2, 98)
            lab_values2 <- c(lab_values2, utf_int1 + 64 + 32)
            code_values2 <- c(code_values2, utf_int1 + 64)

          } else {
            # FNC4
            lab_values2 <- c(lab_values2, 205)
            code_values2 <- c(code_values2, 100)
            lab_values2 <- c(lab_values2, utf_int1 - 128)
            code_values2 <- c(code_values2, utf_int1 -128 - 32)
          }
        }

      }
      if (i > 1L | res == 1L) {
        # Code 128C
        lab_values2 <- c(lab_values2, 204)
        code_values2 <- c(code_values2, 99)
        last_code_B <- FALSE
      }

      for (j in seq(1, pairs)){
        pair1 <- as.integer(paste0(lab_chars[i+2*j-2+res],lab_chars[i+2*j-1+res]))
        lab_values2 <- c(lab_values2, pair1 + ifelse(pair1 < 95, 32, 105))
        code_values2 <- c(code_values2, pair1)

      }

    } else {
      if (i==1L) {
        # Start code 128B
        lab_values2 <- c(lab_values2, 209)
        code_values2 <- c(code_values2, 104)
        last_code_B <- TRUE
      } else {
        if (!last_code_B) {
          # Code 128B
          lab_values2 <- c(lab_values2, 205)
          code_values2 <- c(code_values2, 100)
          last_code_B <- TRUE
        }
      }
      for (j in seq(1, max(block_number[i],block_not_number[i]))){

        utf_int1 <- utf8ToInt(lab_chars[[i+j-1]])

        if (31 < utf_int1 & utf_int1 < 160){

          lab_values2 <- c(lab_values2, utf_int1)
          code_values2 <- c(code_values2, utf_int1 - 32)

        } else {

          if (utf_int1 < 32) {
            # Shift A
            lab_values2 <- c(lab_values2, 203)
            code_values2 <- c(code_values2, 98)
            lab_values2 <- c(lab_values2, utf_int1 + 64 + 32)
            code_values2 <- c(code_values2, utf_int1 + 64)

          } else {
            # FNC4
            lab_values2 <- c(lab_values2, 205)
            code_values2 <- c(code_values2, 100)
            lab_values2 <- c(lab_values2, utf_int1 - 128)
            code_values2 <- c(code_values2, utf_int1 -128 - 32)
          }
        }
      }
    }
    # print(i)
    i <- i + max(block_number[i],block_not_number[i])
    if (i > len1) {
      break
    }
  }

  # print(rbind(lab_values2, code_values2))


  # lab_values <- sapply(lab_chars, function(x) utf8ToInt(x))
  # ascii to code 128 is just a difference of 32, this line keeps clarity
  # code_values <- lab_values - 32
  # 104 is the start value for start code b, hardcoded right now
  # check_sum <- 104 + sum(code_values * seq(1, length(code_values)))
  check_sum <- sum(code_values2 * c(1,seq(1, length(code_values2)-1)))

  check_character <- check_sum %% 103
  Binary_code <- sapply(lab_values2,
                        function(x, Barcodes)
                          Barcodes$Barcode[x == Barcodes$ASCII],
                        Barcodes = Barcodes)
  ## create quiet zone
  quiet_zone <- paste(c(1:(10) * 0), collapse = "")
  ## paste together in order: quiet zone, start code binary, binary label, checksum character
  ## stop code, and quiet zone. Barcode for checksum is extracted based on position in Barcodes.
  binary_label <- paste(
    quiet_zone,
    #Barcodes$Barcode[Barcodes$ASCII == start_code],
    paste(Binary_code, collapse = ""),
    Barcodes$Barcode[check_character + 1],
    "1100011101011",
    quiet_zone,
    collapse = "",
    sep = ""
  )
  ## split binary apart for
  bar_values <-
    as.numeric(unlist(strsplit(binary_label, split = "")))
  barcode_bars <-
    grid::rasterGrob(
      t(!as.matrix(bar_values)),
      width = 1,
      height = 1,
      interpolate = FALSE
    )
  return(barcode_bars)
}


