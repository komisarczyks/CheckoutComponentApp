library(shiny)
library(shinyjs)
library(CheckoutComponent)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),

  fluidRow(column(6, verticalLayout(h3("Available products"),
                                    p("A list of all available products with prices for a single item and special prices for buying multiple items."),
                                    tableOutput(outputId = "products"),
                                    flowLayout(fileInput(inputId="fileProducts", label="Import products from file."),
                                               downloadButton(outputId = "exportProducts", label = "Export Products")),
                                    htmlOutput(outputId = "warningFileProducts"))),
           column(6, verticalLayout(h3("Bundle discounts"),
                                    p("A list of discounts available when buying a pair of two different products."),
                                    tableOutput(outputId = "discounts"),
                                    flowLayout(fileInput(inputId="fileDiscounts", label="Import discounts from file."),
                                               downloadButton(outputId = "exportDiscounts", label = "Export Discounts")),
                                    htmlOutput(outputId = "warningFileDiscounts")))),
  hr(),
  fluidRow(column(6, verticalLayout(h4("Select amounts of products to purchase."),
                                    uiOutput(outputId = "purchasedProducts"))),
           column(6, verticalLayout(actionButton(inputId = "calculateButton", label="Calculate", width=150, class="btn btn-primary"),
                                    textOutput(outputId = "warningCalc"),
                                    tableOutput(outputId = "receipt"), 
                                    textOutput(outputId = "receiptSum")))),
  tags$head(tags$style("#receiptSum{font-size: 18px;
                                    font-weight: bold;}
                        #warningFileProducts, #warningFileDiscounts, #warningCalc{color: maroon;}
                        #exportProducts, #exportDiscounts{margin-top: 25px;}
                        #calculateButton{margin-top: 64px;
                                         margin-left: 50px;}
                        #receiptSum{margin-left: 110px;}"))
)

server <- function(input, output, session) {
  
  # Verify if the format of an input file with products is correct. If not - display a warning and use default file.
  productsFile <- reactive({
    inFile <- input$fileProducts
    if(is.null(inFile))
      return("products.csv")
    else
    {
      valid <- CheckoutComponent::validateProducts(file=inFile$datapath)
      if(valid[[1]])
      {
        output$warningFileProducts <- renderText("")
        return(inFile$datapath)
      }
      output$warningFileProducts <- renderUI(HTML(gsub(pattern = ".", replacement = ".<br/ >", x = valid[[2]], fixed = T)))
      return("products.csv")
    }
  })
  
  # Read products from file
  products <- reactive({
    prod <- CheckoutComponent::readProducts(file=productsFile())
    prod <- prod[, c("Item", "Price", "SpecialPrice")]
    return(prod)
  })
  
  # Verify if the format of an input file with discounts is correct. If not - display a warning and use default file.
  discountsFile <- reactive({
    inFile <- input$fileDiscounts
    if(is.null(inFile))
      return("discounts.csv")
    else
    {
      valid <- CheckoutComponent::validateDiscounts(products = products(), file=inFile$datapath)
      if(valid[[1]])
      {
        output$warningFileDiscounts <- renderText("")
        return(inFile$datapath)
      }
      output$warningFileDiscounts <- renderUI(HTML(gsub(pattern = ".", replacement = ".<br/ >", x = valid[[2]], fixed = T)))
      return("discounts.csv")
    }
      return(inFile$datapath)
  })
  
  # Read discounts from file
  discounts <- reactive({
    disc <- CheckoutComponent::readDiscounts(file=discountsFile(), products=products())
    return(disc)
  })
  
  # Update products/discounts table
  output$products <- renderTable(products())
  output$discounts <- renderTable(discounts())
  
  output$exportProducts <- downloadHandler(
    filename <-  "products.csv",
    content <- function(file) {
      file.copy(productsFile(), file)
    },
    contentType = "text/csv"
  )
  
  output$exportDiscounts <- downloadHandler(
    filename <-  "discounts.csv",
    content <- function(file) {
      file.copy(discountsFile(), file)
    },
    contentType = "text/csv"
  )
  
  # Dynamically generate input fields. The amount of fields is equal to the number of rows in products table
  output$purchasedProducts <- renderUI({
    fluidRow(
      lapply(1:nrow(products()), function(i) {
        column(3, 
          numericInput(inputId = paste0("item", i), value = 1, label=products()[i, "Item"], width = 80, min=0, step=1)
        )
      })
    )
  })

  # Calculate and display the receipt with purchased products and all discounts.
  receipt <- eventReactive(input$calculateButton, {
    purchase <- NULL
    for(i in 1:nrow(products()))
      purchase <- rbind(purchase, data.frame(Item=products()[i, "Item"], Count=input[[paste0("item", i)]]))
    write.table(purchase, file="purchase.csv", sep=",", row.names = F)
    return(CheckoutComponent::mainCheckoutComponent(productsFile=productsFile(), purchaseFile = "purchase.csv", discountsFile=discountsFile()))
  })
  output$receipt <- renderTable({receipt()})
  output$receiptSum <- renderText({paste("Total cost:", getTotalCost(costTab=receipt(), decimal=TRUE))})
  
  # Verify if all input fields have positive integer values. If not - disable Calculate button and display a warning.
  observe({
    disable <- FALSE
    for(i in 1:nrow(products()))
    {
      if(!is.null(input[[paste0("item", i)]]) && (!is.integer(input[[paste0("item", i)]]) || (input[[paste0("item", i)]]) < 0))
      {
        disable <- TRUE
        break
      }
    }
    if(disable)
    {
      shinyjs::disable("calculateButton")
      output$warningCalc <- renderText("Please use positive integers as input values.")
    }
    else
    {
      shinyjs::enable("calculateButton")
      output$warningCalc <- renderText("")
    }
  })
}

shinyApp(ui = ui, server = server)