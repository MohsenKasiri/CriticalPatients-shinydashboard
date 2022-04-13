# Palmer Penguin dashboard, using the
# https://rstudio.github.io/shinydashboard/structure.html

# https://fontawesome.com/icons 
# to get icons available for use

# helpful resources for making ggplots
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

# Known issues and WIPs:
## Debug only one default selection in LDA tab
## LDA page: find way to get errors to not show up as glaringly obvious - i.e. give helpful error message instead
## Reduce duplication in LDA server code 

# Last updated April 1st, 2022

library(palmerpenguins) # penguin data
library(ggplot2) # data visualization
library(magrittr) # pipe operator
library(dplyr) # manipulate data
library(GGally) # ggpairs()
library(plotly) # ggpairs()

# Make dashboard
library(shiny)
library(shinydashboard)

# For LDA
library(tidyverse)
library(caret)
library(MASS)
#install_github("fawda123/ggord") # Install ggord
library(ggord)

data(penguins) #SOURCE: https://github.com/allisonhorst/palmerpenguins
penguins = na.omit(penguins) #Drop 11 penguins with incomplete records.


pendat = na.omit(penguins) #Remove 11 rows with NA for simplicity for LDA
pendat = data.frame(pendat)

# Split the data into training and testing set
set.seed(123)
training.samples = pendat$species %>%
  createDataPartition(p=0.8, list=FALSE) #Randomly select 0.80 of observations
train.data = pendat[training.samples, ]
test.data = pendat[-training.samples, ]

# It is recommended to standardize/normalize continuous predictor before the analysis
# Estimate pre-processing parameters
preproc.param = train.data %>%
  preProcess(method = c("center", "scale")) #Subtract mean and divide by std. dev.
# Transform the data using the estimated parameters
train.transformed = preproc.param %>% predict(train.data)
test.transformed = preproc.param %>% predict(test.data)

###Making drop down menu objects - merely for interests sake, they are not interactive at the moment
#messageData = rbind(c("Support", "The new server is ready"), c("New User", "How do I register?"))
#colnames(messageData) = c("from", "message")

#Create a ui (user interface); commands that create the overall look of the dashboard
ui = dashboardPage( #Create a dashboard page, uses library(shinydashboard)
  #tags$head(
  #  tags$link(rel="stylesheet", type="text/css", href="www/css/custom.css") #rel and type refer to the imported code? i.e. text/css says were importing a CSS file. 
  #),
  skin="red", #Changes colour of the dashboard, use function ?validColors to see available options.
  dashboardHeader(title = "Palmer Penguins"),
  dashboardSidebar( # UI for the Sidebar
      sidebarMenu(id="menu1", #Creates a menu of tab names 
        menuItem("Exploratory Data Analysis", tabName="dashboard", icon=icon("dove")),
        menuItem("Linear Discriminant Analysis", tabName="analysis", icon=icon("chart-line")),
        menuItem("Penguins", tabName="images", icon=icon("camera"))
      ),
      #Set up a panel that only shows when on the "dashboard" tab
      conditionalPanel(condition = "input.menu1=='dashboard'", 
        tags$hr(),
        h4("Controls"),
        radioButtons(
          inputId = "species",
          label = "Penguin Species",
          choices = c("Adelie", "Chinstrap", "Gentoo", "All"),
          selected = c("Adelie")
        ),
        #radioButtons(
        #  inputId = "island",
        #  label = "Island", 
        #  choices = c("Biscoe", "Dream", "Torgersen", "All"),
        #  selected = c("Biscoe")
        #),
        selectInput(
          inputId = "covar1",
          label = "Variable 1",
          choices = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)",
                    "Body mass (g)")
        ),
        selectInput(
          inputId = "covar2",
          label = "Variable 2",
          choices = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)",
                    "Body mass (g)"),
          selected = c("Bill depth (mm)", "Bill depth (mm)") #BUG: not selecting two default options
        )
      ), #End of conditional Panel
      #Conditional panel when within LDA tab
      conditionalPanel(condition = "input.menu1 == 'analysis'",
                       tags$hr(),
                       h4("Controls"),
                       p("Select at least two variables."),
                       checkboxGroupInput(
                         inputId = "scatterVars",
                         label = "LDA",
                         choices = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)",
                                     "Body mass (g)"),
                         #choiceValues = c(3,4,5,6),
                         selected = c("Bill length (mm)")
                       )           
      )
    ),
  dashboardBody( # UI of what appears in the mainPanel of each tab
    tabItems( # One tabItem() for each of the three tabs
      # Exploratory data analysis
      tabItem(tabName="dashboard",
              h2("Exploratory Data Analysis"),
              p("The Palmer penguins data set contains information on 344 penguins observed on three islands in the Palmer Archipelago, Antartica. The species, island, bill length, bill depth, flipper length, body mass, sex and year of observation of each penguin was recorded."),
              tags$br(),
              p("Use this tool and the control panel on the left to explore the data."),
              fluidRow(
                tabBox(width=12,
                       id = "tabset1", #ID lets us use input$tabset1 on the server to find the current tab.
                       #height = 250,
                       tabPanel("By Species", tableOutput("by_species")),
                       tabPanel("By Island", tableOutput("by_island")), #, checkboxInput("sppCheck", label="By species?")
                       tabPanel("By Sex", tableOutput("by_sex")),
                       tabPanel("Overall", tableOutput("crossTab"))
                ),
                tabBox(width=12,
                       id="tabset2",
                       tabPanel("Tab1", tableOutput("univarSum")),
                       tabPanel("Tab2", tableOutput("sexSppSum"))
                       )
                #box(width=12, tableOutput("univarSum")) #width=12 means put onto a new line
              ),
              p("The following histogram and boxplot correspond to covariate 1."),
              fluidRow(
                box(width=5, plotOutput("hist1", height=250)), #Histogram
                box(width=7, plotOutput("boxplot", height=250))
                #box(width=7, plotOutput("scatter1", height=250)) #Scatterplot of two covariates
              ),
              p("The following histogram and boxplot correspond to covariate 2."),
              fluidRow(
                box(width=5, plotOutput("hist2", height=250)), #Histogram
                box(width=7, plotOutput("boxplot2", height=250)),
                box(width=5, p("This is some text")),
                box(width=7, plotOutput("scatter1", height=250)) #Scatterplot of two covariates
              ),
              fluidRow(
                box(width=12, plotOutput("ggpairplot", height=600)) #Matrix of scatterplots
              )
      ),
      # Images of Penguins
      tabItem(tabName="images",
              h2("Meet the Palmer Penguins!"),
              column(width=6, 
                     h3("Gentoo"),
                     img(src = "gentoo.jpg", width=300),
                     p("https://commons.wikimedia.org/wiki/File:Gentoo_penguin.jpg")
                     ),
              column(width=6,
                     h3("Adelie"),
                     img(src = "adelie.jpg", width = 300),
                     p("https://www.flickr.com/photos/rwoan/25706044408"),
                     h3("Chinstrap"),
                     img(src = "chinstrap.jpg", width = 300),
                     p("https://www.flickr.com/photos/cmichel67/39985995")
                     )
      ),
      # Linear Discriminant Analysis
      tabItem(tabName="analysis",
              h2("Linear Discriminant Analysis"),
              p("Use linear discriminant (LD) analysis to identify a linear combination of variables that can be used to identify the species membership of the penguins. Try different combinations of variables to see how to model accuracy and the linear discriminants change."),
              fluidRow(
                box(width=6, 
                    h4("LDs and Model Accuracy"),
                    tags$div(
                      "LD1 = ", textOutput("LD1"), tags$br(),
                      "LD2 = ", textOutput("LD2"), tags$br(),
                      "The model accuracy is: ", span(textOutput("modAcc"), style = "color:red; font-size:150%")
                    )
                  ),
                box(width=6, 
                    h4("Confusion Matrix"),
                    p("The bolded columns are the reference species."),
                    tableOutput("conMat"))
              ),
              fluidRow(
                box(width=12, plotOutput("LDA1", width=600)) 
              )
      )
    )
  )
)

#Server is a function with three parameters: input, output, and session. The fn is called once per session to ensure each run is independent.
server = function(input, output) {
  #Set up necessary data sets and dictionaries for use in outputs
  names = c("Bill length (mm)"="bill_length_mm", "Bill depth (mm)"="bill_depth_mm", "Flipper length (mm)"="flipper_length_mm",
           "Body mass (g)"="body_mass_g", "Sex"="sex", "Species"="species", "Island"="island", "Year"="year")
  dict = c("Bill length (mm)"=3, "Bill depth (mm)"=4, "Flipper length (mm)"=5,
           "Body mass (g)"=6, "Sex"=7)
  by_species = penguins %>% group_by(species) %>% count()
  colnames(by_species) = c("Species", "Count")
  by_island = penguins %>% group_by(island) %>% count()
  colnames(by_island) = c("Island", "Count")
  by_sex = penguins %>% group_by(sex) %>% count()
  colnames(by_sex) = c("Sex", "Count")
  
  #EDA: histogram changes based on species and covariate 1 selection
  output$hist1 = renderPlot({ #The reactive expression will update this value when the widget changes
    df = data.frame(penguins)
    df.subset = reactive({ #subset data based on chosen species
      if(input$species == "All"){a = df} else {a = subset(df, species == input$species)}
      return(data.frame(a))
    }) 
    #hist(df.subset()[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
    #     main=paste(input$species), xlab=paste(input$covar1))
    col = ifelse(input$species == "Adelie", "#F8766D", ifelse(input$species == "Chinstrap", "#00BA38", ifelse(input$species == "Gentoo", "#619CFF", "grey75")))     
    ggplot(data=df.subset(), aes(x=df.subset()[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))])) +
      geom_histogram(bins=8, color="black", fill=col) + theme_classic() +
      labs(title=paste(input$species), x=paste(input$covar1), y="Frequency")
  })
  
  #EDA: histogram changes based on species and covariate 2 selection
  output$hist2 = renderPlot({ #The reactive expression will update this value when the widget changes
    df = data.frame(penguins)
    df.subset = reactive({ #subset data based on chosen species
      if(input$species == "All"){a = df} else {a = subset(df, species == input$species)}
      return(data.frame(a))
    }) 
    #hist(df.subset()[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
    #     main=paste(input$species), xlab=paste(input$covar1))
    col = ifelse(input$species == "Adelie", "#F8766D", ifelse(input$species == "Chinstrap", "#00BA38", ifelse(input$species == "Gentoo", "#619CFF", "grey75")))     
    ggplot(data=df.subset(), aes(x=df.subset()[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))])) +
      geom_histogram(bins=8, color="black", fill=col) + theme_classic() +
      labs(title=paste(input$species), x=paste(input$covar2), y="Frequency")
  })
  
  output$crossTab = renderTable({
    a = ftable(penguins$species, penguins$island, penguins$sex)
    b = as.matrix(a)
    c1 = c("Adelie", "", "", "Chinstrap", "", "", "Gentoo", "", "")
    c2 = c("Biscoe", "Dream", "Torgersen", "Biscoe", "Dream", "Torgersen", "Biscoe", "Dream", "Torgersen")
    tab = cbind(c1, c2, b)
    colnames(tab) = c("Species", "Island", "Female", "Male")
    rownames(tab) = NULL
    tab
  })
  
  output$univarSum = renderTable({
    r1 = summary(penguins$bill_length_mm)[c(1,3,4,6)]
    r2 = summary(penguins$bill_depth_mm)[c(1,3,4,6)]
    r3 = summary(penguins$flipper_length_mm)[c(1,3,4,6)]
    r4 = summary(penguins$body_mass_g)[c(1,3,4,6)]
    tab = rbind(r1, r2, r3, r4)
    colnames(tab) = c("Minimum", "Median", "Mean", "Maximum")
    rownames(tab) = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)", "Body mass (g)")
    tab
  }, include.rownames=T)
  
  output$sexSppSum = renderTable({
    penguins = data.frame(penguins)
    b1 = b2 = numeric()
    for(i in 3:6){
      a = tapply(penguins[,i], penguins[,7], summary)
      b1 = rbind(b1, a$female)
      b2 = rbind(b2, a$male)
    }
    c1 = c2 = c3 = numeric()
    for(i in 3:6){
      a = tapply(penguins[,i], penguins[,1], summary)
      c1 = rbind(c1, a$Adelie)
      c2 = rbind(c2, a$Chinstrap)
      c3 = rbind(c3, a$Gentoo)
    }
    mat = rbind(b1[,4], b2[,4], c1[,4], c2[,4], c3[,4])
    rownames(mat) = c("Female", "Male", "Adelie", "Chinstrap", "Gentoo")
    colnames(mat) = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)", "Body mass (g)")
    mat
  }, include.rownames=T)
  
  #EDA: barplot changes based on covariate 1 selection -- OBSOLETE
  output$plot2 = renderPlot({
    df.subset2 = reactive({ #subset data based on chosen island
      if(input$island == "All"){b = penguins} else {b = subset(penguins, island == input$island)}
      return(b)
    })
    barplot(table(df.subset2()$species), main=paste("Number of Each Species:", input$island, sep=" "))
  })
  
  #EDA: scatter plot of two covariates -- OBSOLETE
  output$plot3 = renderPlot({ 
    df = data.frame(penguins)
    plot(df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))], df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))],
         main=paste(input$covar1, "vs.", input$covar2, sep=" "),
         xlab=paste(input$covar1), ylab=paste(input$covar2))
  })
  
  #EDA: Make scatter plot of two covariates - broken up by species
  output$scatter1 = renderPlot({ 
    df = data.frame(penguins)
    ggplot(data=df, aes(x=df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
                        y=df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))],
                        color=species)) + #, shape=sex
      geom_point(size=2) + 
      labs(title=paste(input$covar1, "vs", input$covar2, sep=" "), x=(paste(input$covar1)), y=(paste(input$covar2))) + 
      geom_smooth(method=lm, se=F, fullrange=T) +
      theme_classic() + theme(text = element_text(size = 12))
  })
  
  #OBSOLETE --- message menu for interest's sake
  output$messageMenu = renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'
    msgs = apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type="messages", .list=msgs)
  })
  
  #EDA: tables of counts, not really based on input so might be able to fix
  output$by_species = renderTable(
    by_species, width="100%"
  )
  output$by_island = renderTable(
    #if(input$sppCheck == TRUE){
    #  by_island = penguins %>% group_by(island, species) %>% count()
    #  colnames(by_island) = c("Island", "Count")
    #}
    by_island, width="100%"
  )
  output$by_sex = renderTable(
    by_sex, width="100%"
  )
  
  #EDA: boxplot of covariate 1 broken down by species and island.
  output$boxplot = renderPlot({
    df = data.frame(penguins)
    ggplot(df, aes(y=df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
                   x=island, fill=species)) + geom_boxplot() + theme_classic() +
      labs(title=paste("Distributions of", input$covar1, "for species by island", sep=" "), x="Island", y=paste(input$covar1)) +
      theme(text = element_text(size = 12))
  })
  
  #EDA: boxplot of covariate 1 broken down by species and island.
  output$boxplot2 = renderPlot({
    df = data.frame(penguins)
    ggplot(df, aes(y=df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))],
                   x=island, fill=species)) + geom_boxplot() + theme_classic() +
      labs(title=paste("Distributions of", input$covar1, "for species by island", sep=" "), x="Island", y=paste(input$covar2)) +
      theme(text = element_text(size = 12))
  })
  
  #EDA: ggpairplot
  output$ggpairplot = renderPlot({
    ggpairs(penguins[,3:6],
            title="Matrix plot", ggplot2::aes(colour=penguins$species),
            columnLabels = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)", "Body mass (g)"),
            upper = list(continuous = wrap("cor", size = 5))) + 
      theme_bw(base_size=15)
  })
  
  ### LDA #################################
  vars = reactive({
    validate( #Test condition and return validation error if test fails. Need tells Shiny what to test and then what to return if fails.
      need(length(input$scatterVars) >= 2, "Please select at least two (2) covariates.")
    )
    input$scatterVars
  })
  
  train.changed = reactive({
    a = cbind(train.transformed[,1], train.transformed[,grep(paste(as.character(names[vars()]), collapse="|"), colnames(penguins))])
    colnames(a)[1] = "species"
    a
  })
  test.changed = reactive({
    a = cbind(test.transformed[,1], test.transformed[,grep(paste(as.character(names[vars()]), collapse="|"), colnames(penguins))])
    colnames(a)[1] = "species"
    a
  })
  centx = reactive({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    centx = numeric()
    for(i in 1:3){
      centx[i] = as.vector(model$scaling[,1])%*%as.vector(model$means[i,])
      #centy[i] = as.vector(model$scaling[,2])%*%as.vector(model$means[i,])
    }
    centx
  })
  centy = reactive({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    centy = numeric()
    for(i in 1:3){
      centy[i] = as.vector(model$scaling[,2])%*%as.vector(model$means[i,])
    }
    centy
  })
  
  output$test = renderText({
    head(train.changed())
  })
  
  ## EDIT TO REDUCE DUPLICATION
  output$LDA1 = renderPlot({
    ###Subset the data
    #Train the model with the training data
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    #Predict using the testing data
    predictions = model %>% predict(test.changed()[,-1])
    modAccuracy = mean(predictions$class == test.changed()$species) # mean of 1 indicates 100% accuracy.
    lda.data = cbind(test.changed(), unlist(predictions$x))
    lda.dat = data.frame(lda.data)
    colnames(lda.data) = c("species", 1:length(input$scatterVars), "LD1", "LD2")
    props = (model$svd^2)/sum(model$svd^2)
    ggplot(lda.data, aes(LD1, LD2)) +
      geom_point(aes(color=species), size=3) +
      theme_classic() + theme(text = element_text(size = 15)) +
      labs(title="Linear discriminant plot", x=paste("First linear discriminant (", signif(props[1]*100, digits=4), ")", sep=""),
           y=paste("Second linear discriminant (", signif(props[2]*100, digits=4), ")", sep="")) +
      geom_vline(xintercept=c(mean(centx()[1:2]), mean(centx()[2:3])), linetype="dashed") +
      geom_hline(yintercept=c(mean(centy()[1:2]), mean(centy()[2:3])), linetype="dashed") +
      labs(color="Class")
  })
  
  #LDA: confusion matrix
  output$conMat = renderTable({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    #Predict using the testing data
    predictions = model %>% predict(test.changed()[,-1])
    #modAccuracy = mean(predictions$class == test.changed()$species)
    not.mat = confusionMatrix(test.changed()[,1], predictions$class, dnn = c("Prediction", "Reference"))
    as.data.frame.matrix(not.mat$table)
  }, include.rownames=T)
  
  #LDA: Model Prediction accuracy 
  output$modAcc = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    #Predict using the testing data
    predictions = model %>% predict(test.changed()[,-1])
    modAccuracy = mean(predictions$class == test.changed()$species)
    signif(modAccuracy, digits=3)
  })
  #LDA: Equation for LD1
  output$LD1 = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    factors = colnames(train.changed())[-1]
    coefs = signif(model$scaling[,1], digits=3)
    len = length(coefs)
    paste(coefs, factors, collapse=" + ", sep="*")
  })
  #LDA: Equation for LD2
  output$LD2 = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1])
    factors = colnames(train.changed())[-1]
    coefs = signif(model$scaling[,2], digits=3)
    len = length(coefs)
    paste(coefs, factors, collapse=" + ", sep="*")
  })
}

shinyApp(ui,server) #Creates a Shiny app object from a ui/server pair


# Extra bits
#box(tags$div( #Creates a section (e.g., "division") of an HTML document. divs provide a useful hook for CSS styling. You can use the helper function div.
#  "Some text followed by a break", 
#  tags$br(),
#  tags$i("Some text following a break")
#))

#Test penguin histogram

#g = ggplot(penguins, aes(x=df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))]))
