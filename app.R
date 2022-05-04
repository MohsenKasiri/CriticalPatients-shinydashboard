# Palmer Penguin dashboard, using RShiny
# Shiny documentation: https://rstudio.github.io/shinydashboard/structure.html
# Data source: https://github.com/allisonhorst/palmerpenguins 

# https://fontawesome.com/icons 
# to get icons available for use

# Known issues and WIPs:
## Reduce duplication in LDA server code.


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


#Create a ui (user interface); commands that create the overall look of the dashboard
ui = dashboardPage( #Create a dashboard page, uses library(shinydashboard)
  skin="red", #Changes colour of the dashboard, use function ?validColors to see available options.
  dashboardHeader(title = "Palmer Penguins"), #Title dashboard
  dashboardSidebar( # UI for the Sidebar
      sidebarMenu(id="menu1", #Creates a menu of tab names 
        menuItem("Exploratory Data Analysis", tabName="dashboard", icon=icon("dove")),
        menuItem("Linear Discriminant Analysis", tabName="analysis", icon=icon("chart-line")),
        menuItem("Penguins", tabName="images", icon=icon("camera"))
      ),
      #Set up a panel that only shows when on the "dashboard" tab
      conditionalPanel(condition = "input.menu1=='dashboard'", 
        tags$hr(), #horizontal rule, i.e. line
        h4("Controls"),
        p("Pick a different penguin species to see changes in the histograms."),
        radioButtons(
          inputId = "species",
          label = "Penguin Species",
          choices = c("Adelie", "Chinstrap", "Gentoo", "Combined"),
          selected = c("Adelie")
        ),
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
                         selected = c("Bill length (mm)")
                       )           
      )
    ),
  dashboardBody( # UI of the mainPanel of each tab
    tabItems( # One tabItem() for each of the three tabs
      #TAB 1: Exploratory data analysis
      tabItem(tabName="dashboard",
              h2("Exploratory Data Analysis"),
              p("The Palmer penguins data set contains information on 344 penguins observed on three islands in the Palmer Archipelago, Antartica. The species, island, bill length, bill depth, flipper length, body mass, sex and year of observation of each penguin was recorded."),
              tags$br(),
              p("Use this tool and the control panel on the left to explore the data."),
              fluidRow(
                tabBox(width=12,
                       id = "tabset1", #ID lets us use input$tabset1 on the server to find the current tab.
                       tabPanel("Counts", "Count of overall penguins by sex, island and species.", tableOutput("crossTab")),
                       tabPanel("Summary", "Summary statistics of each variable.", tableOutput("univarSum")),
                       tabPanel("Group means", "Mean values of each variable broken in sex or species.", tableOutput("sexSppSum"))
                )
              ), #End of first row.
              p("The following histogram and boxplot correspond to variable 1."),
              fluidRow(
                box(width=5, plotOutput("hist1", height=250)), #Histogram
                box(width=7, plotOutput("boxplot", height=250))
                #box(width=7, plotOutput("scatter1", height=250)) #Scatterplot of two covariates
              ),
              p("The following histogram and boxplot correspond to variable 2."),
              fluidRow(
                box(width=5, plotOutput("hist2", height=250)), #Histogram
                box(width=7, plotOutput("boxplot2", height=250)),
                tabBox(width=5, 
                       tabPanel("", p("Select a tab for a description of the scatterplots below and to the right.")),
                       tabPanel("Below", p("The matrix of scatterplots by species below has pairwise scatterplots in the lower left, the Pearson correlations in the upper right, and the univariate distributions by species along the diagonal.")),
                       tabPanel("Right", p("The scatterplot to the right shows the relationship between variable 1 and 2. The regression lines for each species is included, as is the overall trend line (black)."),
                                tags$br(), p("Notice that in some cases the direction of the regression line of the overall data is the opposite of the lines broken down by species. This is an example of Simpson's paradox."))
                ),
                box(width=7, plotOutput("scatter1", height=250)) #Scatterplot of two covariates
              ),
              fluidRow(
                box(width=12, plotOutput("ggpairplot", height=600)) #Matrix of scatterplots
              )
      ),
      #TAB 2: Linear Discriminant Analysis
      tabItem(tabName="analysis",
              h2("Linear Discriminant Analysis"),
              p("Use linear discriminant (LD) analysis to identify a linear combination of variables that can be used to classify the species membership of the penguins. Try different combinations of variables to see how to model accuracy and the linear discriminants change."),
              p("80% of the penguins are randomly allocated to the training set. The remaining penguins make up the test set. There are 3 classes of penguin: Adelie, Chinstrap, and Gentoo. The linear discriminant model is trained using equal prior probabilities (i.e 1/3)."),
              fluidRow(
                box(width=12, 
                    h4("Linear Discriminant Equations"),
                    tags$div(
                      "LD1 = ", textOutput("LD1", inline=T), tags$br(),
                      "LD2 = ", textOutput("LD2", inline=T)#, tags$br(),
                      #"Model accuracy: ", span(textOutput("modAcc", inline=T), style = "color:red; font-size:150%")
                    )
                  )
              ),
                #box(width=6, p("80% of the penguins are randomly allocated to the training set. The remaining penguins make up the test set. There are 3 classes of penguin: Adelie, Chinstrap, and Gentoo. The linear discriminant model is trained using equal prior probabilities (i.e 1/3). The model accuracy is the proportion of correctly classified penguins when predicting the class of the testing set.")),
              p("The model accuracy is the proportion of correctly classified penguins when predicting the classes using a data set. Let's look at the testing set accuracy."),
              fluidRow(
                box(width=8, 
                    h4("Confusion Matrix"),
                    p("The bolded columns are the reference species."),
                    tableOutput("conMat.test")),
                tags$div(tags$br(), h4("Model accuracy"), span(textOutput("modAcc", inline=T), style = "color:red; font-size:150%")),
                box(width=12, plotOutput("LDA1")) 
              )
      ),
      #TAB 3: Images of Penguins
      tabItem(tabName="images",
              h2("Meet the Palmer Penguins!"),
              column(width=6, 
                     h3("Gentoo"),
                     img(src = "gentoo.jpg", width="100%"), #Width of image is in pixels.
                     p("https://commons.wikimedia.org/wiki/File:Gentoo_penguin.jpg")
              ),
              column(width=6,
                     h3("Adelie"),
                     img(src = "adelie.jpg", width = "100%"),
                     p("https://www.flickr.com/photos/rwoan/25706044408"),
                     h3("Chinstrap"),
                     img(src = "chinstrap.jpg", width = "100%"), 
                     p("https://www.flickr.com/photos/cmichel67/39985995")
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
      if(input$species == "Combined"){a = df} else {a = subset(df, species == input$species)}
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
      if(input$species == "Combined"){a = df} else {a = subset(df, species == input$species)}
      return(data.frame(a))
    }) 
    col = ifelse(input$species == "Adelie", "#F8766D", ifelse(input$species == "Chinstrap", "#00BA38", ifelse(input$species == "Gentoo", "#619CFF", "grey75")))     
    ggplot(data=df.subset(), aes(x=df.subset()[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))])) +
      geom_histogram(bins=8, color="black", fill=col) + theme_classic() +
      labs(title=paste(input$species), x=paste(input$covar2), y="Frequency")
  })
  
  output$crossTab = renderTable({
    a = ftable(penguins$species, penguins$island, penguins$sex)
    b = as.matrix(a)
    b = b[-c(4,6,8,9),]
    b = rbind(b, c(sum(b[,1]), sum(b[,2])))
    c1 = c("Adelie", "", "", "Chinstrap", "Gentoo", "")
    c2 = c("Biscoe", "Dream", "Torgersen", "Dream", "Biscoe", "<strong>TOTAL</strong>")
    c3 = c(as.vector(b[,1]+b[,2]))
    tab = cbind(c1, c2, b, c3)
    colnames(tab) = c("Species", "Island", "Female", "Male", "TOTAL")
    rownames(tab) = NULL
    tab
  }, striped=T, sanitize.text.function=function(x){x}) #Change f'n to identity to interpret the tags.
  
  output$univarSum = renderTable({
    summ = sapply(penguins, summary)
    ss = rbind(summ$bill_length_mm, summ$bill_depth_mm, summ$flipper_length_mm, summ$body_mass_g)
    sdvec = apply(penguins, 2, sd, na.rm=T)
    tab = cbind(sdvec[3:6], ss)
    tab = tab[,c(5,1,2,4,7)]
    colnames(tab) = c("Mean", "SD", "Minimum", "Median", "Maximum")
    rownames(tab) = c("Bill length (mm)", "Bill depth (mm)", "Flipper length (mm)", "Body mass (g)")
    tab
  }, include.rownames=T, striped=T)
  
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
  }, include.rownames=T, striped=T)

  
  #EDA: Make scatter plot of two covariates - broken up by species
  output$scatter1 = renderPlot({ 
    df = data.frame(penguins)
    subdf = cbind(df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))], df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))])
    fit = lm(subdf[,2]~subdf[,1])
    ggplot(data=df, aes(x=df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
                        y=df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))],
                        color=species)) + #, shape=sex
      geom_point(size=2) + 
      labs(title=paste(input$covar1, "vs", input$covar2, sep=" "), x=(paste(input$covar1)), y=(paste(input$covar2))) + 
      geom_smooth(method=lm, se=F, fullrange=T) +
      theme_classic() + theme(text = element_text(size = 12)) +
      geom_abline(intercept = fit$coefficients[1], slope=fit$coefficients[2], size=1.5)
  })

  
  #EDA: boxplot of covariate 1 broken down by species and island.
  output$boxplot = renderPlot({
    df = data.frame(penguins)
    ggplot(df, aes(y=df[,which(grepl(as.character(names[input$covar1]), colnames(penguins), fixed=TRUE))],
                   x=island, fill=species)) + geom_boxplot() + theme_classic() +
      labs(title=paste("Distributions of", input$covar1, "for species by island", sep=" "), x="Island", y=paste(input$covar1)) +
      theme(text = element_text(size = 12))
  })
  
  #EDA: boxplot of covariate 2 broken down by species and island.
  output$boxplot2 = renderPlot({
    df = data.frame(penguins)
    ggplot(df, aes(y=df[,which(grepl(as.character(names[input$covar2]), colnames(penguins), fixed=TRUE))],
                   x=island, fill=species)) + geom_boxplot() + theme_classic() +
      labs(title=paste("Distributions of", input$covar2, "for species by island", sep=" "), x="Island", y=paste(input$covar2)) +
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
    model = lda(train.changed()[,-1], grouping=train.changed()[,1], priors=rep(1/3,3))
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
      labs(title="Linear discriminant plot for testing data", x=paste("First linear discriminant (", signif(props[1]*100, digits=4), ")", sep=""),
           y=paste("Second linear discriminant (", signif(props[2]*100, digits=4), ")", sep="")) +
      geom_vline(xintercept=c(mean(centx()[1:2]), mean(centx()[2:3])), linetype="dashed") +
      geom_hline(yintercept=c(mean(centy()[1:2]), mean(centy()[2:3])), linetype="dashed") +
      labs(color="Class")
  })
  
  #LDA: confusion matrices for training and testing data sets
  output$conMat.test = renderTable({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1], priors=rep(1/3,3))
    #Predict using the testing data
    predictions = model %>% predict(test.changed()[,-1])
    #modAccuracy = mean(predictions$class == test.changed()$species)
    not.mat = confusionMatrix(test.changed()[,1], predictions$class, dnn = c("Prediction", "Reference"))
    as.data.frame.matrix(not.mat$table)
  }, include.rownames=T, width="100px")
  
  #LDA: Model Prediction accuracy 
  output$modAcc = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1], priors=rep(1/3,3))
    #Predict using the testing data
    predictions = model %>% predict(test.changed()[,-1])
    modAccuracy = mean(predictions$class == test.changed()$species)
    signif(modAccuracy, digits=3)
  })
  #LDA: Equation for LD1
  output$LD1 = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1], priors=rep(1/3,3))
    factors = colnames(train.changed())[-1]
    coefs = signif(model$scaling[,1], digits=3)
    len = length(coefs)
    paste(coefs, factors, collapse=" + ", sep="*")
  })
  #LDA: Equation for LD2
  output$LD2 = renderText({
    model = lda(train.changed()[,-1], grouping=train.changed()[,1], priors=rep(1/3,3))
    factors = colnames(train.changed())[-1]
    coefs = signif(model$scaling[,2], digits=3)
    len = length(coefs)
    paste(coefs, factors, collapse=" + ", sep="*")
  })
}

# Final call to tie the UI and server together. 
shinyApp(ui,server) # Creates a Shiny app object from a ui/server pair
