#=============================================================================
# Packages
#=============================================================================
library(shiny)
library(fastDummies)
library(rpart)
library(DT)
library(adabag)
library(ada)
library(caret)
library(shinyWidgets)
library(ggplot2)
library(rsconnect)

#=============================================================================
# Data part
#=============================================================================
#open the reference file
df_travel_insurance <- read.csv("TravelInsurancePrediction.csv")
#creation of a dummary variable for the employement type 
df_travel_insurance_clean <- dummy_cols (df_travel_insurance, select_columns = "Employment.Type")
#remove the old columns 
df_travel_insurance_clean <- df_travel_insurance_clean[,-c(3,11)]
#reorder the columns the replace the employement
df_travel_insurance_clean <- df_travel_insurance_clean[,c(1,2,10,3:9)]

#remove the column X which is the index (duplicate)
df_travel_insurance_clean <- df <- df_travel_insurance_clean[,-c(1)]

#rename some columns 
colnames(df_travel_insurance_clean)[2] <- "Private_sector"

#convert the outcome variable as a factor
df_travel_insurance_clean$TravelInsurance <- as.factor(df_travel_insurance_clean$TravelInsurance)

#convert all the yes/no in binary part 1/0
df_travel_insurance_clean$GraduateOrNot <- ifelse(df_travel_insurance_clean$GraduateOrNot=="Yes",1,0)
df_travel_insurance_clean$FrequentFlyer <- ifelse(df_travel_insurance_clean$FrequentFlyer=="Yes",1,0)
df_travel_insurance_clean$EverTravelledAbroad <- ifelse(df_travel_insurance_clean$EverTravelledAbroad=="Yes",1,0)

#=============================================================================
#Data partitioning : 60% trainng and 40% validation 
#=============================================================================

#To reproduce the same simulation
set.seed(1)

# randomly sample 60% of the row IDs for training
train.rows <- sample(rownames(df_travel_insurance_clean), dim(df_travel_insurance_clean)[1]*0.6)

# use setdiff() to find records not already in the training set = 40%
valid.rows <- setdiff(rownames(df_travel_insurance), train.rows)

# create the 2 data frames by collecting all columns from the appropriate rows
df_train <- df_travel_insurance_clean[train.rows, ]
df_valid <- df_travel_insurance_clean[valid.rows, ]

#=============================================================================
#Boosting tree model
#=============================================================================

#We fix the best tuning parameters we found during the analysis with the cross validation 
Boostgrid <- expand.grid(nu=0.001, iter = 250 , maxdepth = 3)

#creation of the tree according to the parameters we found in the project = best parameters 
boost <- train(
  form = TravelInsurance ~ .,
  data = df_train,
  #we set no method here because our analysis of best tuning parameters is already done in the project
  trControl = trainControl(method = "none"),
  method = "ada",
  #we fixe the best parameters found during the cross validation
  tuneGrid = Boostgrid)

#=============================================================================
#Shiny app
#=============================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Prediction of a customer - Boosting tree method"),
  
    # Background design of the app
    setBackgroundColor(
      color = c("#f0f6f9","#c0d9e5"),
      gradient = "linear",
      direction = "bottom"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Age",
                        "Age of the customer:",
                        min = 25,
                        max = 35,
                        value = 30),
            selectInput(inputId = "Private", 
                        label = "Works in private sector ?", 
                        choices = list("No"=0,"Yes"=1)),
            selectInput(inputId = "Graduate", 
                        label = "Customer is graduated ? ", 
                        choices = list("No"=0,"Yes"=1)),
            sliderInput("Income",
                        "Annual income of the customer:",
                        min = 300000,
                        max = 1800000,
                        value = 100000),
            selectInput(inputId = "Familiy", 
                        label = "Number of familiy member", 
                        choices = list(2,3,4,5,6,7,8,9)),
            selectInput(inputId = "Disease", 
                        label = "Has the client any chronic diseases ?", 
                        choices = list("No"=0,"Yes"=1)),
            selectInput(inputId = "Fly", 
                        label = "Is the client a frequent flyer ?", 
                        choices = list("No"=0,"Yes"=1)),
            selectInput(inputId = "Abroad", 
                        label = "	Has the client ever been abroad ?", 
                        choices = list("No"=0,"Yes"=1)),
        ),

        #principal output
        mainPanel(width = 7, height=5,
          #Creation of panel
          tabsetPanel(
            tabPanel("Desired client results",
                     h3('Results of prediction :'),
                     verbatimTextOutput("prediction"),
                     plotOutput("probPlot")
                     ),
            tabPanel("Training data set visualisation",
                     DT::dataTableOutput("mytable"),
                     ),
            tabPanel("Application limitation",
                     br(),
                     "Due to our data set, this application has some limitations. Indeed, the variables that our data set has are sometimes restricted. The most extreme example is age. The data was collected by customers between 25 and 35 years old. In order not to have false predictions, we have voluntarily limited the inputs on the left slider to the values contained in our data set."
                     ),
            tabPanel("Application information",
                     br(),
                     br(),
                     "This application was developed in parallel with the Creating value through data mining project. It is used to predict whether the customer entered is potentially interested in purchasing travel insurance.",
                     br(),
                     br(),
                     "This prediction is done with the help of a machine learning model Boosting tree based on a training data set which is visible on another tab of this application. The ideal tuning parameters were found during our analysis and are as follows: nu=0.001, iter = 250 , maxdepth = 3. These were used in this application to create the boosting tree and their function is described in our project.",
                     br(),
                     br(),
                     "It was designed by : Mohamed Achraf Khemiri and Zimmermann Florian, Master students in Business Analytics at the Unversity of Geneva.",
                    ),
            tabPanel("R code",
                     br(),
                     br(),
                     a("GitHub link to the code of this app", href ="https://github.com/Florian-Zimmermann/CVTDM_Boosting_Shiny_Dashboard"),
                     )
          )
        )
    )
)
  
# Define server logic required 
  server <- function(input, output) {

   
    #=====================Creation of the bar plot with the probabilities=========================
    output$probPlot <-  renderPlot({
      # Enter the data frame according to the inputs 
      client <- data.frame(Age=as.integer(input$Age), Private_sector=as.integer(input$Private), 
                      GraduateOrNot=as.numeric(input$Graduate),AnnualIncome=as.integer(input$Income),
                      FamilyMembers=as.integer(input$Familiy), ChronicDiseases=as.integer(input$Disease),
                      FrequentFlyer= as.numeric(input$Fly), EverTravelledAbroad=as.numeric(input$Abroad))
      #Do the prediction
      prob <- predict(boost, client, type = "prob")
      #Change the format (before a list a now numeric)
      prob_num <- as.numeric(prob)
      #Creation of a data frame for ggplot
      Name <- c("Refuse the insurance","Accept the insurance")
      df_proba <- data.frame(Name, prob_num)
      
      #Plot with GGplot
      ggplot(data=df_proba, aes(x=Name, y=prob_num)) +
        geom_bar(stat="identity", fill="#85C1E9")+
        geom_text(aes(label=round(prob_num, digits = 4)), vjust=1.5, color="black", size=5)+
        theme_minimal() + xlab("") + theme(axis.text.x = element_text(angle = 90),text = element_text(size = 15)) + ylab("Probability")
    })
    
    #=====================Display the data frame =========================
    output$mytable = DT::renderDataTable({df_train})
    
    
    #=====================Answer part=========================
    output$prediction <- renderPrint({
      # Enter the data frame according to the inputs 
      entry <- data.frame(Age=as.integer(input$Age), Private_sector=as.integer(input$Private), 
                           GraduateOrNot=as.numeric(input$Graduate),AnnualIncome=as.integer(input$Income),
                           FamilyMembers=as.integer(input$Familiy), ChronicDiseases=as.integer(input$Disease),
                           FrequentFlyer= as.numeric(input$Fly), EverTravelledAbroad=as.numeric(input$Abroad))
      #Do the prediction
      pr <- predict(boost, entry, type = "raw")
      #Create the output according to the prediction
      result <- ifelse(pr==0, " The predicted client will refuse the insurance with the following probabilities : ", " The predicted client will accept the insurance with the following probabilities : ")
      #display the output
      result
    })
    
  #end of the serveur 
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  
  
  
