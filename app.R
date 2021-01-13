
# FIT5147: Data Exploration and Visulization
# Assignment 5



# Name: Rahul Joseph  
# StudentID: 30347734

####################################################################################
#  LIBRARIES NEEDED FOR THIS FILE TO RUN                                           #
####################################################################################

# Packages that is required to run the Shiny Application

# install.packages("dplyr")
# install.packages("rpart")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinythemes")
# install.packages("plotly")
# install.packages("highcharter")

# Libraries that is used for this Shiny Application

library(dplyr)
library(rpart)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(highcharter)

################################################################################################################################
# PRE-PROCESSING STEPS THAT IS PERFORMED INORDER TO BUILD THIS VISUALIZATION                                                   #
################################################################################################################################


### Load the Dataset
black_data <-read.csv("train.csv")



### Number of user's
user_id <- black_data$User_ID
df_uniq_user_id <- unique(user_id)
length_user_id<-length(df_uniq_user_id)

### NUmber of Product's
product_id <- black_data$Product_ID
df_uniq_product_id <- unique(product_id)
length_product_id<-length(df_uniq_product_id)

### Total purchase amount
total_purchase <- floor(sum(black_data$Purchase/1000000))

### Transforming the Dataset

black_data$User_ID <- as.factor(black_data$User_ID)
black_data$Product_ID <- as.factor(black_data$Product_ID)
black_data$Gender <- as.factor(ifelse(black_data$Gender == 'M', 'Male', 'Female'))
black_data$Age <- as.factor(black_data$Age)
black_data$Occupation <- as.factor(black_data$Occupation)
black_data$City_Category <- as.factor(black_data$City_Category)
black_data$Stay_In_Current_City_Years <- as.factor(black_data$Stay_In_Current_City_Years)
black_data$Marital_Status <- as.factor(ifelse(black_data$Marital_Status == 1, 'Married', 'Single'))
black_data$Product_Category_1 <- as.integer(black_data$Product_Category_1)
black_data$Product_Category_2 <- as.integer(black_data$Product_Category_2)
black_data$Product_Category_3 <- as.integer(black_data$Product_Category_3)
black_data$Purchase <- as.numeric(black_data$Purchase)



### Filling the NA values using the rpart function which predict the category using the records with NA values


# target variable = Product_Category_2
# predictors = User_ID + Product_ID + Age + Gender
# data = everything expect the rows with NA values in product_category_2

fit <-  rpart(Product_Category_2 ~ User_ID + Product_ID + Age + Gender,data = black_data[!is.na(black_data$Product_Category_2),],method = "anova")

black_data$Product_Category_2[is.na(black_data$Product_Category_2)] <- predict(fit, black_data[is.na(black_data$Product_Category_2),])


# target variable = Product_Category_3
# predictors = User_ID + Product_ID + Age + Gender
# data = everything expect the rows with NA values in product_category_3

fit1 <- rpart(Product_Category_3 ~ User_ID + Product_ID + Age + Gender,data = black_data[!is.na(black_data$Product_Category_3),],method = "anova")

black_data$Product_Category_3[is.na(black_data$Product_Category_3)] <- predict(fit1, black_data[is.na(black_data$Product_Category_3),])


### Check for missing values

black_data %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

### transforming the Product_Category_(1,2,3) to integer datatype

black_data$Product_Category_1 <- as.integer(black_data$Product_Category_1)
black_data$Product_Category_2 <- as.integer(black_data$Product_Category_2)
black_data$Product_Category_3 <- as.integer(black_data$Product_Category_3)


### Finding the user purchase counts
userIDCount <- as.data.frame(table(black_data$User_ID))
names(userIDCount) <- c("User_ID","User_Purchase_Count")


### merging the above usercount dataframe with our dataset 
black_data <- merge(x = black_data, y = userIDCount, by = "User_ID", all.x = TRUE)
str(df)  
black_data$User_Purchase_Count <- as.integer(black_data$User_Purchase_Count)  

df_Distinct <- distinct(black_data, User_ID, Age, Gender, Marital_Status, Occupation, City_Category, Stay_In_Current_City_Years, User_Purchase_Count)

### saving the pre-processed dataset
# write.csv(black_data,"Black_Friday_Dataset.csv", row.names = FALSE)


### COntents for the dashboard
black_friday_intro = "Black Friday is an informal name for the Friday following Thanksgiving Day in the United States, which is celebrated on the fourth Thursday of November. The day after Thanksgiving has been regarded as the beginning of the United States Christmas shopping season since 1952, although the term \"Black Friday\" did not become widely used until more recent decades.

Many stores offer highly promoted sales on Black Friday and open very early, such as at midnight, or may even start their sales at some time on Thanksgiving. Black Friday is not an official holiday, but California and some other states observe \"The Day After Thanksgiving\" as a holiday for state government employees, sometimes in lieu of another federal holiday, such as Columbus Day. Many non-retail employees and schools have both Thanksgiving and the following Friday off, which, along with the following regular weekend, makes it a four-day weekend, thereby increasing the number of potential shoppers."

summary_content1 = "Based on the previous pages, the following points were summarised.We could understand that during the Black Friday, Male customers bought more than Female customers. We also understood that people in the 26-35 age group are the main customers, and especially those working in # 0, # 4 and # 7 occupations. Most of the participating customers were unmarried. Although City B had a larger number of customers, City C still had the highest sales. We could realise that customers buy the most when they have lived in that city for more than a year and under 2 years. We also looked the products and came to realize that productID P00265242 was the most sold porduct during the Black Friday and it 1880 units was sold."
########################################################################################################################################################

####################################################################
# Below are the fluid pages that is used to design the dashboard   #
####################################################################

### Introduction page design

fluid_page_b <- fluidRow(
  
                          column(12,align="center",uiOutput(outputId = "image2"))
  
                        )
fluid_page_b_text <- fluidRow(
                              
                                br(),br(),
                                headerPanel(
                                  h2(strong('What is Black Friday?'),
                                     style = "font-weight: 500; color: red; font-size: 40px"))
                                ,
                                p(black_friday_intro, style = "color:white")
                                ,br(),
                                uiOutput("tab"),
                                column(width=6,offset = 5,actionButton("switch_tab_lets_know", "Click Here to know more...",icon = icon("arrow-alt-circle-right")))
                              )


### Basic Facts page design

fluid_page_a1 <- fluidRow(
                    
                            br(),
                            column(3,headerPanel(
                                h4("Gross Revenue :",
                                   style = "font-weight: 500; color: orange; font-size: 40px"))),
                        
                            
                            column(8,valueBox(paste(total_purchase,"Million",sep=' '),"Total Revenue(in millions)", icon = icon("dollar"),color = 'orange',width=8))
                    
                         )


fluid_page_a <- fluidRow(
  
                            br(),
                            
                            column(3,headerPanel(
                              h4("How many people participated :",
                                 style = "font-weight: 500; color: purple; font-size: 40px"))),
                            br(),
                            column(8,valueBox(length_user_id,"Number of Customers", icon = icon("users"),color = 'purple',width = 8))
                            
                        )

fluid_page_a2 <- fluidRow(
  
                            br(),
                            column(3,headerPanel(
                              h4("How many products where there :",
                                 style = "font-weight: 500; color: green; font-size: 40px"))),
                            br(),
                            column(8,valueBox(length_product_id,"Number of Products", icon = icon("product-hunt"),color = 'green',width = 8))
  
                          )


### Customer Demographics page design 


fluid_page_c <- fluidRow(
                    
                        box(title = "How many people participated in Black Friday?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE,
                            sidebarLayout(
                              sidebarPanel(
                                p("The figure shows that almost 72% of the customers were males and rest of it was female. This shows the trend of male customer dominating over female customer in terms of purchasing.")
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_1")
                              )
                              
                            )),
                        
                        box(title = "Which all Age group were involved in the sales?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE 
                            ,sidebarLayout(
                              sidebarPanel(
                                p("People aged  between 26-35 buy a lot more than any other cohort. The bar chart also shows the bulk of sales concentrated on young and middle-aged people.")
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_2")
                              )
                              
                            )),
                        
                        box(title = "Does Marital status of customers provide any information?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE 
                            ,sidebarLayout(
                              sidebarPanel(
                                p("An interesting thing the figure indicates is that customers who are Single buys more stuff than customers who are Married. That is nearly 60% of the customers purchased during the Black Friday Sales were",strong("Single."))
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_3")
                              )
                              
                            )),
                        
                        box(title = "Customers from which all the various professions were actively involved in the sales?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE 
                            ,sidebarLayout(
                              sidebarPanel(
                                p("Customers who worked in the Occupation #4 buys more with record customers of 740. This is followed by the people who work in Occupation #0 and #7 with customer count of 688 and 669 customers respectively.")
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_4")
                              )
                              
                            )),
                        
                        box(title = "Which city had the highest customer turn out?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE 
                            ,sidebarLayout(
                              sidebarPanel(
                                p("City C recorded the highest attendence of customers. Close to 3100 people or customers attended the Sales in City C.")
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_5")
                              )
                              
                            )),
                        
                        box(title = "Did the customer stay in a particular city influence buying pattern?"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE 
                            ,sidebarLayout(
                              sidebarPanel(
                                p("The figure shows that people who has stayed in that city for atleast 1 year purchase more than any others. The customer who are new to city or have been in the city for less than a year does not buy much. There is a downward trend for customers inflow for the sales after they have completed there 2 years or more and it decrease as the years gone by.")
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("pie_6")
                              )
                              
                            ))
                        )


### Sales Pattern page design


fluid_page_d <- fluidRow(
  
  tags$style(".nav-tabs {
  background-color: transparent;
  border-color : #FFF;
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #56a832;
border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #56a832;
}"),
    
                          tabBox(width = 6,height = "500px",title = strong('City'),
                                              
                           id = "tabset1", 
                            tabPanel("City-based Sales",
                                     sidebarLayout(
                                       sidebarPanel(
                                                    p("Among the 3 cities, ",strong('City B')," has the highest sales with almost constitute 41.5% of the total sales from 3 cities. City C and City A contributed 32.7% and 25.8% respectively. This can be easily understood with the help of the piechart. ")
                                         
                                                    ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                                  plotlyOutput('totalsalesbasedoncity')
                                                 )
                                       
                                     )),
                            
                            tabPanel("Gender-based city A sales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("When you take a look at gender based sales in city A it shows that 76.7% of customers are Male and only 23.3% of customers female.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('salescitya')
                                       )
                                       
                                     )),
                            
                            tabPanel("Gender-based city B sales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("Looking at gender-specific sales in city B, 76.7% of customers are male and just 23.3% are female.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('salescityb')
                                       )
                                       
                                     )),
                            
                            tabPanel("Gender-based city C sales",
                                      sidebarLayout(
                                        sidebarPanel(
                                          p("If you look at gender sales in city C, it shows that 76.8% of customers are men and only 23.2% are women.")
                                          
                                        ),
                                        
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotlyOutput('salescityc')
                                        )
                                        
                                      ))),
                          tabBox(width = 6,height = "500px",
                            
                            
                            title = strong("Stay Duration"),
                            
                            
                            id = "tabset2", 
                            tabPanel("< a year of stay length",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("In the case when customers stay duration is less than a year,Male customers have purchased more than Female Customers.")
                                         
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('stay0')
                                       )
                                       
                                     )),
                            tabPanel("> a year of stay length",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("Male customers who has stayed for more than a year tends to buy more than female. Customers who has stayed in a city for more a than year buys more than anyone even the customers who has been in the city for than a year")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('stay1')
                                       )
                                       
                                     )),
                            
                            tabPanel(" > 2 years of stay length",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("Male customers purchase dominates over the Female customers but overall the total sales has went down when compared it with the customers who has completed a year or more.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('stay2')
                                       )
                                       
                                     )),
                            
                            tabPanel(" > 3 years of stay length",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("Purchase or Sales pattern is same has the customers who has been in the city for more than 2 years that is still the male customers purchases more than femalecustomers.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('stay3')
                                       )
                                       
                                     )),
                            
                            tabPanel("> 4 years of stay length",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("Although the trend of Male customers purchasing more then Female customers continues but there has been a drop in sales.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('stay4')
                                       )
                                       
                                     ))),
                          tabBox(width = 6,height = "500px",
                            
                            
                            title = strong("Gender and Marital status customer-based sales"),
                            
              
                            id = "tabset3", 
                            tabPanel("Marital-based sales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("When we check the sales pattern based on marital status of customer we can notice that customers who are Single purchases more than the Married customer. That is almost 60% of the purchase is made by the Single which indeed shows the sales figure of 3008 million dollars.")
                                         
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('gm1')
                                       )
                                       
                                     )),
                            
                            tabPanel("Sales based on Male customers",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("As expected the Single male customers purchase figure dominate over the Married female customers. We can understand that almost 78% of the total single customers purchases is done by the male customers. In the case of married customers, 75% of sales is made by the married male customers.")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('gm2')
                                       )
                                       
                                     )),
                            
                            
                            tabPanel("Sales based on Female customers",
                                     sidebarLayout(
                                       sidebarPanel(
                                         p("On the other hand in the case of female customers, the trend of single's purchase dominate over married continues. Single female contribute only 22% of the total purchases whereas married female customers contribute 25% of the sales.  ")
                                         
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotlyOutput('gm3')
                                       )
                                       
                                     ))),
                          
                          
                          tabBox(width = 6,height = "500px",
                            
                                title = strong("Products"),
                                 
                                 
                                 tabPanel("Top 10 products",
                                          sidebarLayout(
                                            sidebarPanel(
                                              p("This shows the top 10 most sold product in the Black Friday. ProductID P00265242 is the product attracted most customers.")
                                              
                                            ),
                                            
                                            # Show a plot of the generated distribution
                                            mainPanel(
                                              highchartOutput('top10')
                                            )
                                            
                                          )
                                          
                                          ),
                                
                                tabPanel("Product Category 1",
                                         
                                         plotlyOutput('cate1')
                                           
                                         ),
                                tabPanel("Product Category 2",
                                          
                                         plotlyOutput('cate2')
                                         ),
                                tabPanel("Product Category 3",
                                         plotlyOutput('cate3')
                                         )
                                ),
                          
                          tabBox(width = 6,height = "500px",
                                
                                title = strong("Sales Based on Age"),
                                
                                
                                tabPanel("Sales based on Age",
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("Customers who as age between the 26 and 35 buys more products than any other age group. Customers in this age group contribute 40% of sales and rest of 60% is shared between other age groups.")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('age1')
                                           )
                                           
                                         )
                                        
                                ),
                                
                                tabPanel("Gender based categorization",
                                         
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("Overall trend shows that male customers purchase more than female customers which is been proved everytime. We also learned that customers whose age is between 26-35 buys more. But in this 78% is contributed by the male customers and rest by female customers")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('age2')
                                           )
                                           
                                         )
                                         
                                ),
                                tabPanel("Marital Status based categorization",
                                         
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("Single customers in the age group 26-35 are the biggest contributors with almost 60% of sales.")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('age3')
                                           )
                                           
                                         )
                                )),
                          
                          tabBox(width = 6,height = "500px",
                            
                                title = strong("Sales based on Occupation"),
                                
                                
                                tabPanel("Sales vs Occupation",
                                         
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("Out of the 21 occupations customers from only 3 occupation has gone has the sales figure of 500 million dollars. These occupations are #0, #4 and #7. On the other hand occupation #8 has the lowest sales(purchase) figure of 14 million dollars. ")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('Occupation1')
                                           )
                                           
                                         )
                                ),
                                
                                tabPanel("Gender based categorization",
                                         
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("The male dominance of the purchasing pattern of people with occuptions # 4, # 0 and # 7 is clearly illustrated by the figure. In addition to this we can see a outlier in occupation #9 as this is the only profession where female customers purchases more than male customers.")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('Occupation3')
                                           )
                                           
                                         )
                                         
                                ),
                                tabPanel("Marital Status based categorization",
                                         
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("As we have discussed and shown the trend of single customers dominates over married customer can be seen here as well in the case on occupation. But occupation #8 and #9 shows a opposite trend that is here married customers buys more than single customers.")
                                             
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                             plotlyOutput('Occupation2')
                                           )
                                           
                                         )
                                         
                                )
                            )
                                  
                                 
                          
                          
    
                        )


### Below are the action button design

fluid_page_summary <- fluidPage(
  
  
                                  br(),
                                  headerPanel(
                                    h4(strong("Summary"),
                                       style = "font-weight: 500; color: red; font-size: 30px")),
                                  
                                  br(),
                                  p(summary_content1, style = "color:white")
  
                                )

fluid_page_button1 <- fluidRow(
  
                                  div(style="display:inline-block",actionButton("back_fact", "Back",icon = icon("arrow-alt-circle-left")),actionButton("switch_tab_sales", "Click Here to know about Sales Pattern",icon = icon("arrow-alt-circle-right")))
  
                              )

fluid_page_button2 <- fluidRow(
  
                                  div(style="display:inline-block",actionButton("back_demo", "Back",icon = icon("arrow-alt-circle-left")),actionButton("switch_tab_summary", "Summary",icon = icon("arrow-alt-circle-right")))
  
                              )

fluid_page_button3 <- fluidRow(

                                  div(style="display:inline-block",actionButton("back_home", "Back to Home Page",icon = icon("arrow-alt-circle-left")),actionButton("switch_tab_customer", "\n\nClick Here to know about Customer Demographics",icon = icon("arrow-alt-circle-right")),style="float:right")
  
                              )

fluid_page_button4 <- fluidRow(
  
                                    actionButton("back_sales", "Back to Sales Pattern",icon = icon("arrow-alt-circle-left"))
                              )

########################################################################################################################################################
# UI AND SERVER
########################################################################################################################################################


### Below is the UI page code

ui<-dashboardPage(skin = "black",
                  dashboardHeader(title = "Black Friday Sales Analysis",titleWidth = 200,
                                  dropdownMenu(type = "messages",
                                               messageItem(
                                                 from = "Download Original Dataset",
                                                 message = "Click here to download from the link",
                                                 href = "https://www.kaggle.com/sdolezel/black-friday#train.csv"
                                               ),messageItem(
                                                 from = "Download Preprocessed Dataset",
                                                 message = "Click here to download from the link",
                                                 href = "https://drive.google.com/file/d/1QFSw_sTtjvljl8npdUCyBQB9DlQ9qjaw/view?usp=sharing"
                                               ))),
                  
                  # SideBar UI code
                  dashboardSidebar(collapsed=TRUE, 
                      sidebarMenu(  
                                    id = 'sidebarmenu',
                                    # first menu item = Home
                                        menuItem("What is Black Friday ?",tabName = "Home", icon = icon("question-circle")),
                                    
                                    # second menu item = Basic stats
                                        menuItem("Let's know more",tabName = 'Basic',icon = icon("search")),
                                    
                                    # third menu item = Demographics
                                        menuItem("Customer Demographics",tabName = "Demographics", icon = icon("user")),
                                    
                                    # fourth menu item = Sales insights  
                                        menuItem("Sales Insights",tabName = "sales", icon = icon("bar-chart")),
                                    
                                    # fifth menu item = Summary  
                                    menuItem("Summary",tabName = "summary", icon = icon("file-alt"))
                                    
                                    
                                 )
                                ),
                  
                  dashboardBody(
                    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #000000;
                                }'))),

                                
                      tabItems(
                          tabItem("Home",fluid_page_b,fluid_page_b_text),
                          tabItem("Basic",fluid_page_a,fluid_page_a1,fluid_page_a2,br(),br(),fluid_page_button3),
                          tabItem("Demographics",fluid_page_c,fluid_page_button1),
                          tabItem("sales",fluid_page_d,fluid_page_button2),
                          tabItem("summary",fluid_page_summary,br(),br(),fluid_page_button4)
                          
                          
                      ))
                           
)



### Below is the server code 


server <- function(input, output, session) {
  
      
      output$image2 <- renderUI({
        
        
        tags$div(img(src = "black_friday_1.gif"))
        
      })
      
      
  
  
    
    
    #################################
    # Page on demographic insights  #
    #################################
    
    
    
    
    output$pie_1 <- renderPlotly(
        
        {
            
            GenderCount <- as.data.frame(table(df_Distinct$Gender))
            names(GenderCount) <- c("Gender","Count")
            Gender <- GenderCount$Gender
            Count <- GenderCount$Count
            fig <- plot_ly(GenderCount, labels = ~Gender, values = ~Count,
                           marker = list(colors = c('rgba(255,182,193,1)','rgba(0,191,255,1)'),
                                         line = list(color = '#FFFFFF', width = 1)))
                          
            
            fig <- fig %>% add_pie(hole = 0.6)
            # fig <- fig %>% layout(title = "Male and Female customers")
            
            fig
            
            
            
            
        }
    )
    
    output$pie_4 <- renderPlotly(
        
        {
            
            OccupationCount <- as.data.frame(table(df_Distinct$Occupation))
            names(OccupationCount) <- c("Occupation","Count")
            Occupation <- OccupationCount$Occupation
            Count <- OccupationCount$Count
            
            fig <- plot_ly(
                x = Occupation,
                y = OccupationCount$Count,
                name = "Occupation",
                type = "bar",
                showlegend = FALSE,
                color = Occupation
            )
            
            fig <- fig %>% layout(              xaxis = list(title = " Occupation"),
                                  yaxis = list(title = "Number of customers"))
            fig
            
            
        }
    )
    
    output$pie_5 <- renderPlotly(
        
        {
            
            CityCount <- as.data.frame(table(df_Distinct$City_Category))
            names(CityCount) <- c("City","Count")
            City <- CityCount$City
            Count <- CityCount$Count
            CityCount$City <- paste('City',CityCount$City,sep=" ")
            
            fig <- plot_ly(CityCount, labels = ~City, values = ~Count, type = 'pie',
                           textposition = 'inside',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF'),showlegend = TRUE)
            # fig <- fig %>% layout(title = "Count of Customers from different Cities")
            
            fig
           
            
        }
    )
    
    output$pie_6 <- renderPlotly(
        
        {
            
            StayCount <- as.data.frame(table(df_Distinct$Stay_In_Current_City_Years))
            names(StayCount) <- c("Stay","Count")
            Stay <- StayCount$Stay
            Count <- StayCount$Count
            Stay <- paste(Stay, "year",sep=' ')
            
           fig <- plot_ly(
                x = StayCount$Stay,
                y = Count,
                type = "bar",
                showlegend = TRUE,
                color = Stay
            )
            
            fig <- fig %>% layout(
                                  xaxis = list(title = " Stay of length"),
                                  yaxis = list(title = "Number of customers"))
            fig
            
            
        }
    )    
        
    output$pie_3 <- renderPlotly(
            
            {
                
                maritalCount <- as.data.frame(table(df_Distinct$Marital_Status))
                names(maritalCount) <- c("marital","Count")
                marital <- maritalCount$marital
                Count <- maritalCount$Count
                
                
                fig <- plot_ly(
                    x = marital,
                    y = Count,
                    type = "bar",
                    showlegend = TRUE,
                    color = marital
                )
                
                fig <- fig %>% layout(
                                      xaxis = list(title = " Marital Status"),
                                      yaxis = list(title = "Number of customers"))
                fig
                
                
            }
        )
    output$pie_2 <- renderPlotly(
        
        {
            
            AgeCount <- as.data.frame(table(df_Distinct$Age))
            names(AgeCount) <- c("age","Count")
            age <- AgeCount$age
            Count <- AgeCount$Count
            
            
            fig <- plot_ly(
                x = age,
                y = Count,
                type = "bar",
                showlegend = TRUE,
                color = age
            )
            
            fig <- fig %>% layout(
                                  xaxis = list(title = " Age"),
                                  yaxis = list(title = "Number of customers"))
            fig
            
            
        }
    )
    
     
   #################################
   #    Page on Sales insights   #
   #################################
    
    
    
    output$totalsalesbasedoncity <- renderPlotly({
        
              df <- black_data %>% group_by(City_Category) %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste("City",df$City_Category,sep=' ')
              
              
              
              fig <- plot_ly(df, labels = ~text, values = ~Sales,
                             marker = list(colors = c('rgba(255,182,193,1)','rgba(0,191,255,1)'),
                                           line = list(color = '#FFFFFF', width = 1)))
              
              fig <- fig %>% add_pie(hole = 0)
             # fig <- fig %>% layout(title = "Sales in 3 Cities")
              
              fig
              
              
        
        
        
    })
    
    output$salescitya <- renderPlotly({
      
              df <- black_data %>% group_by(Gender,City_Category) %>% filter(City_Category == 'A') %>% summarise(Sales = sum(Purchase)/1000000)
              
              
              
              fig <- plot_ly(df, labels = ~Gender, values = ~Sales,
                             marker = list(colors = c('rgba(255,182,193,1)','rgba(0,191,255,1)'),
                                           line = list(color = '#FFFFFF', width = 1)))
              fig <- fig %>% add_pie(hole = 0.6)
              
              #fig <- fig %>% layout(title = "Sales in City A")
              
              fig
              
      
      
      
    })
    output$salescityb <- renderPlotly({
      
              df <- black_data %>% group_by(Gender,City_Category) %>% filter(City_Category == 'B') %>% summarise(Sales = sum(Purchase)/1000000)
              
              
              
              fig <- plot_ly(df, labels = ~Gender, values = ~Sales,
                              marker = list(colors = c('rgba(255,182,193,1)','rgba(0,191,255,1)'),
                                            line = list(color = '#FFFFFF', width = 1)))
              
              fig <- fig %>% add_pie(hole = 0.6)
              
              #fig <- fig %>% layout(title = "Sales in City B")
              
              fig
      
      
      
    })
    output$salescityc <- renderPlotly({
      
              df <- black_data %>% group_by(Gender,City_Category) %>% filter(City_Category == 'C') %>% summarise(Sales = sum(Purchase)/1000000)
              
              
              
              fig <- plot_ly(df, labels = ~Gender, values = ~Sales,
                             marker = list(colors = c('rgba(255,182,193,1)','rgba(0,191,255,1)'),
                                           line = list(color = '#FFFFFF', width = 1)))
              fig <- fig %>% add_pie(hole = 0.6)
              
              #fig <- fig %>% layout(title = "Sales in City C")
              
              fig
      
      
      
    })
    output$stay1 <- renderPlotly({
      
              df <- black_data %>% group_by(Stay_In_Current_City_Years,Gender) %>% filter(Stay_In_Current_City_Years == '1') %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Gender,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Gender,
                marker = list(color =c('orange','green'))
                
              )
              
              fig <- fig %>% layout(
                                   xaxis = list(title = " Gender"),
                                   yaxis = list(title = "Sales (in million)"))
              
              fig
      
      
      
    })
    output$stay2 <- renderPlotly({
      
              df <- black_data %>% group_by(Stay_In_Current_City_Years,Gender) %>% filter(Stay_In_Current_City_Years == '2') %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Gender,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Gender
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = " Gender"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
      
      
      
    })
    output$stay3 <- renderPlotly({
      
              df <- black_data %>% group_by(Stay_In_Current_City_Years,Gender) %>% filter(Stay_In_Current_City_Years == '3') %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Gender,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Gender,marker = list(color =c('orange','green'))
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = " Gender"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
              
      
      
    })
    output$stay4 <- renderPlotly({
      
              df <- black_data %>% group_by(Stay_In_Current_City_Years,Gender) %>% filter(Stay_In_Current_City_Years == '4+') %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Gender,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Gender
                
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = " Gender"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
      
      
      
    })
    output$stay0 <- renderPlotly({
      
      df <- black_data %>% group_by(Stay_In_Current_City_Years,Gender) %>% filter(Stay_In_Current_City_Years == '0') %>% summarise(Sales = sum(Purchase)/1000000)
      
      text <- paste(df$Sales, "million",sep=' ')
      
      fig <- plot_ly(
        x = df$Gender,
        y = df$Sales,
        type = "bar",
        text=text,
        showlegend = TRUE,
        color = df$Gender
        
        
      )
      
      fig <- fig %>% layout(
        xaxis = list(title = "Gender"),
        yaxis = list(title = "Sales (in million)"))
      
      fig
      
      
      
    })
    output$gm1 <- renderPlotly({
      
              df <- black_data %>% group_by(Marital_Status) %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Marital_Status,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Marital_Status,
                marker = list(color =c('orange','green'))
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "Marital Status"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
      
      
      
    })
    output$gm2 <- renderPlotly({
      
              df <- black_data %>% group_by(Marital_Status,Gender)%>%filter(Gender =='Male') %>% summarise(Sales = sum(Purchase)/1000000)
            
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Marital_Status,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Marital_Status
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "Marital Status"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
      
      
      
    })
    output$gm3 <- renderPlotly({
              
              df <- black_data %>% group_by(Marital_Status,Gender)%>%filter(Gender =='Female') %>% summarise(Sales = sum(Purchase)/1000000)
              
              text <- paste(df$Sales, "million",sep=' ')
              
              fig <- plot_ly(
                x = df$Marital_Status,
                y = df$Sales,
                type = "bar",
                text=text,
                showlegend = TRUE,
                color = df$Marital_Status,
                marker = list(color =c('orange','green'))
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "Marital Status"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
              
      
      
    })
    
    
    
    output$top10 <- renderHighchart({
          
                black_data %>% group_by(Product_ID) %>% count(sort = TRUE) %>% head(10)%>% hchart("treemap", hcaes(x = 'Product_ID', value = 'n', color = 'n'))
      
      
    })
    
    output$cate1 <- renderPlotly({
      
      
              df <- black_data %>% group_by(Product_Category_1) %>% summarise(Sales = sum(Purchase)/1000000)
              fig <- plot_ly(
                x = df$Product_Category_1,
                y = df$Sales,
                type = "bar",

                showlegend = FALSE,
                color = df$Product_Category_1


              )

              fig <- fig %>% layout(
                                    xaxis = list(title = "Product_Category_1"),
                                    yaxis = list(title = "Sales (in million)"))

              fig
    })
    output$cate2 <- renderPlotly({
      
              df <- black_data %>% group_by(Product_Category_2) %>% summarise(Sales = sum(Purchase)/1000000)
              fig <- plot_ly(
                x = df$Product_Category_2,
                y = df$Sales,
                type = "bar",
                
                showlegend = FALSE,
                color = df$Product_Category_2
                
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "Product_Category_2"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
    })
    output$cate3 <- renderPlotly({
      
              df <- black_data %>% group_by(Product_Category_3) %>% summarise(Sales = sum(Purchase)/1000000)
              fig <- plot_ly(
                x = df$Product_Category_3,
                y = df$Sales,
                type = "bar",
                
                showlegend = FALSE,
                color = df$Product_Category_3
                
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "Product_Category_3"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
    })
    output$age1 <- renderPlotly({
      
              df<- black_data %>% group_by(Age) %>% summarise(Sales = sum(Purchase)/1000000)
              text <- paste(df$Sales," million",sep = ' ')
              fig <- plot_ly(
                x = df$Age,
                y = df$Sales,
                type = "bar",
                text = text,
                showlegend = FALSE,
                color = df$Age
                
                
              )
              
              fig <- fig %>% layout(
                                    xaxis = list(title = "age"),
                                    yaxis = list(title = "Sales (in million)"))
              
              fig
    })
    
    output$age2 <- renderPlotly({
      
            male<- black_data %>% group_by(Age,Gender) %>%filter(Gender=='Male')%>% summarise(Sales = sum(Purchase)/1000000)
            female<- black_data %>% group_by(Age,Gender) %>%filter(Gender=='Female')%>% summarise(Sales = sum(Purchase)/1000000)
            
            
            fig <- plot_ly(
              x = male$Age,
              y = male$Sales,
              type = "bar",
              name = 'Male',
              showlegend = TRUE,
              
              marker = list(color = 'light blue')
              
              
            )
            fig <- fig %>% add_trace(y = female$Sales, name = 'Female',marker = list(color = 'pink'))
            fig <- fig %>% layout(barmode = 'group')
            fig <- fig %>% layout(
                                  xaxis = list(title = "age"),
                                  yaxis = list(title = "Sales (in million)"))
            
            fig
    })
    output$age3 <- renderPlotly({
      
          single<- black_data %>% group_by(Age,Marital_Status) %>%filter(Marital_Status=='Single')%>% summarise(Sales = sum(Purchase)/1000000)
          married<- black_data %>% group_by(Age,Marital_Status) %>%filter(Marital_Status=='Married')%>% summarise(Sales = sum(Purchase)/1000000)
          y = c(0,married$Sales)
          y
          
          fig <- plot_ly(
            x = single$Age,
            y = single$Sales,
            type = "bar",
            name = 'Single',
            showlegend = TRUE
            
          )
          fig <- fig %>% add_trace(y = y, name = 'married')
          fig <- fig %>% layout(barmode = 'group')
          fig <- fig %>% layout(
                                xaxis = list(title = "age"),
                                yaxis = list(title = "Sales (in million)"))
          
          fig
    })
    output$Occupation1 <- renderPlotly({
      
            df<- black_data %>% group_by(Occupation) %>% summarise(Sales = sum(Purchase)/1000000)
            text <- paste(df$Sales," million",sep = ' ')
            fig <- plot_ly(
              x = df$Occupation,
              y = df$Sales,
              type = "bar",
              text = text,
              name = 'Occupation',
              showlegend = FALSE,
              color = df$Occupation
              
              
            )
            
            fig <- fig %>% layout(
                                  xaxis = list(title = "Occupation"),
                                  yaxis = list(title = "Sales (in million)"))
            
            fig
    })
    output$Occupation2 <- renderPlotly({
      
          single<- black_data %>% group_by(Occupation,Marital_Status) %>%filter(Marital_Status=='Single')%>% summarise(Sales = sum(Purchase)/1000000)
          married<- black_data %>% group_by(Occupation,Marital_Status) %>%filter(Marital_Status=='Married')%>% summarise(Sales = sum(Purchase)/1000000)
          
          fig <- plot_ly(
            x = married$Occupation,
            y = married$Sales,
            type = "bar",
            name = 'married',
            showlegend = TRUE
            
          )
          fig <- fig %>% add_trace(y = single$Sales, name = 'single')
          fig <- fig %>% layout(barmode = 'group')
          fig <- fig %>% layout(
                                xaxis = list(title = "Occupation"),
                                yaxis = list(title = "Sales (in million)"))
          
          fig
    })
    output$Occupation3 <- renderPlotly({
      
      male<- black_data %>% group_by(Occupation,Gender) %>%filter(Gender=='Male')%>% summarise(Sales = sum(Purchase)/1000000)
      female<- black_data %>% group_by(Occupation,Gender) %>%filter(Gender=='Female')%>% summarise(Sales = sum(Purchase)/1000000)
          
          fig <- plot_ly(
            x = male$Occupation,
            y = male$Sales,
            type = "bar",
            name = 'Male',
            showlegend = TRUE
            
          )
          fig <- fig %>% add_trace(y = female$Sales, name = 'Female')
          fig <- fig %>% layout(barmode = 'group')
          fig <- fig %>% layout(
                                xaxis = list(title = "Occupation"),
                                yaxis = list(title = "Sales (in million)"))
          
          fig
    })
    
    
    # observe events for all the action buttons
    
    observeEvent(input$switch_tab_lets_know, {
      updateTabItems(session, "sidebarmenu",selected = "Basic")
    })
    
    observeEvent(input$switch_tab_customer, {
      updateTabItems(session, "sidebarmenu",selected = "Demographics")
    })
    
    observeEvent(input$switch_tab_sales, {
      updateTabItems(session, "sidebarmenu",selected = "sales")
    })
    
    observeEvent(input$switch_tab_summary, {
      updateTabItems(session, "sidebarmenu",selected = "summary")
    })
    
    observeEvent(input$back_home, {
      updateTabItems(session, "sidebarmenu",selected = "Home")
    })
    
    observeEvent(input$back_fact, {
      updateTabItems(session, "sidebarmenu",selected = "Basic")
    })
    
    observeEvent(input$back_demo, {
      updateTabItems(session, "sidebarmenu",selected = "Demographics")
    })
    
    observeEvent(input$back_sales, {
      updateTabItems(session, "sidebarmenu",selected = "sales")
    })
    
    
    
    
    url <- a("History of Black Friday", href="https://youtu.be/6GX7l--iOBU")
    output$tab <- renderUI({
      tagList(p("URL link:",style="color:red"), url)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
