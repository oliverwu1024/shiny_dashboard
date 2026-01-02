library(rsconnect)
rsconnect::setAccountInfo(name='oliverwu1024',
                          token='A04F9D1AFB88312DF5B6847DBD75EBB5',
                          secret='h8iEnxBAY2bbHWr4jmFAktoGAUT5W477W7MIgs6V')
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(jsonlite)
library(gganimate)
library(reshape2)
library(shinyjs)
library(rintrojs)
library(gifski)
# File paths
combined_data<-read_csv("combined_data.csv")
combined_data$Period <- factor(combined_data$Period, levels = c("Pre-AUSFTA", "Post-AUSFTA"))
us_borninau <- read_csv("us_borninau_with_factors.csv")
au_borninus <- read_csv("au_borninus_with_factors.csv")
combined_data2<-read_csv("combined_data2.csv")

#radar chart 1
data_1<-read_json("au_borninus_r_corr.json", simplifyDataFrame = TRUE)
# Prepare data for radar chart
data_1 <- data_1 %>%
  mutate(variable = factor(variable, levels = variable)) %>%
  mutate(angle = 2 * pi * (as.numeric(variable) - 1) / nrow(.))

# Calculate the coordinates for the polygon
data_poly_1 <- data_1 %>%
  mutate(x = r_squared * sin(angle), y = r_squared * cos(angle))

sizes <- c("Weak" = 5, "Moderate" = 10, "Strong" = 15)
data_1$explanatory_power <- factor(data_1$explanatory_power, levels = c("Strong", "Moderate", "Weak"), ordered = TRUE)
data_1$custom_size <- sizes[as.character(data_1$explanatory_power)]


#radar chart 2
data_2<-read_json("us_borninau_r_corr.json", simplifyDataFrame = TRUE)
# Prepare data for radar chart
data_2 <- data_2 %>%
  mutate(variable = factor(variable, levels = variable)) %>%
  mutate(angle = 2 * pi * (as.numeric(variable) - 1) / nrow(.))

# Calculate the coordinates for the polygon
data_poly_2 <- data_2 %>%
  mutate(x = r_squared * sin(angle), y = r_squared * cos(angle))

sizes2 <- c("Weak" = 5, "Moderate" = 10, "Strong" = 15)
data_2$explanatory_power <- factor(data_2$explanatory_power, levels = c("Strong", "Moderate", "Weak"), ordered = TRUE)

data_2$custom_size <- sizes2[as.character(data_2$explanatory_power)]









# Load correlation matrix CSV file
cor_matrix <- read_csv("correlation matrix.csv")



# Define UI
ui <- navbarPage(
  title = "Global Perspectives: American and Australian Immigration",
  id = "nav",
  useShinyjs(),
  introjsUI(),
  tabPanel("1. Welcome",
           value = "1",
           useShinyjs(),
           
           fluidPage(
             # Add tutorial button only in welcome page
             div(
               style = "position: absolute; top: 8px; right: 450px; z-index: 1000;",
               actionButton("startTutorial", "Start Tutorial", 
                            icon = icon("question-circle"),
                            class = "btn-sm",
                            
                            style = "
                      font-size: 12px;
                      padding: 4px 8px;
                      height: auto;
                      background-color: #f8f9fa;
                      border: 1px solid #ddd;
                      color: #333;
                      margin-bottom: 0;
                    ")
             ),
             
             # Custom CSS for the welcome page
             tags$head(
               tags$link(rel = "stylesheet", 
                         href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600&display=swap"),
               
               tags$style("
    .navbar-brand {
      font-family: 'Inter', sans-serif !important;
      font-weight: 600 !important;
      font-size: 22px !important;
      color: #1a1a1a !important;
      padding: 15px 25px !important;
      position: relative;
      display: flex;
      align-items: center;
      gap: 10px;
    }
    
    .navbar {
      background-color: #ffffff !important;
      border-bottom: 1px solid #eaeaea !important;
    }
    
    /* Minimal dot accent */
    .navbar-brand:before {
      content: '';
      display: inline-block;
      width: 8px;
      height: 8px;
      background: #3498db;
      border-radius: 50%;
      transition: transform 0.3s ease;
    }
    
    .navbar-brand:hover:before {
      transform: scale(1.5);
    }
  ")
             ),
             
             
             
             
             
             
             
             
             
             
             
             
             tags$head(
               tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Roboto:wght@300;400&display=swap"),
               # Custom CSS for introjs tutorial
               tags$style("
      .navbar {
        display: flex;
        align-items: center;
      }
      .navbar-brand {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      #startTutorial:hover {
        background-color: #e9ecef;
        border-color: #ddd;
      }
    ")
               
               
               
             )),
           
           
           
           
           # Page structure
           div(
             class = "container",
             
             # Header
             div(
               class = "header",
               `data step` = 1,
               `data-intro` = "Welcome to Global Perspectives! This application explores immigration patterns between the USA and Australia.",
               style = "background-color: #fff; padding: 20px; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
               div(
                 class = "logo",
                 style = "font-family: 'Playfair Display', serif; font-size: 24px; color: #333; text-transform: uppercase; letter-spacing: 2px;",
                 "Global Perspectives"
               )
             ),
             
             # Main content
             div(
               class = "main",
               style = "flex-grow: 1; display: flex; flex-direction: column; align-items: center; justify-content: flex-start; padding: 40px;",
               
               # Title box
               div(
                 class = "title-box",
                 `data-step` = 2,
                 `data-intro` = "This section introduces our main focus: exploring the relationship between economic factors and immigration.",
                 style = "background-color: rgba(255, 255, 255, 0.9); padding: 40px; border-radius: 10px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); text-align: center; max-width: 80%; margin-bottom: 40px;",
                 h1(
                   "American and Australian Immigration",
                   style = "font-family: 'Playfair Display', serif; font-size: 2.5em; color: #333; margin-bottom: 20px; line-height: 1.2;"
                 ),
                 div(
                   class = "decorative-line",
                   style = "width: 50px; height: 3px; background-color: #c3cfe2; margin: 20px auto;"
                 ),
                 p(
                   "Exploring the Relationship between Economic Factors",
                   style = "font-size: 1.2em; color: #666; margin-bottom: 30px;"
                 )
               ),
               
               # Introduction
               div(
                 class = "introduction",
                 `data-step` = 3,
                 `data-intro` = "Here you'll find our research questions and key objectives for this analysis.",
                 
                 style = "background-color: rgba(255, 255, 255, 0.9); padding: 40px; border-radius: 10px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); max-width: 80%; margin-bottom: 40px; line-height: 1.6;",
                 h2(
                   "Introduction",
                   style = "font-family: 'Playfair Display', serif; color: #333; margin-bottom: 20px;"
                 ),
                 p(
                   "Welcome to our exploration of the dynamic relationship between economic factors and immigration patterns between the United States and Australia. These two nations share close economic ties as significant trading partners and investors, with economic developments on both sides influencing not only trade but also the movement of people across borders."
                 ),
                 p(
                   "This project investigates how changes in trade agreements, economic performance, and population shifts impact immigration trends between Australia and the U.S. Our research is guided by the following key questions:"
                 ),
                 tags$ul(
                   tags$li("Is there a relationship between changes in the populations of Australia and the United States?"),
                   tags$li("How do trade volume, trade agreements, and economic performance relate to immigration between the two countries?"),
                   tags$li("How do changes in the economies of Australia and the United States impact trade between them?")
                 ),
                 p(
                   "Whether you are a policymaker, company, or potential migrant, this project offers insights into how economic changes drive migration trends between these two economically connected nations. Through data-driven analysis and research, we aim to uncover the factors influencing the movement of people and the trade that supports these international relationships."
                 )
                
                 
               ),
               
               # Table of Contents
               div(
                 class = "table-of-contents",
                 `data-step` = 4,
                 `data-intro` = "Use this table of contents to navigate through different sections of our analysis.",
                 style = "background-color: rgba(255, 255, 255, 0.9); padding: 40px; border-radius: 10px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); max-width: 80%; line-height: 1.6;",
                 h2(
                   "Table of Contents",
                   style = "font-family: 'Playfair Display', serif; color: #333; margin-bottom: 20px;"
                 ),
                 tags$ol(
                   style = "list-style-type: decimal; padding-left: 20px;",
                   tags$li("Welcome"),
                   tags$li("The Change Of American and Australian Immigrant Population"),
                   tags$li("Australians Born in the United States vs Economic Factors"),
                   tags$li("Americans Born in Australia vs Economic Factors"),
                   tags$li("Economic Indicators Correlation Analysis: USA & Australia"),
                   tags$li("Summary and Insights"),
                   tags$li("Appendix")
                 )
               )
             )
           )
  ),
  # Navbar 2: Boxplot Section
  tabPanel("2. The Change Of American and Australian Immigrant Population",
           value = "2",
           fluidPage(
             # Introduction section at the top
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "introduction-section",
                   `data-step` = 1,
                   `data-intro` = "This section introduces the impact of the Australia-United States Free Trade Agreement (AUSFTA) on immigration patterns between the two nations.",
                   style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0;",
                   h3("Introduction"),
                   p(
                     "The section 'The Change of American and Australian Immigrant Population' explores ",
                     "the effects of the Australia-United States Free Trade Agreement (AUSFTA) on immigration patterns ",
                     "between the two nations. AUSFTA, implemented in 2005, sought to strengthen economic ties and ",
                     "encourage movement across borders by enhancing trade, investments, and economic cooperation. ",
                     "Here, we examine the trends in immigrant populations from both the USA and Australia before and ",
                     "after the agreement."
                   ),
                   p(
                     "Through interactive box plots, users can visualize how immigration patterns shifted over time, ",
                     "comparing pre- and post-AUSFTA data to gain insights into its potential impact on migration. ",
                     "An animated bar race further illustrates changes in the immigrant populations between the two ",
                     "countries over the years."
                   )
                 )
               )
             ),
             
             # Spacer for separation between introduction and boxplot section
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 30px;")  # Adjust this height for spacing
               )
             ),
             
             # Boxplot and Analysis Section
             fluidRow(
               column(
                 width = 7,  # Left column for radio buttons, title, and plot
                 h3("Immigration Trends by AUSFTA Period"),  # Title before box plot
                 div(
                   class = "radio-button-section",  # Added class for tutorial
                   `data-step` = 2,
                   `data-intro` = "Choose different views of the data: Pre-AUSFTA period, Post-AUSFTA period, or a comparison of both.",
                   radioButtons(
                     "plot_type", 
                     "Select the plot to display:",
                     choices = list(
                       "Pre-AUSFTA" = "Pre-AUSFTA", 
                       "Post-AUSFTA" = "Post-AUSFTA", 
                       "Comparison" = "Comparison"
                     ),
                     selected = "Comparison"
                   )
                 ),
                 div(
                   `data-step` = 3,
                   `data-intro` = "This boxplot visualizes the population changes for immigrants in both countries, divided into pre and post AUSFTA periods.",
                   plotOutput("boxPlot")
                 )
               ),
               column(
                 width = 5,  # Right column for text
                 div(
                   class = "text-section",
                   `data-step` = 4,
                   `data-intro` = "Here you'll find detailed analysis of the trends shown in the boxplot, including insights about migration patterns before and after AUSFTA.",
                   style = "padding-left: 20px;",
                   h3("Analysis"),
                   p(
                     "The boxplot provides insights into the population changes for Australians residing in Australia who were born in the USA and Americans residing in the USA who were born in Australia, segmented by periods before and after the AUSFTA agreement."
                   ),
                   p(
                     strong("Pre-AUSFTA Period:"),
                     br(),
                     "In the period before the AUSFTA, the median population of Australians residing in Australia who were born in the USA was lower, suggesting a relatively stable or moderate level of migration. ",
                     "The population of Americans residing in the USA who were born in Australia also shows lower variation, indicating fewer incentives or pathways for immigration between the two countries during this time."
                   ),
                   p(
                     strong("Post-AUSFTA Period:"),
                     br(),
                     "After the AUSFTA came into effect, both groups show a noticeable increase in population, particularly for Australians residing in Australia who were born in the USA. ",
                     "The spread and median values have increased, implying an upward trend in immigration, possibly driven by more favorable conditions under the trade agreement. ",
                     "For Americans residing in the USA who were born in Australia, while the population has increased, the growth is moderate in comparison but still reflects some positive impact from the agreement."
                   ),
                   p(
                     strong("Overall Comparison:"),
                     br(),
                     "The boxplot reveals that AUSFTA likely facilitated increased migration between the two countries, especially benefiting Australians residing in Australia who were born in the USA. ",
                     "The increased variation in the post-AUSFTA period suggests greater movement flexibility and perhaps easier migration policies or increased economic incentives due to the trade relationship."
                   )
                 )
               )
             ),
             
             # Spacer for separation between boxplot and animated bar race section
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 30px;")  # Adjust this height for spacing
               )
             ),
             
             # Animated Bar Race Section
             fluidRow(
               column(
                 width = 12,
                 h3("Animated Immigration Trends"),  # Title for the animated bar race
                 div(
                   class = "animation-description-section",
                   `data-step` = 5,
                   `data-intro` = "Below, you'll find an animated visualization showing how immigration patterns changed over time between the USA and Australia.",
                   style = "padding: 15px; background-color: #f9f9f9; border: 1px solid #e0e0e0; margin-bottom: 20px;",
                   h4("Description"),
                   p(
                     "The animated bar race visualizes the changes in immigrant populations between the USA and Australia over time, highlighting trends for both Australians residing in Australia who were born in the USA and Americans residing in the USA who were born in Australia. ",
                     "This visualization is particularly useful for tracking changes before and after the AUSFTA agreement in 2005."
                   ),
                   p(
                     "Around 2005, coinciding with the AUSFTA agreement, a notable trend emerges: the population of Australians residing in Australia who were born in the USA surpasses that of Americans residing in the USA who were born in Australia. ",
                     "This shift suggests that the AUSFTA may have facilitated easier movement or economic incentives, encouraging more Australians to reside in Australia after being born in the USA. ",
                     "The animation effectively highlights this tipping point and other significant shifts, allowing us to observe the impact of policy changes on migration patterns."
                   ),
                   p(
                     "This dynamic visualization offers an engaging perspective on the flow of people between these two countries, showcasing how external factors such as trade agreements can shape immigration trends over time."
                   )
                 ),
                 div(
                   `data-step` = 6,
                   `data-intro` = "This animated bar race shows the dynamic changes in immigrant populations over time, highlighting key trends and shifts.",
                   imageOutput("animatedBarRace")
                 )
               )
             )
           )
  ),
             
            
  
  
  
  
  #navbar 3: Australians Born in the United States vs Economic Factors
  tabPanel("3. Australians Born in the United States vs Economic Factors",
           value = "3",
           fluidPage(
             # Introduction text
             fluidRow(
               column(
                 width = 12,
                 class = "introduction-section2",
                 `data-step` = "1",
                 `data-intro` = "Welcome to the Economic Factors analysis section. Here we'll explore how various economic indicators influence Australians born in the United States.",
                 style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0;",
                 h3("Introduction"),
                 p("This analysis explores the impact of various economic factors on Australians born in the United States who reside in Australia. Through visualizations such as scatter plots, radar charts, and bubble charts, we aim to understand how economic indicators like inflation, GDP growth, trade volumes, and stock market performance influence this demographic."),
                 p("In the scatter plot section, you can examine specific relationships between selected economic factors and the Australian-born U.S. residents. Following this, the radar chart offers a comprehensive view of the explanatory power of each factor, allowing for easy comparison across variables. Finally, the bubble chart provides insights into how strongly each factor correlates with the observed trends, with the size and color indicating the relative influence and direction of these relationships."),
                 p("This interactive dashboard allows you to customize the displayed visualizations, enabling a tailored exploration of how key economic factors shape the experiences and conditions of Australians born in the United States.")
               )
             ),
             # Spacer for separation between introduction and scatter plot section
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 30px;")  
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "scatterplot-explanation",  
                   `data-step` = "2",
                   
                   `data-intro` = "This section explains how to interpret the scatterplots and their patterns.",
                 h3("Economic Factors and Their Relationship on Australians Born in the United States"),
                 
                 p("Select the scatterplots to display below to explore the relationship between economic factors and Australians born in the United States who reside in Australia."),
                 p("You can choose multiple scatterplots to compare different economic indicators and observe their impact on the target demographic."),
                 p("If the scatterplot shows a ' / ' shape, this indicates a positive correlation between the economic factor and Australian born in united states."),
                 p("Conversely, if it displays a ' \\ ' shape, this suggests a negative correlation."), 
                 p("You can also hover over the points to view specific data points and see the statistics of the point"),
                 p("Analyse these patterns to interpret the relationship strength and direction between economic factors and the demographic.")
               )
               )
             ),
             
             
             
             # Scatterplot selection and output
             fluidRow(
               column(
                 width = 4,
                 div(
                   class = "scatterplot-selection",
                   `data-step` = "3",
                   `data-intro` = "Select different economic indicators to view their relationships with the immigrant population. You can choose multiple factors to compare.",
                   
                 checkboxGroupInput("scatterplots", 
                                    "Select scatterplots to display:",
                                    choices = list(
                                      "ASX200 Yearly Return" = "asx_return",
                                      "Export to USA" = "export_usa",
                                      "Import from USA" = "import_usa",
                                      "AUS Yearly Unemployment Rate" = "aus_unemployment",
                                      "AUS GDP Growth" = "aus_gdp",
                                      "AUS Inflation Rate" = "aus_inflation",
                                      "USA Inflation Rate" = "usa_inflation",
                                      "USA GDP Growth" = "usa_gdp",
                                      "Export to AUS" = "export_aus",
                                      "Import from AUS" = "import_aus",
                                      "USA Unemployment Rate" = "usa_unemployment",
                                      "S&P500 Yearly Return"= "sp500_return"
                                    ),
                                    selected = c("asx_return")
                 )
                 ) 
               ),
               column(
                 width = 8,
                 div(
                   class = "scatterplot-display",
                   `data-step` = "4",
                   `data-intro` = "The scatterplots show relationships between economic factors and population. A '/' shape indicates positive correlation, while a '\\' shape suggests negative correlation.",
                  uiOutput("scatterPlots")
               )                   
                 )
             ),
             
             # Spacer div
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 100px;")
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "radar-intro", 
                   `data-step` = "5",
                   `data-intro` = "The radar chart helps visualize the explanatory power of each economic factor.",
                 h4("Economic Factors and Their Impact on Australians Born in the United States"),
                 p("The radar chart below visualizes the explanatory power of each selected economic factor, allowing you to compare their influence on Australians born in the United States residing in Australia."),
                 p("Each axis represents an economic factor, and the distance from the center reflects the explanatory power. Factors with higher values exert greater influence on the target demographic."),
                 p("Use this chart to identify which economic factors have the strongest impact and observe the combined effect of selected variables."),
                 p("You have to select at least three variables you are interested in to generate the radar chart. Multiple selections are allowed.")
               )
               )
             ),
             
             # Radar chart selection and output
             fluidRow(
               column(
                 width = 2,
                 div(
                   class = "radar-selection",
                   `data-step` = "6",
                   `data-intro` = "Choose at least three variables to generate a radar chart. This will help you compare the influence of different economic factors.",
                 checkboxGroupInput("selected_variables",  
                                    "Select variables for radar chart:",
                                    choices = data_1$variable,
                                    selected = NULL
                 )
                 )
               ),
               column(
                 width = 10, 
                 div(
                   class = "radar-display",
                   `data-step` = "7",
                   `data-intro` = "The radar chart shows the explanatory power of each factor. Longer spokes indicate stronger influence on the immigrant population.",
                 plotOutput("radarPlot", height = "900px")
               )
               )
             ),
             
             # Spacer div
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 100px;")
               )
             ),
             
             fluidRow(
               column(
                 width = 4, 
                 div(
                   class = "bubble-selection", 
                   `data-step` = "8",
                   `data-intro` = "Select which factors to display in the bubble chart. Each bubble represents an economic factor.",
                 checkboxGroupInput(
                   "bubble_selection",
                   "Select bubbles to display:",
                   choices = data_1$variable,
                   selected = data_1$variable
                 )
                 )
               ),
               column(
                 width = 8,
                 div(
                   class = "bubble-guide",
                   `data-step` = "9",
                   `data-intro` = "This guide explains how to interpret the bubble chart visualization.",
                 h4("Bubble Chart Operation Guide"),
                 p("This bubble chart visualizes the relationship between explanatory power, correlation, and the absolute value of correlation for each economic factor."),
                 p("To use the chart:"),
                 tags$ul(
                   tags$li("Select the economic factors you want to display by checking the boxes. The bubbles represent the selected factors."),
                   tags$li("The bubble size indicates the explanatory power of each factor. Larger bubbles show a higher influence on Australians born in the USA."),
                   tags$li("The bubble color represents the correlation: Blue for positive correlation, red for negative correlation."),
                   tags$li("Analyze bubbles in the top-right for high correlation and explanatory power. These factors have the most significant impact.")
                 ),
                 p("Use this tool to quickly assess which economic factors have the most influence and how they correlate with the target demographic.")
               )
               )
             ),
             fluidRow(
               column(
                 width = 9,  
                 div(
                   class = "bubble-chart",
                   `data-step` = "10",
                   `data-intro` = "The bubble chart combines multiple aspects: size shows explanatory power, color indicates correlation direction (blue positive, red negative), and position shows correlation strength.",
                 plotlyOutput("bubbleChart", height = "600px")
               )
                 ),
               column(
                 width = 3,  # Adjust width as needed
                 div(
                   class = "bubble-legend",
                   `data-step` = "11",
                   `data-intro` = "Reference this legend to interpret the bubble sizes in the chart.",
                 plotOutput("legendPlot", height = "200px")
               )
               
             )
           )
           )
  ),
  
  ## Navbar 4: Americans Born in Australia vs Economic Factors
  tabPanel("4. Americans Born in Australia vs Economic Factors",
           value = "4",
           fluidPage(
             # Introduction text
             fluidRow(
               column(
                 width = 12,
                 class = "introduction-section3",
                 `data-step` = "1",
                 `data-intro` = "Welcome to the Economic Factors analysis section. Here we'll explore how various economic indicators influence Americans born in Australia.",
                 style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0;",
                 h3("Introduction"),
                 p("This analysis explores into the impact of various economic factors on Americans born in Australia who reside in the United States. Utilizing visual tools such as scatter plots, radar charts, and bubble charts, we aim to elucidate how economic indicators such as inflation, GDP growth, trade volumes, and stock market performance affect this unique demographic group."),
                 p("In the scatter plot section, you can investigate the specific relationships between selected economic factors and the U.S.-based Australian-born residents. This is followed by the radar chart, which offers a comprehensive view of the explanatory power of each economic factor, facilitating easy comparison across different variables. The bubble chart then provides deeper insights into how strongly each factor correlates with observed trends, with variations in size and color representing the relative influence and direction of these relationships."),
                 p("This interactive dashboard is designed to allow you to customize the visualizations displayed, providing a personalized exploration of how key economic factors influence the lives and circumstances of Americans born in Australia.")
               )
             ),
             # Spacer for separation between introduction and boxplot section
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 30px;")  # Adjust this height for spacing
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "scatterplot-explanation2",  
                   `data-step` = "2",
                   
                   `data-intro` = "This section explains how to interpret the scatterplots and their patterns.",
                 
                 h3("Economic Factors and Their Relationship on Americans Born in Australia"),
                 
                 p("Select the scatterplots displayed below to explore the relationship between various economic factors and Americans born in Australia who reside in the United States. 
                   This interactive feature allows you to choose multiple scatterplots to compare different economic indicators and observe their impact on this specific demographic group."),
                 p("You can choose multiple scatterplots to compare different economic indicators and observe their impact on the target demographic."),
                 p("If the scatterplot shows a ' / ' shape, this indicates a positive correlation between the economic factor and Australian born in united states."),
                 p("Conversely, if it displays a ' \\ ' shape, this suggests a negative correlation."), 
                 p("You can also hover over the points to view specific data points and see the statistics of the point"),
                 p("Analyse these patterns to interpret the relationship strength and direction between economic factors and the demographic.")
               )
               )
             ),
             
            
             # Scatterplot selection and display
             fluidRow(
               column(
                 width = 4,  
                 div(
                   class = "scatterplot-selection2",
                   `data-step` = "3",
                   `data-intro` = "Select different economic indicators to view their relationships",
                 checkboxGroupInput(
                   "scatterplots2",
                   "Select scatterplots to display:",
                   choices = list(
                     "ASX200 Yearly Return" = "asx_return",
                     "Export to USA" = "export_usa",
                     "Import from USA" = "import_usa",
                     "AUS Yearly Unemployment Rate" = "aus_unemployment",
                     "AUS GDP Growth" = "aus_gdp",
                     "AUS Inflation Rate" = "aus_inflation",
                     "USA Inflation Rate" = "usa_inflation",
                     "USA GDP Growth" = "usa_gdp",
                     "Export to AUS" = "export_aus",
                     "Import from AUS" = "import_aus",
                     "USA Unemployment Rate" = "usa_unemployment",
                     "S&P500 Yearly Return"= "sp500_return"
                   ),
                   selected = c("asx_return")
                   ),
                   selected = "asx_return"  # Default selection as a single item
                 )
                 
               ),
               column(
                 width = 8,  
                 div(
                   class = "scatterplot-display2",
                   `data-step` = "4",
                   `data-intro` = "The scatterplots show relationships between economic factors and population. A '/' shape indicates positive correlation, while a '\\' shape suggests negative correlation.",
                 uiOutput("scatterPlots2")  # Dynamic scatterplot output based on selected options
               )
               )
             ),
             
             # Spacer after scatterplots
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 50px;")  # height as needed for spacing
               )
             ),
             
             
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "radar-intro2", 
                   `data-step` = "5",
                   `data-intro` = "The radar chart helps visualize the explanatory power of each economic factor.",
                 h4("Economic Factors and Their Relationship on Americans Born in Australia"),
                 p("The radar chart below visualizes the explanatory power of each selected economic factor, allowing you to compare their influence on Americans Born in Australia residing in USA."),
                 p("Each axis represents an economic factor, and the distance from the center reflects the explanatory power. Factors with higher values exert greater influence on the target demographic."),
                 p("Use this chart to identify which economic factors have the strongest impact and observe the combined effect of selected variables."),
                 p("You have to select at least three variables you are interested in to generate the radar chart. Multiple selections are allowed.")
               )
               )
             ),
             
             
  
             
             
             # Radar Chart with variable selection
             fluidRow(
               column(
                 width = 4, 
                 div(
                   class = "radar-selection2",
                   `data-step` = "6",
                   `data-intro` = "Choose at least three variables to generate a radar chart. This will help you compare the influence of different economic factors.",
                 checkboxGroupInput(
                   "selected_variables2",
                   "Select variables for radar chart:",
                   choices = data_2$variable,  # Choices populated from dataset
                   selected = NULL  # Start with no variables selected
                 )
                 )
               ),
               column(
                 width = 8,
                 div(
                   class = "radar-display2",
                   `data-step` = "7",
                   `data-intro` = "The radar chart shows the explanatory power of each factor. Longer spokes indicate stronger",
                 plotOutput("radarPlot2", height = "900px")  # Radar chart output
               )
              )
             ),
             
             # Spacer after radar chart
             fluidRow(
               column(
                 width = 12,
                 div(style = "height: 50px;")  # Adjust the height as needed for spacing
               )
             ),
             
             # Bubble chart selection
             fluidRow(
               column(
                 width = 4, 
                 div(
                   class = "bubble-selection2",
                   `data-step` = "8",
                   `data-intro` = "Select which factors to display in the bubble chart. Each bubble represents an economic factor.",
                 checkboxGroupInput(
                   "bubble_selection2",
                   "Select bubbles to display:",
                   choices = data_2$variable,  # Choices populated from dataset
                   selected = data_2$variable  # Start with all bubbles selected
                 )
                 )
               ),
               column(
                 width = 8,
                 div(
                   class = "bubble-guide2",
                   `data-step` = "9",
                   `data-intro` = "This guide explains how to interpret the bubble chart visualization.",
                 h4("Bubble Chart Operation Guide"),
                 p("This bubble chart visualizes the relationship between explanatory power, correlation, and the absolute value of correlation for each economic factor."),
                 p("To use the chart:"),
                 tags$ul(
                   tags$li("Select the economic factors you want to display by checking the boxes. The bubbles represent the selected factors."),
                   tags$li("The bubble size indicates the explanatory power of each factor. Larger bubbles show a higher influence on Americans born in Australia."),
                   tags$li("The bubble color represents the correlation: Blue for positive correlation, red for negative correlation."),
                   tags$li("Analyze bubbles in the top-right for high correlation and explanatory power. These factors have the most significant impact.")
                 ),
                 p("Use this tool to quickly assess which economic factors have the most influence and how they correlate with the target demographic.")
               )
               )
             ),
             
             # Bubble chart and insights
             fluidRow(
               column(
                 width = 9, 
                 div(
                   class = "bubble-chart2",
                   `data-step` = "10",
                   `data-intro` = "The bubble chart combines multiple aspects: size shows explanatory power, color indicates correlation direction (blue positive, red negative), and position shows correlation strength.",
                 plotlyOutput("bubbleChart2", height = "600px")
               )
               ),
               column(
                 width = 3,  # Adjust width as needed
                 div(
                   class = "bubble-legend2",
                   `data-step` = "11",
                   `data-intro` = "Reference this legend to interpret the bubble sizes in the chart.",
                 plotOutput("legendPlot2", height = "200px")
               )
               )
             )
           )
  ),
  
 
  
  
  
  
  #navbar 5: Economic Indicators Correlation Analysis USA & Australia
  tabPanel("5. Economic Indicators Correlation Analysis USA & Australia",
           value = "5",
           fluidPage(
             # First fluid row for the introduction
             fluidRow(
               mainPanel(
                 div(
                   class = "correlation-intro",
                   `data-step` = "1",
                   `data-intro` = "Welcome to the Economic Indicators Correlation Analysis. This section explores the relationships between various economic metrics in the USA and Australia.",
                   
                   titlePanel("Economic Indicators Correlation Analysis: USA & Australia"),
                   h4("Understanding Cross-Market Economic Relationships"),
                   
                   div(
                     class = "correlation-description",
                     `data-step` = "2",
                     `data-intro` = "Here you'll find an explanation of what the visualization shows and how to interpret it.",
                     
                     p("This interactive visualization explores the intricate relationships between key economic 
                    indicators across the United States and Australian markets. The heatmap presents correlation 
                    coefficients between:"),
                     
                     div(
                       class = "correlation-metrics",
                       `data-step` = "3",
                       `data-intro` = "These are the main categories of economic indicators we'll be examining.",
                       
                       tags$ul(
                         tags$li("Market Performance (S&P500 and ASX200 Returns)"),
                         tags$li("Macroeconomic Indicators (GDP, Inflation, Unemployment)"),
                         tags$li("Trade Metrics (Imports and Exports)")
                       )
                     ),
                     
                     div(
                       class = "correlation-interpretation",
                       `data-step` = "4",
                       `data-intro` = "Learn how to interpret the colors in the heatmap and use the interactive features.",
                       
                       p("The color intensity indicates correlation strength, with dark blue showing strong positive 
                      correlations (+1.0) and dark red showing strong negative correlations (-1.0). This tool 
                      helps identify patterns and relationships between economic variables across these two 
                      major economies. You can also hover over each cell to view the exact correlation coefficient.")
                     )
                   )
                 )
               )
             ),
             
             # Second fluid row dedicated to the heatmap
             fluidRow(
               column(12,
                      div(
                        class = "heatmap-container",
                        `data-step` = "5",
                        `data-intro` = "Explore the correlation heatmap by hovering over cells to see exact values. Look for patterns of blue (positive) and red (negative) correlations between different economic indicators.",
                        plotlyOutput("heatmapPlot", height = "1000px")
                      )
               )
             )
           )
  ),
  
  # summary tab
  tabPanel("6. Summary and Insights",
           value = "6",
           fluidPage(
             fluidRow(
               column(12,
                      div(
                        class = "summary-header",
                        `data-step` = "1",
                        `data-intro` = "Welcome to the Summary and Insights section. Here we'll review the key findings from our analysis.",
                        h2("Summary and Insights", 
                           style = "text-align: center; margin-bottom: 30px;")
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      div(
                        class = "introduction-summary",
                        `data-step` = "2",
                        `data-intro` = "This section provides an overview of the entire analysis.",
                        h3("Introduction"),
                        p("This section provides a brief summary and insights on the change in population trends for Australians born in the USA and Americans born in Australia, as well as an analysis of various economic factors influencing these trends. The data highlights the intricate economic relationships and how these have evolved.")
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      div(
                        class = "population-changes",
                        `data-step` = "3",
                        `data-intro` = "Review the key findings about population changes before and after AUSFTA.",
                        h3("The Change of American and Australian Immigrant Population"),
                        p("Before the introduction of AUSFTA, the growth trends of Australians born in America and Americans born in Australia followed very similar patterns. Post-AUSFTA, both populations continued to grow, but the rate for Australians born in America outpaced that of Americans born in Australia.")
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      div(
                        class = "aus-factors",
                        `data-step` = "4",
                        `data-intro` = "Explore the economic factors affecting Australians born in the United States.",
                        h3("Australians Born in the United States vs. Economic Factors"),
                        h4("High Explanatory Power and High Positive Correlation:"),
                        tags$ul(
                          tags$li("Australia's imports from the USA"),
                          tags$li("Australia's exports to the USA"),
                          tags$li("USA's imports from Australia"),
                          tags$li("USA's exports to Australia")
                        ),
                        p("These trade metrics demonstrate a strong, positive relationship with the population of Australians born in the USA, indicating significant economic interdependence."),
                        
                        h4("Moderate Explanatory Power and Medium Negative Correlation:"),
                        tags$ul(tags$li("Australia's unemployment rate")),
                        p("This trend suggests that lower unemployment rates in Australia correlate with an increase in the population of Australians born in the USA, potentially indicating that better economic conditions may support immigration."),
                        
                        h4("Low Explanatory Power but Medium Correlation:"),
                        tags$ul(tags$li("GDP growth rates and inflation rates in both countries")),
                        p("These economic indicators have less impact on the population trends compared to trade metrics, although they still show some correlation.")
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      div(
                        class = "economic-indicators",
                        `data-step` = "5",
                        `data-intro` = "Review the correlation analysis between US and Australian economic indicators.",
                        h3("Economic Indicators Correlation Analysis between USA & Australia"),
                        h4("High Positive Correlation:"),
                        tags$ul(
                          tags$li("Cross-correlations between imports and exports for both countries"),
                          tags$li("Inflation rates between the two nations")
                        ),
                        
                        h4("High Negative Correlation:"),
                        p("Relationships between imports, exports, and unemployment rates in Australia, with negative impacts observed when US trade figures rise"),
                        
                        h4("Medium to Low Correlations:"),
                        tags$ul(
                          tags$li("GDP growth and unemployment rates show medium correlations, indicating synchronized economic cycles to some extent"),
                          tags$li("Lesser correlations are observed with inflation rates and volatility in stock market returns (S&P 500 vs. ASX 200)")
                        )
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      div(
                        class = "conclusion",
                        `data-step` = "6",
                        `data-intro` = "Final conclusions and implications of the analysis.",
                        h3("Conclusion"),
                        p("This analysis emphasizes the strong economic ties and interdependencies between the USA and Australia, particularly through trade. While trade significantly correlates with the immigrant populations between the two countries, other economic factors like GDP growth, inflation, and unemployment rates show varied impacts. This comprehensive assessment helps in understanding the economic and social dynamics influenced by and influencing immigration trends, providing valuable insights for policymakers, economists, and sociologists studying the economic drivers of immigration and their broader implications.")
                      )
               )
             )
           )),
  #Appendix
  tabPanel("7. Appendix",
           value = "7",
           fluidPage(
             fluidRow(
               column(12,
                      h2("Author", style = "margin-top: 20px;"),
                      p("This application was designed by Che-Yu Wu. To connect or learn more about my work, visit my LinkedIn profile: ",
                        tags$a(href = "https://www.linkedin.com/in/oliver-wu-aa40a7215", 
                               "Oliver Wu", 
                               target = "_blank")),
                      
                      h2("Data Sources", style = "margin-top: 30px;"),
                      p("The original data for this application was sourced from the following websites:"),
                      
                      tags$ul(
                        tags$li(
                          "Australian Bureau of Statistics, Historical Population: ",
                          tags$a(href = "https://www.abs.gov.au/statistics/people/population/historical-population/latest-release",
                                 "ABS Historical Population",
                                 target = "_blank")
                        ),
                        tags$li(
                          "Migration Policy Institute, Immigrants by Country of Birth Over Time: ",
                          tags$a(href = "https://www.migrationpolicy.org/programs/data-hub/charts/immigrants-countries-birth-over-time?width=900&height=850&iframe=true",
                                 "MPI Data Hub",
                                 target = "_blank")
                        ),
                        tags$li(
                          "Yahoo Finance, S&P 500 Index: ",
                          tags$a(href = "https://finance.yahoo.com/quote/%5EGSPC/",
                                 "Yahoo Finance",
                                 target = "_blank")
                        ),
                        tags$li(
                          "Investing.com, ASX 200 Index: ",
                          tags$a(href = "https://au.investing.com/indices/aus-200",
                                 "ASX 200 on Investing.com",
                                 target = "_blank")
                        ),
                        tags$li(
                          "World Bank, GDP Data: ",
                          tags$a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD",
                                 "World Bank GDP",
                                 target = "_blank")
                        ),
                        tags$li(
                          "World Bank, Inflation Data: ",
                          tags$a(href = "https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG",
                                 "World Bank Inflation",
                                 target = "_blank")
                        ),
                        tags$li(
                          "Federal Reserve Economic Data, Unemployment Rate: ",
                          tags$a(href = "https://fred.stlouisfed.org/series/UNRATE",
                                 "FRED Unemployment Rate",
                                 target = "_blank")
                        ),
                        tags$li(
                          "Australian Bureau of Statistics, Labour Force Australia: ",
                          tags$a(href = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release",
                                 "ABS Labour Force",
                                 target = "_blank")
                        ),
                        tags$li(
                          "UN Comtrade Database: ",
                          tags$a(href = "https://comtradeplus.un.org/",
                                 "UN Comtrade",
                                 target = "_blank")
                        )
                      ),
                      
                      h2("Cleaned Data for This Application", style = "margin-top: 30px;"),
                      p("For cleaned and processed datasets used in this application, access the following Google Drive folder: ",
                        tags$a(href = "https://drive.google.com/drive/folders/1ivHZHXxqy625T0EUZe1-8zJHw16wjWV-?usp=sharing",
                               "Cleaned Data Google Drive",
                               target = "_blank"))
               )
             )
           )),
  fluidRow(
    column(
      width = 8, offset = 2,  # This centers the content by using 8 columns with 2 column offset
      style = "margin-top: 40px;",  # Add some space from the content above
      sidebarLayout(
        sidebarPanel(
          width = 12,  # Make sidebar take full width of the column
          id = "navigation-panel",  # Add an ID
          class = "navigation-guide",  # Add a specific class
          
          tags$head(tags$style(HTML("
            .sidebar { 
              text-align: center; 
              background-color: #f8f9fa;
              border-radius: 8px;
              padding: 20px;
              box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            .action-button { 
              width: 100%; 
              margin-bottom: 10px;
              padding: 10px;
              text-align: left;
            }
            "))),
          h4("Where would you like to go next?", style = "margin-bottom: 20px;"),
          actionButton("toc_1", "1. Welcome", class = "btn-block"),
          actionButton("toc_2", "2. The Change Of American and Australian Immigrant Population", class = "btn-block"),
          actionButton("toc_3", "3. Australians Born in the United States vs Economic Factors", class = "btn-block"),
          actionButton("toc_4", "4. Americans Born in Australia vs Economic Factors", class = "btn-block"),
          actionButton("toc_5", "5. Economic Indicators Correlation Analysis: USA & Australia", class = "btn-block"),
          actionButton("toc_6", "6. Summary and Insights", class = "btn-block"),
          actionButton("toc_7", "7. Appendix", class = "btn-block")
        ),
        mainPanel(
          width = 0  # Hide the main panel since we're not using it
        )
      )
    )
  )
)



  
  
  
  
  
  
  
  


# Define server logic
server <- function(input, output, session) {

   
  
  # Function to initialize intro.js for tutorial
  observeEvent(input$toc_1, {
    updateNavbarPage(session, "nav", selected = "1")
    runjs("window.scrollTo(0, 0);")  # JavaScript to scroll to the top
  })
  observeEvent(input$toc_2, {
    updateNavbarPage(session, "nav", selected = "2")
    runjs("window.scrollTo(0, 0);")
  })
  observeEvent(input$toc_3, {
    updateNavbarPage(session, "nav", selected = "3")
    runjs("window.scrollTo(0, 0);")
  })
  observeEvent(input$toc_4, {
    updateNavbarPage(session, "nav", selected = "4")
    runjs("window.scrollTo(0, 0);")
  })
  observeEvent(input$toc_5, {
    updateNavbarPage(session, "nav", selected = "5")
    runjs("window.scrollTo(0, 0);")
  })
  observeEvent(input$toc_6, {
    updateNavbarPage(session, "nav", selected = "6")
    runjs("window.scrollTo(0, 0);")
  })
  observeEvent(input$toc_7, {
    updateNavbarPage(session, "nav", selected = "7")
    runjs("window.scrollTo(0, 0);")
  })
  
  observe({
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "steps" = list(
                list(
                  "element" = ".header",
                  "intro" = "Welcome to Global Perspectives! This application explores immigration patterns between the USA and Australia."
                ),
                list(
                  "element" = ".title-box",
                  "intro" = "This section introduces our main focus: exploring the relationship between economic factors and immigration."
                ),
                list(
                  "element" = ".introduction",
                  "intro" = "Here you'll find our research questions and key objectives for this analysis."
                ),
                list(
                  "element" = ".table-of-contents",
                  "intro" = "This table of contents provides an overview of all sections in our analysis."
                ),
                # Add navigation panel as final step only in welcome tab
                list(
                  "element" = ".sidebar",
                  "intro" = "There is a navigation panel on the bottom of each page to help you quickly jump to any section of the application. \n You can also click the top panels.",
                  "position" = "top"
                )
              )
            ))
  })
  
  # Keep tutorial button functionality
  observeEvent(input$startTutorial, {
    # Same steps as above
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "steps" = list(
                list(
                  "element" = ".header",
                  "intro" = "Welcome to Global Perspectives! This application explores immigration patterns between the USA and Australia."
                ),
                list(
                  "element" = ".title-box",
                  "intro" = "This section introduces our main focus: exploring the relationship between economic factors and immigration."
                ),
                list(
                  "element" = ".introduction",
                  "intro" = "Here you'll find our research questions and key objectives for this analysis."
                ),
                list(
                  "element" = ".table-of-contents",
                  "intro" = "This table of contents provides an overview of all sections in our analysis."
                ),
                list(
                  "element" = ".sidebar",
                  "intro" = "There is a navigation panel on the bottom of each page to help you quickly jump to any section of the application. \n You can also click the top panels.",
                  "position" = "top"
                )
              )
            ))
  })
  
  
  
  #tab 2
  observeEvent(input$nav, {
    if (input$nav == "2") {
      # First show the loading message
      showModal(modalDialog(
        title = "Welcome to Immigration Trends",
        "The visualizations on this page include animations and may take a few seconds to load. 
         The tutorial will begin after you click the start tutorial button.",
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "startPageTutorial",
            "Start Tutorial",
            class = "btn btn-primary",
            style = "background-color: #007bff; color: white;"
          ),
          modalButton("Skip Tutorial")
        )
      ))
    }
  })
  
  # Start tutorial after user clicks the start button in the modal
  observeEvent(input$startPageTutorial, {
    removeModal()
    Sys.sleep(0.5)  # Small delay before starting tutorial
    
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "steps" = list(
                list(
                  "element" = ".introduction-section",
                  "intro" = "This section introduces the impact of the Australia-United States Free Trade Agreement (AUSFTA) on immigration patterns between the two nations.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".radio-button-section",
                  "intro" = "Choose different views of the data: Pre-AUSFTA period, Post-AUSFTA period, or a comparison of both.",
                  "position" = "right"
                ),
                list(
                  "element" = "#boxPlot",
                  "intro" = "This boxplot visualizes the population changes for immigrants in both countries, divided into pre and post AUSFTA periods.",
                  "position" = "right"
                ),
                list(
                  "element" = ".text-section",
                  "intro" = "Here you'll find detailed analysis of the trends shown in the boxplot, including insights about migration patterns before and after AUSFTA.",
                  "position" = "left"
                ),
                list(
                  "element" = ".animation-description-section",
                  "intro" = "Below, you'll find an animated visualization showing how immigration patterns changed over time between the USA and Australia.",
                  "position" = "top"
                ),
                list(
                  "element" = "#animatedBarRace",
                  "intro" = "This animated bar race shows the dynamic changes in immigrant populations over time, highlighting key trends and shifts.",
                  "position" = "top"
                )
              )
            ))
  })
  #tab 2 end
  
  
  # Observer for tab 3
  observeEvent(input$nav, {
    if (input$nav == "3") {
      # Show welcome modal
      showModal(modalDialog(
        title = "Welcome to Economic Factors Analysis",
        HTML("This page contains multiple interactive visualizations including scatterplots, radar charts, and bubble charts. <br><br>
             Some visualizations may take a few moments to load fully. <br><br>
             Click 'Start Tutorial' for a guided tour of all features."),
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "startEconomicTutorial",
            "Start Tutorial",
            class = "btn btn-primary",
            style = "background-color: #007bff; color: white;"
          ),
          modalButton("Skip Tutorial")
        )
      ))
    }
  })
  
  # Start tutorial after user clicks the start button
  observeEvent(input$startEconomicTutorial, {
    removeModal()
    Sys.sleep(0.1)
    
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "showBullets" = TRUE,
              "showProgress" = TRUE,
              "steps" = list(
                list(
                  "element" = ".introduction-section2",
                  "intro" = "Welcome to the Economic Factors analysis section where we explore how various economic indicators influence Australians born in the United States.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".scatterplot-explanation",
                  "intro" = "This section explains how to interpret the scatterplots.",
                  "position" = "top"
                ),
                list(
                  "element" = ".scatterplot-selection",
                  "intro" = "Select different economic indicators to view their relationships with the population of Australians born in the United States. Multiple selection is allowed",
                  "position" = "right"
                ),
                list(
                  "element" = ".scatterplot-display",
                  "intro" = "The scatterplots show the relationships.",
                  "position" = "left"
                ),
                list(
                  "element" = ".radar-intro",
                  "intro" = "The radar chart helps visualize the explanatory power of each economic factor.",
                  "position" = "top"
                ),
                list(
                  "element" = ".radar-selection",
                  "intro" = "Choose at least three variables to generate a radar chart.",
                  "position" = "right"
                ),
                list(
                  "element" = ".radar-display",
                  "intro" = "The radar chart.",
                  "position" = "left"
                ),
                list(
                  "element" = ".bubble-selection",
                  "intro" = "Select which factors to display in the bubble chart.",
                  "position" = "right"
                ),
                list(
                  "element" = ".bubble-guide",
                  "intro" = "This guide explains how to interpret and operate the bubble chart visualization.",
                  "position" = "left"
                ),
                list(
                  "element" = ".bubble-chart",
                  "intro" = "The bubble chart.",
                  "position" = "left"
                ),
                list(
                  "element" = ".bubble-legend",
                  "intro" = "Legend for bubble sizes.",
                  "position" = "left"
                )
              )
            ))
  })
  
  #end observer tab 3
  
  # Observer for tab 4
  observeEvent(input$nav, {
    if (input$nav == "4") {
      # Show welcome modal
      showModal(modalDialog(
        title = "Welcome to Economic Factors Analysis",
        HTML("This page contains multiple interactive visualizations including scatterplots, radar charts, and bubble charts. <br><br>
             Some visualizations may take a few moments to load fully. <br><br>
             Click 'Start Tutorial' for a guided tour of all features."),
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "startEconomicTutorial2",
            "Start Tutorial",
            class = "btn btn-primary",
            style = "background-color: #007bff; color: white;"
          ),
          modalButton("Skip Tutorial")
        )
      ))
    }
  })
  
  #Start tutorial after user clicks the start button
  observeEvent(input$startEconomicTutorial2, {
    removeModal()
    Sys.sleep(0.1)
    
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "showBullets" = TRUE,
              "showProgress" = TRUE,
              "steps" = list(
                list(
                  "element" = ".introduction-section3",
                  "intro" = "Welcome to the Economic Factors analysis section where we explore how various economic indicators influence Americans born in Australia.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".scatterplot-explanation2",
                  "intro" = "This section explains how to interpret the scatterplots.",
                  "position" = "top"
                ),
                list(
                  "element" = ".scatterplot-selection2",
                  "intro" = "Select different economic indicators to view their relationships with the population of Americans born in Australia. Multiple selection is allowed",
                  "position" = "right"
                ),
                list(
                  "element" = ".scatterplot-display2",
                  "intro" = "The scatterplots show the relationships.",
                  "position" = "left"
                ),
                list(
                  "element" = ".radar-intro2",
                  "intro" = "The radar chart helps visualize the explanatory power of each economic factor.",
                  "position" = "top"
                ),
                list(
                  "element" = ".radar-selection2",
                  "intro" = "Choose at least three variables to generate a radar chart.",
                  "position" = "right"
                ),
                list(
                  "element" = ".radar-display2",
                  "intro" = "The radar chart.",
                  "position" = "left"
                ),
                list(
                  "element" = ".bubble-selection2",
                  "intro" = "Select which factors to display in the bubble chart.",
                  "position" = "right"
                ),
                list(
                  "element" = ".bubble-guide2",
                  "intro" = "This guide explains how to interpret and operate the bubble chart visualization.",
                  "position" = "left")
                ,
                list(
                  "element" = ".bubble-chart2",
                  "intro" = "The bubble chart.",
                  "position" = "left"
                ),
                list(
                  "element" = ".bubble-legend2",
                  "intro" = "Legend for bubble sizes.",
                  "position" = "left"
                )
              )
            ))
  
  })
  
  
  #tab 5 observer
  observeEvent(input$nav, {
    if (input$nav == "5") {
      showModal(modalDialog(
        title = "Welcome to Correlation Analysis",
        "This visualization shows the relationships between economic indicators using an interactive heatmap. The tutorial will guide you through its features.",
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "startCorrelationTutorial",
            "Start Tutorial",
            class = "btn btn-primary",
            style = "background-color: #007bff; color: white;"
          ),
          modalButton("Skip Tutorial")
        )
      ))
    }
  })
  
  # Tutorial steps
  observeEvent(input$startCorrelationTutorial, {
    removeModal()
    Sys.sleep(0.5)
    
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "showBullets" = TRUE,
              "showProgress" = TRUE,
              "steps" = list(
                list(
                  "element" = ".correlation-intro",
                  "intro" = "Welcome to the Economic Indicators Correlation Analysis. This section explores the relationships between various economic metrics in the USA and Australia.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".correlation-description",
                  "intro" = "Here you'll find an explanation of what the visualization shows and how to interpret it.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".correlation-metrics",
                  "intro" = "These are the main categories of economic indicators we'll be examining.",
                  "position" = "left"
                ),
                list(
                  "element" = ".correlation-interpretation",
                  "intro" = "Learn how to interpret the colors in the heatmap and use the interactive features.",
                  "position" = "left"
                ),
                list(
                  "element" = ".heatmap-container",
                  "intro" = "Explore the correlation heatmap by hovering over cells to see exact values. Look for patterns of blue (positive) and red (negative) correlations between different economic indicators. You
                  can also click on the cells to see the exact correlation value.",
                  "position" = "top"
                )
              )
            ))
  })
  
  #summary tab
  observeEvent(input$nav, {
    if (input$nav == "6") {
      showModal(modalDialog(
        title = "Welcome to Summary and Insights",
        "This section provides a comprehensive overview of our findings. The tutorial will guide you through each key insight.",
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            "startSummaryTutorial",
            "Start Tutorial",
            class = "btn btn-primary",
            style = "background-color: #007bff; color: white;"
          ),
          modalButton("Skip Tutorial")
        )
      ))
    }
  })
  
  # Tutorial steps
  observeEvent(input$startSummaryTutorial, {
    removeModal()
    Sys.sleep(0.5)
    
    introjs(session, 
            options = list(
              "nextLabel" = "Next →",
              "prevLabel" = "← Previous",
              "skipLabel" = "Skip",
              "doneLabel" = "Done",
              "tooltipPosition" = "auto",
              "tooltipClass" = "customTooltip",
              "highlightClass" = "customHighlight",
              "showBullets" = TRUE,
              "showProgress" = TRUE,
              "steps" = list(
                list(
                  "element" = ".summary-header",
                  "intro" = "Welcome to the Summary and Insights section. Here we'll review the key findings from our analysis.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".introduction-summary",
                  "intro" = "This section provides an overview of the entire analysis.",
                  "position" = "bottom"
                ),
                list(
                  "element" = ".population-changes",
                  "intro" = "Review the key findings about population changes before and after AUSFTA.",
                  "position" = "left"
                ),
                list(
                  "element" = ".aus-factors",
                  "intro" = "Explore the economic factors affecting Australians born in the United States.",
                  "position" = "left"
                ),
                list(
                  "element" = ".economic-indicators",
                  "intro" = "Review the correlation analysis between US and Australian economic indicators.",
                  "position" = "left"
                ),
                list(
                  "element" = ".conclusion",
                  "intro" = "Final conclusions and implications of the analysis.",
                  "position" = "top"
                )
              )
            ))
  })
  
  
  #summary end
  
  
  #boxplot operation start
  # Filter the data reactively based on user selection
  filtered_data <- reactive({
    if (input$plot_type == "Comparison") {
      combined_data  # Show all data for comparison
    } else {
      combined_data %>% filter(Period == input$plot_type)  # Show Pre-AUSFTA or Post-AUSFTA
    }
  })
  
  # Update plot title based on the selection
  plot_title <- reactive({
    if (input$plot_type == "Comparison") {
      "Immigrant Population Before and After AUSFTA"
    } else if (input$plot_type == "Pre-AUSFTA") {
      "Immigrant Population Before AUSFTA"
    } else {
      "Immigrant Population After AUSFTA"
    }
  })
  
  # Render the box plot based on user selection
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Period, y = Population, fill = Dataset)) +
      geom_boxplot() +
      labs(
        title = plot_title(),  # Dynamic plot title
        x = "Period",
        y = "Population",
        fill = "Migration Type"  # Custom legend title
      ) +
      scale_fill_manual(
        values = c("AU Born in US" = "red", "US Born in AU" = "royalblue"),  
        labels = c("AU Born in US" = "Australians born in USA", "US Born in AU" = "Americans born in Australia") 
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      ) +
      scale_y_continuous(labels = comma)
  })
  #boxplot operation end 
  
  
  #animation operation start
  output$animatedBarRace <- renderImage({
    # Path to save the animation
    outfile <- tempfile(fileext = ".gif")
    
    # Create the animated bar race using gganimate
    p <- ggplot(combined_data2, aes(x = reorder(Dataset, -Population), y = Population, fill = Dataset)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Population Over Time: {closest_state}", y = "Population", x = "Country") +
      scale_fill_manual(
        # Make sure these names EXACTLY match Dataset values
        values = setNames(
          c("red", "royalblue"),
          unique(combined_data2$Dataset)
        ),
        labels = c("Australians born in USA",
                   "Americans born in Australia")
      )+
      transition_states(Year, transition_length = 2, state_length = 1) +
      ease_aes('cubic-in-out')+
      theme_minimal() +
      # Increase sizes
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Title size
        axis.title.x = element_text(size = 16),  # X-axis label size
        axis.title.y = element_text(size = 16),  # Y-axis label size
        axis.text.x = element_text(size = 14),   # X-axis tick size
        axis.text.y = element_text(size = 14),   # Y-axis tick size
        legend.title = element_text(size = 14),  # Legend title size
        legend.text = element_text(size = 12)    # Legend text size
      )
      
    
    # Animate the plot with the correct parameters
    animation <- animate(p, nframes = 100, fps = 10, width = 1000, height = 400, renderer = gifski_renderer())
    
    # Save the animation as a gif
    anim_save(outfile, animation)
    
    # Return a list with the file information
    list(src = outfile, contentType = 'image/gif')
    
  }, deleteFile = TRUE) 
  
  #animation operation end
  
  
  
  #scatterplot operation start
  # Scatterplot output
  output$scatterPlots <- renderUI({
    plot_list <- list()  # Empty list to store plots
    
    if ("asx_return" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(width = 7, plotlyOutput("asx_return_plot", height = "800px"))
      )
    }
    
    if ("export_usa" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("export_usa_plot", height = "800px")
        )
      )
    }
    
    if ("import_usa" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("import_usa_plot", height = "800px")
        )
      )
    }
    
    if ("aus_unemployment" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("aus_unemployment_plot", height = "800px")
        )
      )
    }
    
    # Repeat the process for all scatterplots 
    if ("aus_gdp" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("aus_gdp_plot", height = "800px")
        )
      )
    }
    
    if ("aus_inflation" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("aus_inflation_plot", height = "800px")
        )
      )
    }
    
    if ("usa_inflation" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("usa_inflation_plot", height = "800px")
        )
      )
    }
    
    if ("usa_gdp" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("usa_gdp_plot", height = "800px")
        )
      )
    }
    
    if ("export_aus" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("export_aus_plot", height = "800px")
        )
      )
    }
    
    if ("import_aus" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("import_aus_plot", height = "800px")
        )
      )
    }
    
    if ("usa_unemployment" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("usa_unemployment_plot", height = "800px")
        )
      )
    }
    
    if ("sp500_return" %in% input$scatterplots) {
      plot_list[[length(plot_list) + 1]] <- fluidRow(
        column(
          width = 7,  
          plotlyOutput("sp500_return_plot", height = "800px")
        )
      )
    }
    # Return all selected plots with their corresponding text
    do.call(tagList, plot_list)
  })
  
  # Define individual scatterplots in server
  output$asx_return_plot <- renderPlotly({
    p <- ggplot(au_borninus, aes(x = ASX_Yearly_Return_Percentage, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs ASX200 Yearly Return Percentage",
           x = "ASX200 Yearly Return in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14))
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nASX200 Yearly Return Percentage",
          x = 0.5, y = 0.95, font = list(size = 18)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = au_borninus$ASX_Yearly_Return_Percentage, 
        y = predict(lm(Population ~ ASX_Yearly_Return_Percentage, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  output$export_usa_plot <- renderPlotly({
    p <- ggplot(au_borninus, aes(x = `Export to USA`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(title = "Australians Born in the United States vs Australia Exports to USA in USD",
           x = "Australia Exports to USA in USD",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma)
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nAustralia Exports to USA in USD",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia Exports to USA in USD", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = au_borninus$`Export to USA`, 
        y = predict(lm(Population ~ `Export to USA`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  
  output$import_usa_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `Import from USA`, y = Population,
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs Australia Imports from USA in USD",
           x = "Australia Imports from USA in USD",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 8),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nAustralia Imports from USA in USD",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "Australia Imports from USA in USD", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`Import from USA`, 
        y = predict(lm(Population ~ `Import from USA`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$aus_unemployment_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `AUS Yearly AVG Unemployment Rate`, y = Population,
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs Australia Yearly Unemployment Rate",
           x = "Australia Yearly Unemployment Rate in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nAustralia Yearly Unemployment Rate",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "Australia Yearly Unemployment Rate in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`AUS Yearly AVG Unemployment Rate`, 
        y = predict(lm(Population ~ `AUS Yearly AVG Unemployment Rate`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$aus_gdp_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = AUS_gdp_growth, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs Australia GDP Growth per Year",
           x = "Australia GDP Growth in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nAustralia GDP Growth per Year",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "Australia GDP Growth in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$AUS_gdp_growth, 
        y = predict(lm(Population ~ AUS_gdp_growth, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$aus_inflation_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `Australia Inflation Rate %`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs Australia Yearly Inflation Rate",
           x = "Australia Inflation Rate in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nAustralia Yearly Inflation Rate",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "Australia Inflation Rate in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`Australia Inflation Rate %`, 
        y = predict(lm(Population ~ `Australia Inflation Rate %`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$usa_inflation_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `USA Inflation Rate %`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs USA Yearly Inflation Rate",
           x = "USA Inflation Rate in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nUSA Yearly Inflation Rate",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "USA Inflation Rate in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`USA Inflation Rate %`, 
        y = predict(lm(Population ~ `USA Inflation Rate %`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$usa_gdp_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = USA_gdp_growth, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs USA Yearly GDP Growth",
           x = "USA GDP Growth in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nUSA Yearly GDP Growth",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "USA GDP Growth in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$USA_gdp_growth, 
        y = predict(lm(Population ~ USA_gdp_growth, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$export_aus_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `Export to AUS`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs USA Exports to Australia in USD",
           x = "USA Exports to Australia in USD",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 8),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nUSA Exports to Australia in USD",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "USA Exports to Australia in USD", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`Export to AUS`, 
        y = predict(lm(Population ~ `Export to AUS`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$import_aus_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `Import from AUS`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs USA Imports from Australia in USD",
           x = "USA Imports from Australia in USD",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 8),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nUSA Imports from Australia in USD",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "USA Imports from Australia in USD", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`Import from AUS`, 
        y = predict(lm(Population ~ `Import from AUS`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$usa_unemployment_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `USA Yearly AVG Unemployment Rate`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs USA Yearly Average Unemployment Rate",
           x = "USA Yearly Unemployment Rate in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nUSA Yearly Average Unemployment Rate",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "USA Yearly Unemployment Rate in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`USA Yearly AVG Unemployment Rate`, 
        y = predict(lm(Population ~ `USA Yearly AVG Unemployment Rate`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  output$sp500_return_plot <- renderPlotly({
    # Create the ggplot
    p <- ggplot(au_borninus, aes(x = `S&P500_Yearly_Return_Percentage`, y = Population, 
                                 text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
      theme_minimal() +
      labs(title = "Australians Born in the United States vs S&P500 Yearly Return Percentage",
           x = "S&P500 Yearly Return in %",
           y = "Australians Born in the United States") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.text.x = element_text(size = 14),  # Adjust x-axis text size 
        plot.title = element_text(hjust = 0.5, size = 16)  # Center title and size
      )
    
    # Convert ggplot to interactive plot using ggplotly
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Australians Born in the United States vs\nS&P500 Yearly Return Percentage",
          x = 0.5,  # Center the title
          y = 0.95,  # Adjust title position
          font = list(size = 16)  # Adjust font size
        ),
        margin = list(t = 80),  # Add margin to prevent the title from being cut off
        yaxis = list(
          title = list(text = "Australians Born in the United States", standoff = 20, font = list(size = 16))
        ),
        xaxis = list(
          title = list(text = "S&P500 Yearly Return in %", standoff = 20, font = list(size = 16))
        ),
        width = 600, height = 800  # Adjust plot size
      ) %>%
      add_trace(
        x = au_borninus$`S&P500_Yearly_Return_Percentage`,
        y = predict(lm(Population ~ `S&P500_Yearly_Return_Percentage`, data = au_borninus)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE  # Add regression line explicitly
      )
  })
  
  
  # Scatterplot output 1 end
  #radar plot 1 start
  
  # Render the radar chart interactively
  #radar plot 1 start
  output$radarPlot <- renderPlot({
    
    # Check if any variables are selected
    if (is.null(input$selected_variables) || length(input$selected_variables) < 3) {
      # Display a message if less than 3 variables are selected
      plot.new()
      text(0.5, 0.5, "Please select at least 3 variables to display the radar chart.", cex = 1.5)
      return()
    }
    
    # Subset data_1 based on selected variables
    selected_data <- data_1 %>%
      filter(variable %in% input$selected_variables)
    
    # Reset factor levels for 'variable'
    selected_data <- selected_data %>%
      mutate(variable = factor(variable, levels = variable))  # Levels in the order of appearance
    
    # Recalculate the angle for the selected variables
    N <- nrow(selected_data)
    selected_data <- selected_data %>%
      mutate(angle = 2 * pi * (as.numeric(variable) - 1) / N)
    
    # Calculate x and y coordinates
    selected_data <- selected_data %>%
      mutate(x = r_squared * sin(angle),
             y = r_squared * cos(angle))
    
    # Close the polygon by adding the first point at the end
    data_poly <- rbind(selected_data, selected_data[1, ])
    
    # Begin plotting
    ggplot() +
      # Add gridlines
      geom_polygon(data = data.frame(x = 100 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 100 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 75 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 75 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 50 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 50 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 25 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 25 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      # Add axis lines
      geom_segment(data = selected_data,
                   aes(x = 0, y = 0, xend = 105 * sin(angle), yend = 105 * cos(angle)),
                   color = "gray80") +
      # Add polygon
      geom_polygon(data = data_poly, aes(x, y), fill = "#69b3a2", alpha = 0.3) +
      geom_path(data = data_poly, aes(x, y), color = "#69b3a2", size = 1) +
      # Add points
      geom_point(data = selected_data, aes(x, y), color = "#69b3a2", size = 3) +
      # Add labels
      geom_text(data = selected_data,
                aes(x = 120 * sin(angle), y = 130 * cos(angle), label = variable),
                hjust = ifelse(sin(selected_data$angle) < 0, 1, 0),
                vjust = ifelse(cos(selected_data$angle) < 0, 1, 0.5),
                size = 6) +
      # Add R-squared values
      geom_text(data = selected_data,
                aes(x = 100 * sin(angle), y = 90 * cos(angle), label = sprintf("%.1f%%", r_squared)),
                size = 5, color = "#69b3a2", fontface = "bold") +
      # Add scale labels
      geom_text(aes(x = 0, y = 25, label = "25"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 50, label = "50"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 75, label = "75"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 100, label = "100"), size = 5, color = "gray50") +
      coord_fixed() +
      theme_void() +
      labs(title = "Explanatory Power (%) for Selected Factors",
           subtitle = "Percentage values indicate the magnitude of influence each selected factor.") +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 14)) +
      ylim(-150, 150) + xlim(-170, 170)
  })

  #radar plot 1 end
  # Bubble chart rendering
  output$bubbleChart <- renderPlotly({
    # Filter data based on user selection
    filtered_data <- data_1[data_1$variable %in% input$bubble_selection,]
    
    # Create the bubble chart
    p <- ggplot(filtered_data, aes(
      x = r_squared,
      y = abs_correlation,
      size = custom_size,
      color = correlation,
      text = paste(
        variable, "\n",
        "Explanatory Power:", r_squared, "%", "<br>",
        "Correlation:", correlation
      )
    )) +
      geom_point(alpha = 0.6, show.legend = TRUE) +
      scale_color_gradient2(
        low = "red",
        mid = "grey",
        high = "blue",
        midpoint = 0,
        limit = c(-1, 1),
        name = "Correlation"
      ) +
      scale_size_identity(name = "Explanatory Power", guide = "legend") + 
      labs(
        title = "Correlation vs Explanatory Power for Factors Affecting Australians born in USA",
        x = "Explanatory Power",
        y = "Absolute Value of Correlation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm")
      ) +
      # Define fixed limits for x and y axes
      xlim(0, 100) +
      ylim(0, 1)
    
    # Convert ggplot to plotly for interactive features
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 16)
        )
      )
  })
  
  
  # Bubble chart rendering end 
  
  
  
  legend_data <- data.frame(
    explanatory_power = factor(c("Weak", "Moderate", "Strong"), levels = c("Weak", "Moderate", "Strong")),
    size = c(5, 10, 15)
  )
  
  # Render the legend as a separate plot
  output$legendPlot <- renderPlot({
    ggplot(legend_data, aes(x = explanatory_power, y = 1, size = size)) +
      geom_point(shape = 21, fill = "grey", color = "black", alpha = 0.6) +
      scale_size_identity() +  # Use identity to directly control the size
      labs(title = "Explanatory Power Legend", x = NULL, y = NULL) +
      theme_void() +  # Remove all axes and backgrounds for a clean legend
      theme(
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
        
      ) +
      geom_text(aes(label = explanatory_power), vjust = -1.5, size = 5)  # Add text labels above points
  })
  
  
  
  
  
  
  
  
  
  
  
  
  # Scatterplot2 operation start
  output$scatterPlots2 <- renderUI({
    plot_list <- list()  # Initialize an empty list to store plots
    
    # Define each plot with conditionals for selected options
    if ("asx_return" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("asx_return_plot2", height = "800px"))
        )
      ))
    }
    
    if ("export_usa" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("export_usa_plot2", height = "800px"))
        )
      ))
    }
    
    if ("import_usa" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("import_usa_plot2", height = "800px"))
        )
      ))
    }
    
    if ("aus_unemployment" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("aus_unemployment_plot2", height = "800px"))
        )
      ))
    }
    
    # Repeat the above process for remaining options
    if ("aus_gdp" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("aus_gdp_plot2", height = "800px"))
        )
      ))
    }
    
    if ("aus_inflation" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("aus_inflation_plot2", height = "800px"))
        )
      ))
    }
    
    if ("usa_inflation" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("usa_inflation_plot2", height = "800px"))
        )
      ))
    }
    
    if ("usa_gdp" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("usa_gdp_plot2", height = "800px"))
        )
      ))
    }
    
    if ("export_aus" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("export_aus_plot2", height = "800px"))
        )
      ))
    }
    
    if ("import_aus" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("import_aus_plot2", height = "800px"))
        )
      ))
    }
    
    if ("usa_unemployment" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("usa_unemployment_plot2", height = "800px"))
        )
      ))
    }
    
    if ("sp500_return" %in% input$scatterplots2) {
      plot_list <- append(plot_list, list(
        fluidRow(
          column(width = 7, plotlyOutput("sp500_return_plot2", height = "800px"))
        )
      ))
    }
    
    # Return all selected plots as a tagList for proper rendering
    do.call(tagList, plot_list)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$asx_return_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = ASX_Yearly_Return_Percentage, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs ASX200 Yearly Return Percentage",
        x = "ASX200 Yearly Return (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nASX200 Yearly Return Percentage",
          x = 0.5, y = 0.95, font = list(size = 18)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$ASX_Yearly_Return_Percentage,
        y = predict(lm(Population ~ ASX_Yearly_Return_Percentage, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  output$export_usa_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `Export to USA`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs Australia Exports to USA",
        x = "Australia Exports to USA (USD)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nAustralia Exports to USA (USD)",
          x = 0.5, y = 0.95, font = list(size = 18)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia Exports to USA (USD)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`Export to USA`, 
        y = predict(lm(Population ~ `Export to USA`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  
  output$import_usa_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `Import from USA`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs Australia Imports from USA",
        x = "Australia Imports from USA (USD)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nAustralia Imports from USA (USD)",
          x = 0.5, y = 0.95, font = list(size = 18)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia Imports from USA (USD)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`Import from USA`, 
        y = predict(lm(Population ~ `Import from USA`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render Australia Yearly Unemployment Rate Plot
  output$aus_unemployment_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `AUS Yearly AVG Unemployment Rate`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs Australia Yearly Unemployment Rate",
        x = "Australia Yearly Unemployment Rate (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nAustralia Yearly Unemployment Rate",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia Yearly Unemployment Rate (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`AUS Yearly AVG Unemployment Rate`,
        y = predict(lm(Population ~ `AUS Yearly AVG Unemployment Rate`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render Australia GDP Growth per Year Plot
  output$aus_gdp_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = AUS_gdp_growth, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs Australia GDP Growth per Year",
        x = "Australia GDP Growth (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nAustralia GDP Growth per Year",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia GDP Growth (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$AUS_gdp_growth,
        y = predict(lm(Population ~ AUS_gdp_growth, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render Australia Yearly Inflation Rate Plot
  output$aus_inflation_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `Australia Inflation Rate %`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs Australia Yearly Inflation Rate",
        x = "Australia Inflation Rate (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nAustralia Yearly Inflation Rate",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "Australia Inflation Rate (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`Australia Inflation Rate %`, 
        y = predict(lm(Population ~ `Australia Inflation Rate %`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render USA Yearly Inflation Rate Plot
  output$usa_inflation_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `USA Inflation Rate %`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs USA Yearly Inflation Rate",
        x = "USA Inflation Rate (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nUSA Yearly Inflation Rate",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "USA Inflation Rate (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`USA Inflation Rate %`, 
        y = predict(lm(Population ~ `USA Inflation Rate %`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render USA Yearly GDP Growth Plot
  output$usa_gdp_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = USA_gdp_growth, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs USA Yearly GDP Growth",
        x = "USA GDP Growth (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nUSA Yearly GDP Growth",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "USA GDP Growth (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$USA_gdp_growth, 
        y = predict(lm(Population ~ USA_gdp_growth, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render USA Exports to Australia Plot
  output$export_aus_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `Export to AUS`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs USA Exports to Australia",
        x = "USA Exports to Australia (USD)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nUSA Exports to Australia (USD)",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "USA Exports to Australia (USD)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`Export to AUS`, 
        y = predict(lm(Population ~ `Export to AUS`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render USA Imports from Australia Plot
  output$import_aus_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `Import from AUS`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs USA Imports from Australia",
        x = "USA Imports from Australia (USD)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nUSA Imports from Australia (USD)",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "USA Imports from Australia (USD)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`Import from AUS`, 
        y = predict(lm(Population ~ `Import from AUS`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Render USA Yearly Average Unemployment Rate Plot
  output$usa_unemployment_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `USA Yearly AVG Unemployment Rate`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs USA Yearly Average Unemployment Rate",
        x = "USA Yearly Unemployment Rate (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nUSA Yearly Average Unemployment Rate",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "USA Yearly Unemployment Rate (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`USA Yearly AVG Unemployment Rate`, 
        y = predict(lm(Population ~ `USA Yearly AVG Unemployment Rate`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  output$sp500_return_plot2 <- renderPlotly({
    p <- ggplot(us_borninau, aes(x = `S&P500_Yearly_Return_Percentage`, y = Population, text = paste("Year:", Year))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Americans Born in Australia vs S&P500 Yearly Return",
        x = "S&P500 Yearly Return (%)",
        y = "Population of Americans Born in Australia"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme(
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Americans Born in Australia vs\nS&P500 Yearly Return (%)",
          x = 0.5, y = 0.95, font = list(size = 16)
        ),
        margin = list(t = 80),
        yaxis = list(title = list(text = "Population of Americans Born in Australia", standoff = 20, font = list(size = 16))),
        xaxis = list(title = list(text = "S&P500 Yearly Return (%)", standoff = 20, font = list(size = 16))),
        width = 600, height = 800
      ) %>%
      add_trace(
        x = us_borninau$`S&P500_Yearly_Return_Percentage`,
        y = predict(lm(Population ~ `S&P500_Yearly_Return_Percentage`, data = us_borninau)),
        mode = "lines", line = list(color = "red"), showlegend = FALSE
      )
  })
  
  # Scatterplot output 2 end
  #radar plot 2 start
  
  # Render the radar chart interactively
  #radar plot 2 start
  output$radarPlot2 <- renderPlot({
    
    # Check if at least 3 variables are selected
    if (is.null(input$selected_variables2) || length(input$selected_variables2) < 3) {
      plot.new()
      text(0.5, 0.5, "Please select at least 3 variables to display the radar chart.", cex = 1.5)
      return()
    }
    
    # Filter and prepare data based on selected variables
    selected_data2 <- data_2 %>%
      filter(variable %in% input$selected_variables2) %>%
      mutate(variable = factor(variable, levels = variable))
    
    # Calculate angles and coordinates
    N <- nrow(selected_data2)
    selected_data2 <- selected_data2 %>%
      mutate(
        angle = 2 * pi * (as.numeric(variable) - 1) / N,
        x = r_squared * sin(angle),
        y = r_squared * cos(angle)
      )
    
    # Close the polygon by repeating the first point
    data_poly_2 <- rbind(selected_data2, selected_data2[1, ])
    
    # Plot
    ggplot() +
      # Gridlines for radar chart
      geom_polygon(data = data.frame(x = 100 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 100 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 75 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 75 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 50 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 50 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      geom_polygon(data = data.frame(x = 25 * sin(seq(0, 2 * pi, length.out = 100)),
                                     y = 25 * cos(seq(0, 2 * pi, length.out = 100))),
                   aes(x, y), fill = NA, color = "gray80") +
      # Axes for each variable
      geom_segment(data = selected_data2,
                   aes(x = 0, y = 0, xend = 105 * sin(angle), yend = 105 * cos(angle)),
                   color = "gray80") +
      # Radar polygon
      geom_polygon(data = data_poly_2, aes(x, y), fill = "#69b3a2", alpha = 0.3) +
      geom_path(data = data_poly_2, aes(x, y), color = "#69b3a2", size = 1) +
      # Points for each variable
      geom_point(data = selected_data2, aes(x, y), color = "#69b3a2", size = 3) +
      # Variable names
      geom_text(data = selected_data2,
                aes(x = 120 * sin(angle), y = 130 * cos(angle), label = variable),
                hjust = ifelse(sin(selected_data2$angle) < 0, 1, 0),
                vjust = ifelse(cos(selected_data2$angle) < 0, 1, 0.5),
                size = 6) +
      # R-squared values
      geom_text(data = selected_data2,
                aes(x = 100 * sin(angle), y = 90 * cos(angle), label = sprintf("%.1f%%", r_squared)),
                size = 5, color = "#69b3a2", fontface = "bold") +
      # Scale labels
      geom_text(aes(x = 0, y = 25, label = "25"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 50, label = "50"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 75, label = "75"), size = 5, color = "gray50") +
      geom_text(aes(x = 0, y = 100, label = "100"), size = 5, color = "gray50") +
      coord_fixed() +
      theme_void() +
      labs(title = "Explanatory Power (%) for Selected Factors",
           subtitle = "Percentage values indicate the magnitude of influence each selected factor.") +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 14)) +
      ylim(-150, 150) + xlim(-170, 170)
  })
  
  
  #radar plot 2 end
  # Bubble chart rendering
  output$bubbleChart2 <- renderPlotly({
    
    # Filter data based on selected bubbles
    filtered_data2 <- data_2 %>% 
      filter(variable %in% input$bubble_selection2)
    
    # Plotting
    p <- ggplot(filtered_data2, aes(
      x = r_squared, 
      y = abs_correlation, 
      size = custom_size, 
      color = correlation, 
      text = paste(variable, "<br>Explanatory Power:", r_squared, "%", "<br>Correlation:", correlation)
    )) +
      geom_point(alpha = 0.6, show.legend = TRUE) +
      scale_color_gradient2(
        low = "red", mid = "grey", high = "blue", midpoint = 0, 
        limits = c(-1, 1), name = "Correlation"
      ) +
      scale_size_identity(name = "Explantory Power", guide="legend" )  +
      labs(
        title = "Correlation vs Explanatory Power for Factors Affecting Australians Born in USA",
        x = "Explanatory Power (%)",
        y = "Absolute Value of Correlation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm")
      ) +
      xlim(0, 100) +  # Adjust to full range of r_squared if necessary
      ylim(0, 1)      # Adjust to full range of abs_correlation if necessary
    
    # Convert ggplot to plotly and set up hover labels
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 16)
        ),
        title = list(
          text = "Correlation vs Explanatory Power for Factors Affecting Australians Born in USA",
          x = 0.5  # Center the title
        )
      )
  })
  
  
 
  
  # Render the legend as a separate plot
  output$legendPlot2 <- renderPlot({
    ggplot(legend_data, aes(x = explanatory_power, y = 1, size = size)) +
      geom_point(shape = 21, fill = "grey", color = "black", alpha = 0.6) +
      scale_size_identity() +  # Use identity to directly control the size
      labs(title = "Explanatory Power Legend", x = NULL, y = NULL) +
      theme_void() +  # Remove all axes and backgrounds for a clean legend
      theme(
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
        
      ) +
      geom_text(aes(label = explanatory_power), vjust = -1.5, size = 5)  # Add text labels above points
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #heatmap start
  output$heatmapPlot <- renderPlotly({
    # Convert the matrix into a long format for ggplot
    melted_cor <- melt(cor_matrix)
    
    # Replace periods with "\n" in variable names to create line breaks
    melted_cor$Var1 <- gsub("\\.", "\n", melted_cor$...1)
    melted_cor$Var2 <- gsub("\\.", "\n", melted_cor$variable)
    # Remove any NA factor levels explicitly
    melted_cor <- melted_cor[!is.na(melted_cor$...1) & !is.na(melted_cor$variable), ]
    # Define the desired order with "\n" added
    variable_order <- c(
      "Australia\nExports", "Australia\nImports", "Australia\nGDP", 
      "Australia\nInflation\nRate", "Australia\nUnemployment\nRate", 
      "ASX200\nReturn", 
      "USA\nExports", "USA\nImports", "USA\nGDP", 
      "USA\nInflation\nRate", "USA\nUnemployment\nRate",
      "SP500\nReturn"
    )
    
    
    p<-ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value, 
                              text = paste('Correlation:', round(value, 4)))) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name = "Correlation") +
      theme_minimal() + 
      theme(
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 8),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),  # Larger plot title
        legend.title = element_text(size = 16),  # Larger legend title
        legend.text = element_text(size = 14),  # Larger legend text
        plot.margin = margin(15, 15, 15, 15),
        aspect.ratio = 2*3
        
      ) +
      coord_fixed() +
      labs(title = "Correlation Heatmap between each Economic Indicator",
           x = "", y = "") +
      scale_x_discrete(drop = FALSE) +  # Ensures no factor levels are dropped
      scale_y_discrete(drop = FALSE)
  
    ggplotly(p, tooltip = "text")%>%
      style(hoverinfo = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 16)
        )
      )

    
    })
  
  
  
  }
  

# Run the app
shinyApp(ui = ui, server = server)
