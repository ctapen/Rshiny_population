# UI
shinyUI(
  
  # Dashboard Page
  dashboardPage(
    
    # DashboardHeader
    dashboardHeader(
      title = "Population Insights",
      titleWidth = 200
    ),
    
    # DashboardSidebar
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        
        # Menu bars w/Tabs
        menuItem("Overview", tabName = "Overview", icon = icon("info"))
        , menuItem("Total Population", tabName = "Total_Pop", icon = icon("globe"))
        , menuItem("Life Expectancy", tabName = "Life_Expectancy_Boxplot", icon = icon("heartbeat"))
        , menuItem("Fertility Rate", tabName = "Fertility_Rate", icon = icon("leaf"))
        , menuItem("Babies Per Year", tabName = "Babies_Per_Year", icon = icon("child"))
        , menuItem("Age", tabName = "Age", icon = icon("calendar"),
                   menuSubItem(icon = NULL, "Youth Waterfall", tabName="youth_waterfall")
                   , menuSubItem(icon = NULL, "Age Roseplot", tabName="age_roseplot")
        )
      )
    ),
    
    # Dashboard Body
    dashboardBody(

      tabItems(
        
        # Overview
        tabItem(tabName = "Overview"
                , titlePanel("Overview")
                
                # Rundown box
                , box(title="Rundown / The Why", height=340
                      , "The future of population will shape how we create businesses, develop social services, grow our infrastructure, build institutions, provide health care, and the like.", tags$br(), tags$br(), tags$i("Understanding the future of population will help equip us to be prepared for opportunities and challenges ahead!"))
                
                # Findings box
                , box(title="Findings", height=340
                      , "1) Africa's population will take off.  Asia's will start declining ~2055.", tags$br(), tags$br()
                      , "2) While life expectancy remains lower in Africa, further improvements will increase population.", tags$br(), tags$br()
                      , "3) Fertility rates will significantly shape future population (Africa's higher rates).", tags$br(), tags$br()
                      , "4) Illustration of future: There are more babies born in a year in Nigeria than in all of Europe.", tags$br(), tags$br()
                      , "5) Nearly all of the world's youth reside in developing nations.  The world's population will age considerably.")
                
                # Contents box
                , box(title="Contents", height=270
                      , "This dashboard shows different facets of population projections, often broken down by continent or subregion.", tags$br(), tags$br(), tags$b("Topics include: "), tags$br(), "-total population", tags$br(), "-life expectancy", tags$br(), "-fertility rate", tags$br(), "-babies per year", tags$br(), "-age")
                
                # Data sources box
                , box(title="Data", height=270
                      , "Data Sources Include:", tags$br(), tags$br(), "-UN World Population Prospects 2017", tags$br(), tags$br(), "-Gapminder")
                )
        ,
        
        # Total Population
        tabItem(tabName = "Total_Pop"
                # Title
                , titlePanel("Aggregate Population Projections")
                
                # Selector
                , selectizeInput(inputId = "category",
                               label = "Select Category",
                               choices = unique(df_1_TotalPopSex_1$category[df_1_TotalPopSex_1$category != 'country_territory']))

                # Plot
                , plotOutput("pop_over_time")
                
                # Source
                , tags$br(), "Source: UN World Population Prospects"
                ),
                
        # Life Expectancy Boxplot
        tabItem(tabName = "Life_Expectancy_Boxplot"
                # Title
                , titlePanel("Life Expectancy")
                
                # Selector
                , selectizeInput(inputId = "year_life_expectancy",
                                 label = "Select Year",
                                 choices = unique(gapminder$year),
                                 selected=2007)
                
                # Plot
                , plotOutput("life_expectancy_boxplot")
                
                # Source
                , tags$br(), "Source: Gapminder"
                ),    
        
        # Fertility Rate
        tabItem(tabName = "Fertility_Rate"
                # Title
                , titlePanel("Fertility Rate, by Country")
                
                # Plot
                , htmlOutput("birth_rate") # used htmlOutput b/c gvis' syntax
                
                # Source
                , tags$br(), "Source: UN World Population Prospects"
                ),        
        
        # Babies Per Year
        tabItem(tabName = "Babies_Per_Year"
                # Title
                , titlePanel("Babies Per Year")
                
                # Plots and Selector
                , tabPanel("Babies Per Year"
                           , fluidRow(box(plotOutput("babies_born_per_year_bar_graph", height = 350), width=410))
                           )
                
                # Source
                , "Source: UN World Population Prospects; Year: 2020"
                ),
        
        # Youth
        tabItem(tabName = "youth_waterfall"
                # Title
                , titlePanel("Youth Waterfall by Continent")
                
                # Selector
                , sliderInput(inputId="year_youth_waterfall", label="Year:", min=1990, max=2100, value=2020, step=10, sep = "")
                
                #Plot
                , plotOutput("youth")
                
                # Source
                , tags$br(), "Source: UN World Population Prospects"
                ),
        
        # Age Grouping
        tabItem(tabName = "age_roseplot"
                , titlePanel("Age Group Changes from 1900 to 2100 (projected)")
                
                # Selector
                , selectizeInput(inputId = "category_age",
                                 label = "Select Category",
                                 choices = unique(df_1_TotalPopSex_1$category[!df_1_TotalPopSex_1$category %in% c('country_territory','development', 'subcontinent')]))

                # Plot
                , plotOutput("age_rose_plot")
                
                # Source
                , tags$br(), "Source: UN World Population Prospects"
                )
        
      # Close Tab Items
      )
    # Close dashboardBody
    )
  # Close dashboardPage
  )
# Close ShinyUI
)

