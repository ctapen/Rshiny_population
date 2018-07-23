function(input, output, session) {
  
  # Pop Over Time
  output$pop_over_time <- renderPlot(
    ggplot(df_1_TotalPopSex_1 %>% 
             filter(category==input$category) %>% 
             group_by(year, Location, projection_flag, category) %>% 
             summarise(max=max(numeric_pop))
           , aes(x=year, y=max, group=Location, colour=Location)) + 
      # Solid line for historical data
      geom_line(data = filter(filter(df_1_TotalPopSex_2_Filtered1,category==input$category)
                              , !projection_flag), aes(colour=Location)) + 
      # Dashed line for projected data
      geom_line(data = filter(filter(df_1_TotalPopSex_2_Filtered1,category==input$category)
                              , projection_flag), aes(colour=Location), linetype="dashed") +
      y_axis_billions + # reformat Y axis to commas from sci notation
      x_axis_ticks + # only show certain x axis ticks
      labs(x='Year', y='Population', title='Population Over Time') +
      theme(plot.title = element_text(face="bold", hjust = 0.55)
            , axis.title=element_text(size=10, face="bold")
            , axis.title.x=element_text(hjust = 0.53)
            )
  )
  
  # Babies Born Per Year
  output$babies_born_per_year_bar_graph <- renderPlot(
    babies_born_per_year_bar_graph
  )
  
  # Rose Chart
  output$age_rose_plot <- renderPlot(
    ggplot(df_2_PopAgeSex_1 %>% 
             filter(category==input$category_age&year %in% years_rose) %>% 
             group_by(year, Location, age_grouping) %>% 
             summarise(numeric_pop=sum(numeric_pop))
           , aes(x=year, y=numeric_pop, fill=age_grouping, group=Location)) +
      geom_bar(stat="identity", position = "fill") + 
      coord_polar() + 
      scale_fill_manual(values = c("#FFFFAA", "#deebf7", "#3182bd"), # to assign colors to stacked bars
                        name = "Age Grouping") + # to change legend title 
      facet_wrap( ~ Location) + 
      theme(strip.text = element_text(face="bold"), # to bold facet titles
            axis.title = element_text(face="bold"),
            plot.title = element_text(face="bold", hjust=.6),
            legend.title = element_text(face="bold")) + 
      labs(x='Year', y='Population Proportion'
           , title="Age Groups' Population Proportion Across Time")
  )
  
  # Life Expectancy
  output$life_expectancy_boxplot <- renderPlot(
    ggplot(gapminder %>% filter(year == input$year_life_expectancy)
           , aes(x = continent, y = lifeExp, fill = continent)) + 
      geom_boxplot() + 
      labs(x="Continent", y="Life Expectancy (years)", fill="Continent"
           , title="Life Expectancy by Continent") +
      scale_fill_brewer(palette="YlGnBu") +
      theme(plot.title = element_text(face="bold", hjust = 0.55)
            , axis.title=element_text(size=10, face="bold")
            , legend.position="bottom")
  )  
  
  # Youth Waterfall
  output$youth <- renderPlot(
    ggplot(df_2_PopAgeSex_3 %>% filter(year==input$year_youth_waterfall)
           , aes(Location, fill = category)) + 
      geom_rect(aes(x = Location, xmin = id - 0.35, xmax = id + 0.35, ymin = end, ymax = start)) +
      #y_axis_billions +
      labs(y='Population Under 25 (in billions)'
           , title="Youth Population (under 25), by Location") +
      theme(axis.title = element_text(face="bold"),
            plot.title = element_text(face="bold", hjust=.6),
            legend.title = element_text(face="bold")) +
      scale_fill_manual(values = c("#add8e6", "#3182bd")) +
      geom_text(aes(id, end
                    , label = percent(round((numeric_pop/1000000000)/(df_2_PopAgeSex_3 %>% filter(year==input$year_youth_waterfall,Location=="World"))[["numeric_pop_waterfall"]], 2))), vjust = -0.3, size = 3)
  )
  
  # Birth Rate
  output$birth_rate <- renderGvis( # b/c googlevis
    gvisGeoChart(df_5_TotalFertility_1
                 , "Location", "birth_rate"
                 , options=list(width=500, height=400
                                , colorAxis="{colors:['#ffe9ec', 'green']}"
                                ))
         
  )
}
