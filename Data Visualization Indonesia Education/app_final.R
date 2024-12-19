#--------------------------------------------------------------------------
# THIS BLOCK OF CODE IS FOR PREPARING PACKAGES
#--------------------------------------------------------------------------

# Required packages
required_packages = c('shiny', 'shinydashboard', 'shinythemes', 'plotly', 'readr', 'dplyr', 
                      'scales', 'ggplot2', 'leaflet', 'sf', 'tigris')

# Checking if package is already installed and install the rest
for (package in required_packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

library(shiny) #To make interactive visualization
library(shinydashboard) #To show the visualization on a dashboard
library(shinythemes) #To import themes to the dashboard
library(plotly) #To make an interactive visualization
library(readr) #To read help read CSV file
library(dplyr) #To provide a basic function for data manipulation
library(scales) #To help scaling the tables so that ggplot2 can visualize the plot
library(ggplot2) #To visualize the plot 
library(leaflet) #To visualize the map
library(leaflet.extras) #To provide extra plugins for the leaflet library
library(sf) #To encode spatial vector data
library(tigris) #To allows users to directly download and use TIGER/Line shapefiles 

#-------------------------------------------------------------------------
# THIS BLOCK OF CODE IS FOR PREPARING THE DATA 
#--------------------------------------------------------------------------

#source https://rpubs.com/nurussadad/Peta-Choropleth
#This block of code is to prepare the shp and long,lat data to be merged later
Admin2 = read_sf('Data/idn_admbnda_adm2_bps_20200401.shp') %>%
  filter(ADM1_EN == 'Jawa Barat')

#To load the longitude and latitude for West Java provided by the link
JabarIndonesia<-read.csv("https://raw.githubusercontent.com/Nr5D/BatasWilayahID/main/JabarIndonesia.csv",
                         header=TRUE, sep=";")

merged_JabarIndonesia <- geo_join(spatial_data=Admin2, 
                                  data_frame=JabarIndonesia, by_sp="ADM2_PCODE", 
                                  by_df="ADM2_PCODE", how = "inner")

#This block of code to prepare the data and merged it with the map data
csv_data <- read.csv('Data/cleandata2.csv')

merged_data = left_join(merged_JabarIndonesia, csv_data, by = c('ADM2_EN.x' = 'nama_kabupaten_kota'))

#--------------------------------------------------------------------------
# THIS BLOCK OF CODE IS FOR PREPARING PACKAGES source : Lecture Notes
#--------------------------------------------------------------------------

#This code is to make ui and set the themes
ui = fluidPage(
  theme = shinytheme('yeti'), 
  
  #To make the navbar
  navbarPage(
    title = "Indonesia Education Measured by Quantitative Values", #Setting title of the navbar
    #To make the Home tab
    tabPanel("Home",
             mainPanel(width = 12,
               h3("What’s the Condition of Education in Indonesia? A Closer Look at Its Quantitative Aspect from Jawa Barat Region"),
               br(),
               p("5 months ago, on January 10th, 2023, there was an interview with Nadiem Anwar Makarim with Gita Wirjawan  that discusses education in Indonesia. 
                 One of the topics Gita Wirjawan mentioned is how low Indonesia’s PISA score (Programme for International Student Assessment). There are three components 
                 in PISA which are reading literacy, mathematics, and science. In 2018, Indonesia only ranked 74th for literacy, 73rd for mathematics, and 71st for science 
                 among the other 79 countries that were being measured. From this score alone, it’s clear that Indonesia’s education quality is not the best. "),
               br(),
               p("One important aspect of education is whether a child can pursue education which is measured by the Education Index. Education Index is measured by 
                 two components which are,"),
               HTML("<ul>
                      <li>Expected Years of Schooling Index (EYSI) which is the expected years a student is enrolled.</li>
                      <li>Mean Years of Schooling Index (MYSI) which is the average number of years a student over the age of 25 has actually received. </li>
                      </ul>"),
               br(),
               p("Previous research at Selayar Island has compared Education Index with teacher-to-student ratio and they found out that on their region, teacher-to-student 
                 ratio didn’t have any impact to Education Index. Although a research conducted in Las Vegas USA mentioned that a good teacher-to-student ratio will have a 
                 positive impact on student education. Finally, an article from IDN times mentioned that there’s a downward trend on the graduation rate across Indonesia’s 
                 education levels, because of that the question we hoped to answer from this visualization are."),
               br(),
               HTML("<ul>
                      <li>Does the teacher-to-student ratio compared to the Education Index pattern reflect the conclusion derived from the research?</li>
                      It’s known that the student-to-teacher ratio impacts the quality of education received by the students, but research showed there’s no connection between them. 
                      This question is aimed to see if that pattern holds true in the West Java region.
                      <li>What about the distribution of the teacher-to-student ratio for each region?</li>
                      According to the research conducted in Las Vegas, a good student-to-teacher ratio will provide a better-quality education for the students. (Koc 2015) This 
                      question is aimed to see does Indonesia have a good teacher-to-student ration in region across West Java
                      <li>What is the pattern of the total of students across the education levels? Does it have a downward trend for all regions in West Java?</li>
                      As mentioned above, the graduation rate of students in Indonesia is decreasing for each education level. (Zarawaki, 2023) This question is aimed to see if 
                      the pattern is true by looking at it from the trend of total students across each education level for 5 years.
                    </ul>")
               )
             ),
    #To make the Education Index tab
    tabPanel("Education Index",
             #To make the sidebar, that have the users input
             sidebarPanel(width = 3,
               h3("Education Index"),
               p("A good R squared value is at least 0.10-0.50"),
               selectizeInput("ip_level_1","Select Education Level",
                              choices = c("SMP", "SMA", "SMK"),
                              multiple = TRUE, options = list(plugins = list('remove_button'))
               )
             ),
             #To make the main panel that have the visualization and narrative
             mainPanel(
               plotlyOutput('plot1'),
               box(width =12, p("	From the scatter plot, the cluster tend to make a constant horizontal 
                                line indicating a no correlation between Education Index and Teacher to Student Ratio. "),
               br(),
               p("R^2 value was counted and even though there are some true outliers that cannot be deleted, the highest R^2 
                 given is still 0.0025. The valid value is at least between 0.10 and 0.50 if some or most explanatory variables are statistically significant."),
               br(),
               p("Therefore, it looks like there’s no relationship between Education Index and Teacher to Student Ratio in West Java region. .")
               )
               )
             ),
    #To make the tab panel of Map of Teacher to Student Ratio
    tabPanel("Map of Teacher to Student Ratio",
             #To make the sidebar that have the users input
             sidebarPanel(width = 3,
               h3("Map of Teacher to Student Ratio"),
               HTML("<ul>
                    <li> < 15 will be beneficial to the students </li>
                    <li> 15 to 20 is not bad </li>
                    <li> >20 will have negative impact </li>
                    </ul>"),
               radioButtons("peta_kelas_2", "Education Level",
                            choices = c("SMP", "SMA", "SMK"),
                            selected = "SMP"),
               selectizeInput("peta_tahun_2", "Year",
                              choices = c(2015, 2016, 2017,2018, 2019))
             ),
             #To make the main panel that have the visualization and narrative
             mainPanel(
               leafletOutput('plot2'),
               br(),
               p("The trend for the teacher to student ratio across all student level and years is still not ideal 
                 for 2015 – 2019.  Most of the regions have a bad teacher to student ratio of over 20 which is 195 
                 while the acceptable range which is 15 to 20 is 155. The ideal ratio which is below 15 is only 40 from 390 which is approximately 10%")
             )),
    #To make the tab panel of Total Students trend
    tabPanel("Trend for Total Students",
             #To make the sidebar that have the users input
             sidebarPanel(
               h3("Total Student by Education Levels"),
               radioButtons('button_kelas_3',
                            label = 'Bar Chart Total Student',
                            choices = c("ALL","SD VS SMP/SMA/SMK","SMP VS SMA/SMK","SMA VS SMK")),
               h3("Line Chart for Total Students Trend"),
               selectizeInput("trend_kelas_3", "Kelas",
                              label = 'Line Chart Total Student',
                            choices = c("SD", "SMP", "SMA","SMK"),
                            multiple = TRUE, options = list(plugins = list('remove_button'))
               )
             ),
             #To make the main panel that have the visualization and narrative
             mainPanel(
               plotlyOutput('plot3')),
               br(),
             
             fluidRow(
               p("From the bar chart, even though there’s a big difference between SD and other classification. 
                 It must be remembered that SD (elementary education) is 6 years while SMP/SMK/SMA is 3 years. 
                 If we compare the total of student of SD to the combination of SMP/SMK/SMA they’re about the same. 
                 This is also true when you compare SMP to the combination of SMK/SMA and finally SMK to SMA."),
               plotOutput('plot4'),
               p("From the line chart with the data transformed to logarithmic. It is found that the trend for 
                all educational level mostly after 2017 goes in a straight line for all regions. So there’s no 
                downward trend for all education levels, it all even out.")
             )
             ),
    #To make the tab panel for conclusion
    tabPanel("Conclusion",
             #To make the main panel that have the narrative
             mainPanel(
               h3("So the conclusion gathered from this visualizations are: "),
               HTML("<ul>
                 <li>Corresponding to the two research, there’s no connection between Education Index and Teacher to Student ratio at SMP, SMK, and SMA level even in West Java region.</li>
                 <li>The trend for the teacher to student ratio across all student level and years is still not ideal for 2015 – 2019.  Most of the regions have a bad teacher to student ratio of over 20 which is 195 while the acceptable range which is 15 to 20 is 155. The ideal ratio which is below 15 is only 40 from 390 which is approximately 10%</li>
                 <li>Although the bar chart initially shows that there’s a decrease in the number of student following each education level. If you compare it considering the year that each education level is supposed to go on. The number of students is actually about the same. </li>
                 <li>This is also shown from the line chart that across all education levels, the graph even out. From 2017 almost all region across all education levels, the line goes straight so the trend of the number of students stays the same. So quantitatively, there is no problem from the number of students. </li>
                 <ul>")
             )
             )
    )
  )

#--------------------------------------------------------------------------
# THIS BLOCK OF CODE IS MAKING THE SERVER LOGIC source : Lecture Notes
#--------------------------------------------------------------------------

#To prepare making the server logic
server <- function(input, output) {
  
  #--------------------------------------------------------------------------
  # THIS BLOCK OF CODE IS MAKING THE 1st Plot a scatter plot
  #--------------------------------------------------------------------------  
  output$plot1 <- renderPlotly({
    
    #To enable user input and wrangle the data
    if(!isTruthy(input$ip_level_1)){
      selected_data = csv_data %>%
        filter(level != 'sd')
    } else {
      selected_data = csv_data %>%
        filter(level %in% tolower(input$ip_level_1))
    }
    
    
    #To make the plot based on the user input according to the education level
    m = lm(ratio ~ indeks.pendidikan, selected_data)
    #To make the formula for the r^2 
    plot1_label = paste0('y = ', round(coef(m)[2], 2), 'x + ', round(coef(m)[1], 2),
                         '\nr^2 = ', round(summary(m)$r.squared, 4))
    #To 
    plot1 = ggplot(data = selected_data, aes(x = indeks.pendidikan, y = ratio,  
                                             text =paste0('Region: ', nama_kabupaten_kota,
                                                          '\nRatio: ', round(ratio, 2),
                                                          '\nEducation Index: ', indeks.pendidikan,
                                                          '\nYear: ', tahun_ajaran,
                                                          '\nLevel: ', level))) + 
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE) +
      geom_text(x = 70, y =0.8*max(selected_data$ratio), label = plot1_label) + 
      labs(x = 'Education Index', y = 'Teacher Student Ratio')
    
    ggplotly(plot1, tooltip = 'text')
    
    
  })

  #--------------------------------------------------------------------------
  # THIS BLOCK OF CODE IS MAKING THE 2nd Plot a choropleth map source https://r-graph-gallery.com/choropleth-map.html
  #--------------------------------------------------------------------------
  
  #To enable user input and wrangle the data
  output$plot2 <- renderLeaflet({
    merged_data2 = merged_data %>%
      filter(level == tolower(input$peta_kelas_2) & tahun_ajaran == input$peta_tahun_2)
    
    #Preparing the Red Green Yellow color
    mybins = c(0, 15, 20, Inf)
    palette_2 = colorBin(palette = c("green", "yellow", "red"), domain = 'merged_data2$ratio', na.color = 'transparent', bins = mybins)
    
    
    # To prepare the text for hover over the region
    text_2 = paste(
      'Region: ', merged_data2$ADM2_EN.y, '<br/>',
      'Ratio: ', round(merged_data2$ratio,1), '<br/>',
      sep = '') %>%
      lapply(htmltools::HTML)
    
    # To make the label on the bottom right
    label_2 = c(paste('≤ 15 ( Total = ', sum(csv_data$ratio<15, na.rm=TRUE), ')'),
                paste('15 - 20 ( Total = ', sum(csv_data$ratio>15 & csv_data$ratio<20, na.rm=TRUE), ')'),
                paste('≥ 20 ( Total = ', sum(csv_data$ratio>20, na.rm=TRUE), ')'))
    
    # To create the choropleth based on the user input
    leaflet(merged_data2) %>%
      addTiles() %>%
      setView(lat = -7, lng = 108, zoom = 7.2) %>%
      
      #To add color to the choropleth and border lines
      addPolygons(
        fillColor = ~palette_2(ratio), stroke = TRUE,fillOpacity = 1,
        color = 'black',
        weight = 1,
        label = text_2,
        labelOptions = labelOptions(
          style = list('font-weight' = 'normal', padding = '2px 10px'),
          textsize = '14px',
          direction = 'auto'
        )) %>%
      #To add the legend for the map
      addLegend(pal = palette_2, values = ~ratio,
                opacity = 1, title = 'Teacher to Student Ratio',
                position = 'bottomright',
                labFormat = function(type, cuts, p){
                  paste0(label_2)
                })
    
    
  })
  
  #--------------------------------------------------------------------------
  # THIS BLOCK OF CODE IS MAKING THE 3rd and 4th Plot a stacked-bar chart and Line chart
  #--------------------------------------------------------------------------
  
  output$plot3 <- renderPlotly({

    #To enable user input and wrangle the data
    if(input$button_kelas_3 == 'ALL'){
      students_1 = csv_data %>%
        group_by(level) %>%
        summarise(jumlah_murid = sum(jumlah_murid, na.rm= TRUE))
      students_1$agg_level = students_1$level
      plot_data3 = students_1
    } else if(input$button_kelas_3 == 'SD VS SMP/SMA/SMK') {
      students_2 = csv_data %>%
        group_by(level) %>%
        summarise(jumlah_murid = sum(jumlah_murid, na.rm= TRUE))
      students_2$agg_level = ifelse(students_2$level == 'sd', 'SD', 'SMP-SMA-SMK')
      plot_data3 = students_2
    } else if (input$button_kelas_3 == 'SMP VS SMA/SMK'){
      students_3 = csv_data %>%
        group_by(level) %>%
        summarise(jumlah_murid = sum(jumlah_murid, na.rm= TRUE)) %>%
        filter(level != 'sd')
      students_3$agg_level = ifelse(students_3$level == 'smp', 'SMP', 'SMA-SMK')
      plot_data3 = students_3
    } else if (input$button_kelas_3 == 'SMA VS SMK'){
      students_4 = csv_data %>%
        group_by(level) %>%
        summarise(jumlah_murid = sum(jumlah_murid, na.rm= TRUE)) %>%
        filter(level %in% c('sma','smk'))
      students_4$agg_level = ifelse(students_4$level == 'smk', 'SMK', 'SMA')
      plot_data3 = students_4
    }
    
    #To make the stacked bar chart based on radio button selection
    plot3 = ggplot(data = plot_data3, aes(x = agg_level, y = jumlah_murid, fill = level, 
                                       text =paste0('Total Students: ', (jumlah_murid)))) + 
      geom_bar(stat = "identity")+
      labs(x = "Education Levels", y = "Total Students", title = "Chart for Trend of the Total Students") +
      theme_minimal()
    
    ggplotly(plot3, tooltip = 'text')
  })
  
  output$plot4 <- renderPlot({
    #To enable user input
    if(!isTruthy(input$trend_kelas_3)){
      plot_data4 = csv_data
    } else {
      plot_data4 = csv_data %>%
        filter(level %in% tolower(input$trend_kelas_3))
    }
    
    #To make the line chart
    plot4 = ggplot(data = plot_data4, aes(x = tahun_ajaran, y = log(jumlah_murid), 
                                          group = nama_kabupaten_kota, 
                                          level, 
                                          color = level,  
                                          text =paste0('Total Student: ', jumlah_murid,
                                                        '\nYear: ', tahun_ajaran,
                                                        '\nEducation Level: ', level))) +
      
      geom_line()+
      geom_path()+
      labs(x = "Year", y = "Total Student", title = "Total Student / Year")
    
    plot4
  })
}

#To run the program
shinyApp(ui,server)

