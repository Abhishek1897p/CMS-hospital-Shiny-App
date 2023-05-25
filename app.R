#required library

library(shiny)
library(DT)         # for data tables 
library(fmsb)       # for radar plot 
library(miscTools)  # for insert row function
library(shinythemes)# for shiny theme


# all the data set required for our app.
# these the same files we exported from final project.Rmd

##############################################################################

final_app <- read.csv("final.csv", stringsAsFactors = FALSE)

final_general_app <- read.csv("final_general.csv", stringsAsFactors = FALSE)

Star_Rating_final_app <- read.csv("Star_Rating_final.csv", stringsAsFactors= FALSE)

Star_Rating_final_general_app <- read.csv("Star_Rating_final_general.csv", stringsAsFactors = FALSE)


##############################################################################

#pivot files

cad_p_app <- read.csv("cad_p.csv", stringsAsFactors = FALSE)

hai_p_app <- read.csv("hai_p.csv", stringsAsFactors = FALSE)

hcahps_p_app <- read.csv("hcahps_p.csv", stringsAsFactors = FALSE)

mhs_p_app <- read.csv("mhs_p.csv", stringsAsFactors = FALSE)

oie_p_app <- read.csv("oie_p.csv", stringsAsFactors = FALSE)

pvc_p_app <- read.csv("pvc_p.csv", stringsAsFactors = FALSE)

tec_p_app <- read.csv("tec_p.csv", stringsAsFactors = FALSE)

##############################################################################

# unique measure id and name files

cad_u_app <- read.csv("cad_u.csv", stringsAsFactors = FALSE)

hai_u_app <- read.csv("hai_u.csv", stringsAsFactors = FALSE)

hcahps_u_app <- read.csv("hcahps_u.csv", stringsAsFactors = FALSE)

mhs_u_app <- read.csv("mhs_u.csv", stringsAsFactors = FALSE)

oie_u_app <- read.csv("oie_u.csv", stringsAsFactors = FALSE)

pvc_u_app <- read.csv("pvc_u.csv", stringsAsFactors = FALSE)

tec_u_app <- read.csv("tec_u.csv", stringsAsFactors = FALSE)

################################################################################

# mean aggregate table files

cad_a_app <- read.csv("cad_a.csv", stringsAsFactors = FALSE)

hai_a_app <- read.csv("hai_a.csv", stringsAsFactors = FALSE)

hcahps_a_app <- read.csv("hcahps_a.csv", stringsAsFactors = FALSE)

mhs_a_app <- read.csv("mhs_a.csv", stringsAsFactors = FALSE)

oie_a_app <- read.csv("oie_a.csv", stringsAsFactors = FALSE)

pvc_a_app <- read.csv("pvc_a.csv", stringsAsFactors = FALSE)

tec_a_app <- read.csv("tec_a.csv", stringsAsFactors = FALSE)

# app ui

ui <- fluidPage(
  # App theme used can we changed
  theme = shinytheme("flatly"),                 
  
  # App header or name
  h1(id = "big_head" ,"Find Your Hospital"),    
  
  # these are all the html style we have used in our app to make it a bit colorful
  tags$style(HTML("#big_head{
                                color: red;
                                font-size:45px;
                                font-style: bold;
                                text-align: center; }
              
                  #infobar{
                                color: blue;
                                font: Areal
                                font-style: bold;
                                text-align: left;
                                font-size:30px; }

                  #infonext{    
                                color: #09bd48;
                                font: Areal
                                font-style: bold;
                                text-align: left;
                                font-size:20px; }
                                
                  #dict{ 
                                font-size:20px;
                                font-family:Times New Roman;
                                margin-bottom:10px; }
                  
                  label { 
                                font-size:15px; 
                                font-family:Times New Roman; 
                                margin-bottom:10px; } 

                  "
  )),
  
  # info about the app below name
  p(id = "infobar", "About this App"),
  h3(id = "infonext", p("The app will help you find your hospital by Zip code. 
                        It will provide you General information  as well as indepth 
                        information about the hospitals. There are 9 tabs with specific 
                        information about the hospital.")),
  
  # this is were sidebar components are added
  sidebarLayout(
    
    # first sidebar panel
    sidebarPanel(
      
      # this is a drop down menu to select the zip code
      selectInput("id", "Select your zip code", choices = sort(unique(final_app$ZIP.Code))),
      
      # This is another drop down to select rating between 1-5 and look at the average 
      # score of each measure for that rating
      selectInput(
        inputId = "rid",
        label = "Select star rating to look at its group average score (1-5)",
        choices = c("1", "2", "3", "4", "5")
      ),
      hr(),
      
      # information about where the data has been taken from
      h4(helpText("Data taken from Centers for Medicare & Medicaid Services (USA)")),
      uiOutput("tab"),
      
      # our main radar plot which shows measure values for the hospitals in the 
      # selected zip code area.
      plotOutput('Gen_radar', click = "plot_click1"),
      # this will show you the distance from center to help you compare
      verbatimTextOutput("info1"),
      
      # infobar about what the above graph means and how to use it
      p(id ="infobar", "Information"),
      h3(id = "infonext", p("The above data will show, what is the average score for each measure in the selected star raiting."), 
         p("Compare this with the hospital of your choice. which will help you understand, how well the
                          hospital of your choice performs in different measure areas when compared to the group's average. ")),
      
      # Maker info
      p(HTML("Made by <br/> Abhishek Devidas Pawar <br/> abhishekdpawar276@gmail.com")),
      
      
    ),
    
    # this is the central area called main panel
    # this is where we show our output
    mainPanel(
      
      # navigation bar to make it look clean
      # contains many tab
      navbarPage( 
        title = "Hospital Information for selected ZIP code",    
        
        # first tab showing basic info about hospital in area selected
        tabPanel( 
          "Basic information",
          h4(p("Sr.No---------------Hospital Name ----------------- Rating")),
          
          # this will pull all the hospital in the given area.
          verbatimTextOutput("hospital"),
          
          # rating wise radar plot
          plotOutput('radarPlot', click = "plot_click"),
          verbatimTextOutput("info"),
          
          # data dictionary 
          p(id = "infobar", "Data Dictionary"),
          h4( id = "dict",
              HTML("CAD => Complications and Deaths <br />"),
              HTML("HAI => Healthcare Associated Infections <br /> "),
              HTML("MHS => Medicare Hospital Spending Per Patient <br />"),
              HTML("OIE => Outpatient Imaging Efficiency <br />"),
              HTML("PVC => Payment and Value of Care <br />"),
              HTML("TEC => Timely and Effective Care <br />"),
              HTML("HCAHPA => Hospital Consumer Assessment of Healthcare Providers and Systems <br />")
          ),
          
        ),
        
        # panel 2
        tabPanel(
          "Important information",
          h4("Emergency Service Available"),
          verbatimTextOutput("emr"),
          h4("Address"),
          verbatimTextOutput("add"),
          h4("City"),
          verbatimTextOutput("city"),
          h4("Phone Number"),
          verbatimTextOutput("phn"),
          h4("Hospital Ownership"),
          verbatimTextOutput("own"),
          
        ),
        
        
        
        # all other panels 
        tabPanel("HCAHPS information",
                 h4(HTML(
                   "HCAHPA => Hospital Consumer Assessment of Healthcare Providers and Systems <b>(Higher is better)(1 is low , 5 is high)</b>")),
                 DT::dataTableOutput("myhcatable")),
        tabPanel("CAD information",
                 h4(HTML("CAD => Complications and Deaths <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("mycadtable")),
        tabPanel("HAI information",
                 h4(HTML("HAI => Healthcare Associated Infections <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("myhaitable")),
        tabPanel("MHS information",
                 h4(HTML("MHS => Medicare Hospital Spending Per Patient <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("mymhstable")),
        tabPanel("OIE information",
                 h4(HTML("OIE => Outpatient Imaging Efficiency <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("myoietable")),
        tabPanel("PVC information",
                 h4(HTML("PVC => Payment and Value of Care <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("mypvctable")),
        tabPanel("TEC information",
                 h4(HTML("TEC => Timely and Effective Care <b>(LOWER THAN AVERAGE IS BETTER)</b>")),
                 DT::dataTableOutput("mytectable"))
      )
    ))
)

# Server side code

server <- function(input, output) {
  
  # output functions for all unique measure id and names for each measure 
  
  ################################################################################  
  
  # CAD  
  output$mycadtable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, cad_p_app[which(cad_p_app$Facility.ID == fid[row]), 2:20])
      cad_u_app <-
        insertCol(as.matrix(cad_u_app), row + 2 + c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(cad_a_app[which(cad_a_app$Hospital.overall.rating == rat[row]), 2:20],2))
      cad_u_app <-
        insertCol(as.matrix(cad_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    cad_u_app
  })
  
  # HAI 
  
  output$myhaitable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, hai_p_app[which(hai_p_app$Facility.ID == fid[row]), 2:7])
      
      hai_u_app <-
        insertCol(as.matrix(hai_u_app), row + 2 +c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(hai_a_app[which(hai_a_app$Hospital.overall.rating == rat[row]), 2:7],2))
      hai_u_app <-
        insertCol(as.matrix(hai_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    hai_u_app
  })
  
  # HCAHPS
  
  output$myhcatable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, hcahps_p_app[which(hcahps_p_app$Facility.ID == fid[row]), 2:12])
      
      hcahps_u_app <-
        insertCol(as.matrix(hcahps_u_app), row + 2, v, cName = nam[row])
      
    }
    
    hcahps_u_app
  })  
  
  # MHS
  
  output$mymhstable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, mhs_p_app[which(mhs_p_app$Facility.ID == fid[row]), 2])
      
      mhs_u_app <-
        insertCol(as.matrix(mhs_u_app), row + 2+c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(mhs_a_app[which(mhs_a_app$Hospital.overall.rating == rat[row]), 2],2))
      mhs_u_app <-
        insertCol(as.matrix(mhs_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    mhs_u_app
  })
  
  # OIE
  
  output$myoietable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, oie_p_app[which(oie_p_app$Facility.ID == fid[row]), 2:4])
      
      oie_u_app <-
        insertCol(as.matrix(oie_u_app), row + 2+c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(oie_a_app[which(oie_a_app$Hospital.overall.rating == rat[row]), 2:4],2))
      oie_u_app <-
        insertCol(as.matrix(oie_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    oie_u_app
  })   
  
  # PVC
  
  output$mypvctable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, pvc_p_app[which(pvc_p_app$Facility.ID == fid[row]), 2:5])
      
      pvc_u_app <-
        insertCol(as.matrix(pvc_u_app), row + 2+c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(pvc_a_app[which(pvc_a_app$Hospital.overall.rating == rat[row]), 2:5], 2))
      pvc_u_app <-
        insertCol(as.matrix(pvc_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    pvc_u_app
  })
  
  
  # TEC
  
  output$mytectable = DT::renderDataTable({
    n <- input$id
    fid <- c()
    fid <- c(fid, final_app[which(final_app$ZIP.Code == n), 2])
    nam <- c()
    nam <- c(nam, final_app[which(final_app$ZIP.Code == n), 3])
    rat <- c()
    rat <- c(rat, final_app[which(final_app$ZIP.Code == n), 13])
    c <- 0
    for (row in 1:length(fid)) {
      v <- c()
      v <-
        c(v, tec_p_app[which(tec_p_app$Facility.ID == fid[row]), 2:17])
      
      tec_u_app <-
        insertCol(as.matrix(tec_u_app), row + 2+c, v, cName = nam[row])
      
      v <- c()
      v <-
        c(v, round(tec_a_app[which(tec_a_app$Hospital.overall.rating == rat[row]), 2:17],2))
      tec_u_app <-
        insertCol(as.matrix(tec_u_app), row + 3 + c, v, cName = " <= Average Score")
      c <- c + 1
    }
    
    tec_u_app
  })  
  
  
  # output for Important info tab
  # Address
  
  output$add <- reactive({
    n <- input$id
    addr <- final_app[which(final_app$ZIP.Code == n), 4]
    hospital_e <- c()
    len <- length(addr)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ->", addr[row])
        hospital_e <- c(hospital_e, k)
      }
    }
    
    return(hospital_e)
  })
  
  # CITY
  
  output$city <- reactive({
    n <- input$id
    cit <- final_app[which(final_app$ZIP.Code == n), 5]
    hospital_e <- c()
    len <- length(cit)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ->", cit[row])
        hospital_e <- c(hospital_e, k)
      }
    }
    
    return(hospital_e)
  })
  
  # Phone Number
  
  output$phn <- reactive({
    n <- input$id
    addr <- final_app[which(final_app$ZIP.Code == n), 8]
    hospital_e <- c()
    len <- length(addr)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ->", addr[row])
        hospital_e <- c(hospital_e, k)
      }
    }
    
    return(hospital_e)
  })
  
  # Hospital Ownership Info
  
  output$own <- reactive({
    n <- input$id
    addr <- final_app[which(final_app$ZIP.Code == n), 10]
    hospital_e <- c()
    len <- length(addr)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ->", addr[row])
        hospital_e <- c(hospital_e, k)
      }
    }
    
    return(hospital_e)
  })
  
  # Emergency service
  
  output$emr <- reactive({
    n <- input$id
    addr <- final_app[which(final_app$ZIP.Code == n), 11]
    hospital_e <- c()
    len <- length(addr)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ->", addr[row])
        hospital_e <- c(hospital_e, k)
      }
    }
    
    return(hospital_e)
  })
  
  
  # hospitals in the selected ZIP code
  
  output$hospital <- reactive({
    hospital <- c()
    n <- input$id
    myhospital <- final_app[which(final_app$ZIP.Code == n), 3]
    rating <- final_app[which(final_app$ZIP.Code == n), 13]
    len <- length(myhospital)
    if (len != 0) {
      for (row in 1:len) {
        k <- paste("\n", row, ". ----", myhospital[row], ". ---- ", rating[row])
        hospital <- c(hospital,k)
      }
      return(hospital)
    }
  })
  
  # radar plot for hospitals in the selected zip code
  
  output$radarPlot <- renderPlot({
    
    # dataframe to store basic hospital info
    data <- data.frame(matrix(ncol = 7, nrow = 0))
    
    
    colnames(data) <-
      c("CAD", "HAI", "HACAPS", "MHS", "OIE", "PVC", "TEC")
    nam <- c()
    
    # going through each hospital in the area
    for (row in 1:length(input$id)) {
      n <- input$id
      v <- c()
      nam <- c()
      
      nam <-
        c(nam, final_general_app[which(final_general_app$ZIP.Code == n), 3])
      
      
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 14])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 15])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 16])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 17])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 18])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 19])
      v <-
        c(v, final_general_app[which(final_general_app$ZIP.Code == n), 20])
      
      
      data <- insertRow(as.matrix(data), 1, v)
      
    }
    
    rownames(data) <- c(nam)
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data = rbind(rep(1000, 7) , rep(0, 7) , data)
    
    data <- as.data.frame(data)
    
    # color for plot
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
    colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
                  rgb(0.8, 0.2, 0.5, 0.4) ,
                  rgb(0.7, 0.5, 0.1, 0.4))
    
    # actual plot
    radarchart(
      data  ,
      axistype = 5 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      #custom the grid
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 1, 5),
      cglwd = 0.8,
      #custom labels
      vlcex = 0.8
    )
    legend(
      x = 1.1,
      y = 1,
      legend = rownames(data[-c(1, 2),]),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.5,
      pt.cex = 4
    )
  })
  
  # output for showing your selected location distance from center (main radar plot)  
  output$info <- renderText({
    x <- input$plot_click$x
    y <- input$plot_click$y
    d <- ((x*x)+(y*y))*10
    paste0("Score at select point = ", d)
  })
  
  # rating wise radar plot
  output$Gen_radar <- renderPlot({
    n <- input$rid
    v <- c()
    nam <- c()
    
    nam <-
      c(nam, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 2])
    
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 3])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 4])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 5])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 6])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 7])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 8])
    v <-
      c(v, Star_Rating_final_general_app[which(Star_Rating_final_general_app$Hospital.overall.rating == n), 9])
    
    data <- data.frame(matrix(ncol = 7, nrow = 0))
    
    colnames(data) <-
      c("CAD", "HAI", "HACAPS", "MHS", "OIE", "PVC", "TEC")
    
    data <- insertRow(as.matrix(data), 1, v)
    rownames(data) <- c(nam)
    
    # limits for plot
    data = rbind(rep(1000, 7) , rep(0, 7) , data)
    
    data <- as.data.frame(data)
    
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
    colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
                  rgb(0.8, 0.2, 0.5, 0.4) ,
                  rgb(0.7, 0.5, 0.1, 0.4))
    
    radarchart(
      data  ,
      axistype = 1 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      #custom the grid
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 1, 5),
      cglwd = 0.8,
      #custom labels
      vlcex = 0.8
    )
    legend(
      x = 0.7,
      y = 1,
      legend = rownames(data[-c(1, 2),]),
      bty = "n",
      pch = 20 ,
      col = colors_in ,
      text.col = "grey",
      cex = 1.2,
      pt.cex = 3
    )
    
    # output for showing your selected location distance from center (sidebar radar plot)  
    output$info1 <- renderText({
      x <- input$plot_click1$x
      y <- input$plot_click1$y
      d <- ((x*x)+(y*y))*10
      paste0("Score at select point = ", d)
    })
    
  })
  
  url <- a("CMS hospital Compare", href="https://data.cms.gov/provider-data/topics/hospitals")
  output$tab <- renderUI({
    tagList("source:", url)})
}

shinyApp(ui = ui, server = server)