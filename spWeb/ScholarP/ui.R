if (interactive()) {

htmlTemplate("www/index.html")

htmlTemplate("www/biography.html")

htmlTemplate("www/citesPerArea.html")

htmlTemplate("www/citingAuthors.html")

htmlTemplate("www/collaborators.html")

htmlTemplate("www/docDetails.html")

htmlTemplate("www/download.html")

htmlTemplate("www/journals.html")

htmlTemplate("www/personalImpact.html")

htmlTemplate("www/publications.html")

htmlTemplate("www/publishers.html")

htmlTemplate("www/templateStyles.html")

dateRangeInput("pubWindow", "Ventana de Publicación:",
               start  = "2001-01-01",
               end    = "2010-12-31",
               min    = "1950-01-01",
               max    = "2022-12-21",
               format = "mm/dd/yy",
               separator = " - ")

htmlTemplate("www/authorProfile.html",
             collTable = tableOutput("collTable") %>%
               tagAppendAttributes(class = "lateralTable"),
             jourTable = tableOutput("jourTable") %>%
               tagAppendAttributes(class = "lateralTable"),
             docTableBody = htmlOutput("docTableBody"),
             citesPlot = plotlyOutput("citesPlot"),
             plot1 = plotly("plot1"),
             plot2 = plotly("plot2"),
             citingAuthTable = tableOutput("citingAuthTable") %>%
               tagAppendAttributes(class = "lateralTable")
             
)

}