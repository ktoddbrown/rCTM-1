#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Define server logic
shinyServer(function(input, output, session) {
  
# Load Initial ----------------------------------------------------------------------------
  #Make a reactive function to populate inputParms
  getParms <- reactive({
    inputParms <- list(startYear = input$dateRange[1],
                       endYear = input$dateRange[2],
                       relSeaLevelRiseInit = input$relSeaLevelRiseInit,
                       relSeaLevelRiseTotal = input$relSeaLevelRiseTotal,
                       initElv = input$initElev,
                       meanSeaLevel = input$meanSeaLevel,
                       meanSeaLevelDatum = input$meanSeaLevelDatum,
                       meanHighWater = input$meanHighWater,
                       meanHighHighWater = input$meanHighHighWater,
                       meanHighHighWaterSpring = input$meanHighHighWaterSpring,
                       suspendedSediment = input$suspendedSediment,
                       lunarNodalAmp = input$lunarNodalAmp,
                       bMax = input$bMax,
                       zVegMin = input$vegElevRange[1],
                       zVegMax = input$vegElevRange[2],
                       zVegPeak = input$zVegPeak,
                       plantElevationType = input$planeElevationType,
                       rootToShoot = input$rootToShoot,
                       rootTurnover = input$rootTurnover,
                       rootDepthMax = input$rootDepthMax,
                       shape = input$shape,
                       omDecayRate = input$omDecayRate,
                       recalcitrantFrac= input$recalcitrantFrac,
                       settlingVelocity = input$settlingVelocity,
                       omPackingDensity = input$omPackingDensity,
                       mineralPackingDensity = input$mineralPackingDensity,
                       rootPackingDensity = input$rootPackingDensity,
                       coreYear = input$coreYear,
                       coreDepth = input$coreDepth,
                       coreMaxs = 1:input$coreDepth,
                       coreMins = 1:input$coreDepth-1)
    #print(inputParms)
    return(inputParms)
  })
  
  
  # Run the function runMemWithCohorts with GUI inputs
  runSim <- eventReactive(input$run_sim, {
    do.call(rCTM::runMemWithCohorts, getParms())
  })
  
  # Open the plots tab when "Run Simulation" is clicked.
  observeEvent(input$run_sim, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Plots")
  })
  
# Check for NA buttons ------------------------------------------------------------------
  # observeEvent(input$NA_meanHighHighWater, {
  #   toggleState("meanHighHighWater")
  #   updateNumericInput(session, "meanHighHighWater", value = NA)
  # })
  # observeEvent(input$NA_meanHighHighWaterSpring, {
  #   toggleState("meanHighHighWaterSpring")
  #   updateNumericInput(session, "meanHighHighWaterSpring", value = NA)
  # })
  # observeEvent(input$NA_zVegPeak, {
  #   toggleState("zVegPeak")
  #   updateNumericInput(session, "zVegPeak", value = NA)
  # })
  
# Restore Inputs Button -----------------------------------------------------------------
  observeEvent(input$restore_inputs, {
    reset("physical_inputs")
    reset("biological_inputs")
    reset("optional_inputs")
  })
  
# Plots Tab -----------------------------------------------------------------------------
  # Run "makeGuiPlots"
  graphs <- eventReactive(input$run_sim, {
    modelOutput <- do.call(rCTM::runMemWithCohorts, getParms())
    inputParms <- getParms()
    rCTM::makeGuiPlots(modelOutput, inputParms)
  })  
  
  # Render Plots for UI window
  output$plot1 <- renderPlot({
    graphs()$plot1
  })
  output$plot2 <- renderPlot({
    graphs()$plot2
  })
  output$plot3 <- renderPlot({
    graphs()$plot3
  })
  output$plot4 <- renderPlot({
    graphs()$plot4
  })
  output$plot5 <- renderPlot({
    graphs()$plot5
  })
  output$plot6 <- renderPlot({
    graphs()$plot6
  })
  
# Animations Tab ------------------------------------------------------------------------
  #Run animateCohorts and render gif
  gif <- observeEvent(input$generate_gif, {
    modelOutput <- do.call(rCTM::runMemWithCohorts, getParms())
    do.call(rCTM::animateCohorts, args = list(cohorts = modelOutput$cohorts,
                                 scenario = modelOutput$annualTimeSteps,
                                 duration = 20))

  output$gif <- renderImage({
    # Return a list containing the filename
    list(src = "MEM-CTM-animated.gif",
         contentType = 'image/gif',
          width = 450,
          height = 450,
          alt = 'Click "Generate Animation". This gif may take a couple of minutes to render.'
    )}, deleteFile = TRUE)
  })
  
# Model Diagram Tab ---------------------------------------------------------------------  
  #Render Model diagram from an image file
  # output$model_diagram <- renderImage({
  #   # Return a list containing the filename
  #   list(src = "../www/model_diagram.png",
  #        contentType = 'image/png',
  #        width = 900,
  #        height = 500,
  #        alt = "Oops... something went wrong!"
  #   )}, deleteFile = FALSE)
  
  # Source diagram from ModelStructure.Rmd
   temp = tempfile(fileext=".R")
   knitr::purl(input = "../vignettes/ModelStructure.Rmd", output=temp)
   source(temp)
  
  output$model_diagram <- renderGrViz(model_diagram)
  
# Parameter Ranges Tab ------------------------------------------------------------------
  #Read csv as data table:
  parameters <- read.csv("../docs/MEM_Variables.csv")
  
  output$parameter_spreadsheet <- DT::renderDataTable({
    DT::datatable(parameters)})
    
# R Code for Parameterization Tab -------------------------------------------------------
  # Load the current input variables
  # *Don't change white space for this section*
  output$run_on_local <- renderText({
    c("# Make sure the rCTM library is installed
library(rCTM)",
      "
      
# Run rMEM with Cohorts
      runMemWithCohorts(startYear =", input$dateRange[1],
      ", endYear =", input$dateRange[2],
      ", relSeaLevelRiseInit =", input$relSeaLevelRiseInit,
      ", relSeaLevelRiseTotal =", input$relSeaLevelRiseTotal,
      ", initElv = ", input$initElev,
      ", meanSeaLevel =", input$meanSeaLevel,
      ", meanSeaLevelDatum =", input$meanSeaLevelDatum,
      ", meanHighWater =", input$meanHighWater,
      ", meanHighHighWater =", input$meanHighHighWater,
      ", meanHighHighWaterSpring =", input$meanHighHighWaterSpring,
      ", suspendedSediment =", input$suspendedSediment,
      ", lunarNodalAmp =", input$lunarNodalAmp,
      ", bMax =", input$bMax,
      ", zVegMin =", input$vegElevRange[1],
      ", zVegMax =", input$vegElevRange[2],
      ", zVegPeak =", input$zVegPeak,
      ", plantElevationType =", input$planeElevationType,
      ", rootToShoot =", input$rootToShoot,
      ", rootTurnover =", input$rootTurnover,
      ", rootDepthMax =", input$rootDepthMax,
      ", shape =", input$shape,
      ", omDecayRate =", input$omDecayRate,
      ", recalcitrantFrac =", input$recalcitrantFrac,
      ", settlingVelocity =", input$settlingVelocity,
      ", omPackingDensity =", input$omPackingDensity,
      ", mineralPackingDensity =", input$mineralPackingDensity,
      ", rootPackingDensity =", input$rootPackingDensity,
      ", coreYear =", input$coreYear,
      ", coreDepth =", input$coreDepth,
      ")",
      "

# look at the structure of the function output
str(memCohortExample)

# Look at the three tables making up the output
head(memCohortExample$annualTimeSteps)
head(memCohortExample$cohorts)
head(memCohortExample$core)

# run the animate cohorts function
# This will take a few minutes to run and then save an animation of accumulating cohorts
animateCohorts(cohorts=memCohortExample$cohorts,
               scenario=memCohortExample$annualTimeSteps,
               filename='MEM-Try_191212.gif')"
    )
  })
  
  
  
})