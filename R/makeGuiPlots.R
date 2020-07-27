#' Make plots for the rCTM model
#' 
#' This function delivers a set of standard plots for the Marsh Equilibrium Cohort model.
#'
#' @param modelOutput A list of three data frames. Contains output of the function "runMemWithCohorts".
#' @param inputParms A list of model parameters passed from the GUI input.
#'
#' @importFrom dplyr mutate group_by summarize
#' @import ggplot2
#' 
#' @return a list of 6 plots similar to those in MEM 3.4 
#' 
#' @export
#'
makeGuiPlots <- function(modelOutput, inputParms){
  # calculate OM fraction for modelOutput$cohorts
  modelOutput$cohorts <- modelOutput$cohorts %>% 
    dplyr::mutate(om_fraction = (fast_OM+slow_OM)/cumCohortVol)
  
  # Compute average carbon stock for each year of the simulation
  cohorts <- modelOutput$cohorts %>%
    dplyr::mutate(carbon = fast_OM + slow_OM) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(carb = mean(carbon))
  
  # Compute the derivative so we can plot change in carbon stock over time (plot 6)
  cohorts <- cohorts %>%
    dplyr::mutate(dcdt = rep(NA, times = nrow(cohorts)))
  cohorts$dcdt[1:(nrow(cohorts) - 1)] <- cohorts$carb[2:nrow(cohorts)] - cohorts$carb[1:(nrow(cohorts) - 1)]
    #Note: no need to divide by timestep since the timestep for the model is one year.
  
  # Run predictedBiomass to create standing biomass vs elevation curve
    # First, generate a range of elevations as inputs for predictedBiomass
  min_elev <- floor(inputParms$zVegMin + 0.1*inputParms$zVegMin)
  max_elev <- floor(inputParms$zVegMax + 0.1*inputParms$zVegMax)
  biomass_curve <- data.frame(z = min_elev:max_elev)
  biomass_curve <- biomass_curve %>%
    dplyr::mutate(biomass = do.call(rCTM::predictedBiomass, args = list(z = biomass_curve$z, bMax = inputParms$bMax, zVegMax = inputParms$zVegMax, zVegMin = inputParms$zVegMin, zVegPeak = inputParms$zVegPeak)))
  
  ans <- list(
    plot1= #A plot of Standing biomass vs Marsh Elevation ---------------------
      ggplot(biomass_curve,
             aes(x=z, y=biomass)) +
      labs(x="Marsh Elevation (cm)", y="Standing Biomass (g/m2)") +
      geom_line(size = 1) +
      theme_gray(base_size = 13),
 
    plot2= #A plot of standing biomass vs time ---------------------------------
      ggplot(modelOutput$annualTimeSteps,
             aes(x=years, y=biomass)) +
      labs(x="time (yrs)", y="Standing Biomass (g/m2)") +
      geom_line(size = 1) +
      theme_gray(base_size = 13),
    
    plot3= #A plot of Depth vs Time --------------------------------------------
      ggplot(modelOutput$annualTimeSteps,
             aes(x=years, y=meanHighWater-surfaceElevation)) +
      labs(x="time (yrs)", y="Depth (cm below Mean High Water)") +
      geom_line(size = 1) +
      theme_gray(base_size = 13),
      
    plot4= #A plot of Marsh Elevation compared to Mean Sea Level over time ----
      ggplot(modelOutput$annualTimeSteps) +
      labs(x="time (yrs)", y="(cm NAVD)" ) +
      geom_line(aes(x=years, y=surfaceElevation, color = "Surface Elevation"), size=1) +
      geom_line(aes(x=years, y=meanSeaLevel, color = "Mean Sea Level"), size=1) +
      theme_gray(base_size = 13) +
      theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank()),
      
    plot5= #A plot of Carbon stock per volume vs depth -------------------------
      ggplot(modelOutput$cohorts,
             aes(x=(layer_top+layer_bottom/2), y=om_fraction)) +
      labs(x="Sediment Depth (cm)", y="Sediment Organic Matter (%)") +
      geom_smooth() +
      theme_gray(base_size = 13),
      
    plot6= #A plot of Marsh Accretion over time (change in carbon stock over time)
      ggplot(cohorts,
             aes(x=year, y=dcdt)) +
      labs(x="time (yrs)", y="Change in carbon Stock (g/m^2 year") +
      geom_line(size = 1) + 
      theme_gray(base_size = 13)
    )
  print(modelOutput$cohorts$dcdt)
  
  return(ans)
}
