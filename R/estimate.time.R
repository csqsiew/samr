#' The estimate.time() function figures out, given a specified decay rate, the number of time steps
#' required for total activation in the network to decrease to a specified proportion of its value at
#' t = 0.
#'
#' @param decay Proportion of activation that is lost at each time step, ranges from 0 to 1.
#' @param final_proportion Proportion of initial total activation at t = 0 that is left at the end of the simulation
#' @return An integer, # of timesteps to be used in simulation
#' @examples
#' # estimate.time(0.2, 0.1) # 10

estimate.time <- function(decay, final_proportion) {
  # decay = proportion of activation that is 'lost' at each time step, ranges from 0 to 1
  # decay must be a value from 0 to 1
  # final_proportion = proportion of initial total activation at t = 0 that is left at the end of the simulation
  # final_proportion must be a value from 0 to 1

  # check if decay is a number from 0 to 1
  if (decay < 0 || decay > 1) {
    stop('decay value is not a number from 0 to 1.')
  }

  # check if final_proportion is a number from 0 to 1
  if (final_proportion < 0 || final_proportion > 1) {
    stop('final_proportion value is not a number from 0 to 1.')
  }

  t <- round(log(final_proportion)/log(1 - decay), 0)

  return(t)
}
