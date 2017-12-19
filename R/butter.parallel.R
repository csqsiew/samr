#' The butter function takes in a dataframe with 'words' and 'activation' columns, and
#' calls the spread function for a specific number of times to simulate the spread of
#' activation of a given initial activation space (specified in start_time) over time.
#'
#' Note: butter.decay() is a modified function of butter.retention() and calls the parallelized
#' spread.decay.parallel() function for faster processing. The main difference is that spread.decay()
#' specifies decay rate, d, which the rate at which activation is lost at each time step. Use the
#' estimate.time() function to figure out the number of time steps required for total activation in
#' the network to decrease to 10% of its value at t = 0.
#'
#' @param start_run A non-empty dataframe with 'words' and 'activation' columns. Must be specified.
#' @param decay Proportion of activation that is lost at each time step. Default is 20%.
#' @param retention Proportion of activation that remains in the node, ranges from 0 to 1. Default is 0.
#' @param suppress Suppress nodes with total final activation of < x units at each time step. Recommended value of x is 0.1% of initial activation of target at t = 0. Default is 0.1.
#' @param network Network where the spreading occurs. Must be specified. Default is gc.net.
#' @param time Number of time steps to run spread function for. Default is 10.
#' @return A compiled dataframe with 'words', 'activation' and 'time' columns showing the spread of activation in the network over time.
#' @examples
#' See Vignette for examples.

butter.parallel <- function(start_run, decay = 0.2, retention = 0, suppress = 0.1, network = gc.net, time = 10) {
  # start_run = a non-empty dataframe with 'words' and 'activation' columns
  # decay = proportion of activation that is lost over time, ranges from 0 to 1
  # decay value default = 0.2
  # retention = proportion of activation that remains in the node, ranges from 0 to 1
  # retention value default = 0
  # suppress = nodes with activation less than the suppress value will be suppressed at each time step
  # suppress value default = 0.1 (0.1% of 100 units)
  # network = network where the spreading occurs
  # network value default = gc.net
  # time = number of time steps to run spread() for
  # time value default = 10

  # check if start_run is in the correct format
  if (is.data.frame(start_run) == F || colnames(start_run) != c('words', 'activation')) {
    stop('Input data is not in the correct format. Must be a dataframe with -words-
         and -activation- columns.')
  }

  # check if decay is a number from 0 to 1
  if (decay < 0 || decay > 1) {
    stop('Decay value is not a number from 0 to 1.')
  }

  # check if retention is a number from 0 to 1
  if (retention < 0 || retention > 1) {
    stop('Retention value is not a number from 0 to 1.')
  }

  # check if time is a non-negative number
  if (time < 0 || is.numeric(time) == F) {
    stop('Something is off with the time value.')
  }

  # check if network is an igraph object
  if (is.igraph(network) == F) {
    stop('Network is not an igraph object.')
  }

  # create an empty dataframe to store output
  output <- data.frame(words = vector(), activation = vector(), time = vector(),
                       stringsAsFactors=FALSE)

  for (t in 1:time) {

    updated <- spread.parallel(start_run, decay, retention, suppress, network)

    if (nrow(updated) > 0) {
      # if updated is not empty, save the updated output
      updated$time <- t
      output <- rbind(output, updated)
      # updated is now the new input (start_run)
      start_run <- updated
    } else {
      print('Spread terminated due to low activations (< 1).')
      return(output)
    }
  }
  return(output)
  }

