#' The butter function takes in a dataframe with 'words' and 'activation' columns, and
#' calls the spread function for a specific number of times to simulate the spread of
#' activation of a given initial activation space (specified in start_time) over time.
#'
#' Note that spread.retention() is different from spread.decay() in that spread.retention()
#' specifies a retention rate, r, which is the proportion of activation that is retained by the
#' target node during the spreading of activation. butter.retention() calls spread.retention().
#'
#' @param start_run A non-empty dataframe with 'words' and 'activation' columns. Must be specified.
#' @param retention Proportion of activation that remains in the node, ranges from 0 to 1. Default is 0.5.
#' @param network Network where the spreading occurs. Must be specified.
#' @param time Number of time steps to run spread function for. Default is 10.
#' @return A compiled dataframe with 'words', 'activation' and 'time' columns showing the spread of activation in the network over time.
#' @examples
#' See Vignette for examples.

butter <- function(start_run, retention = 0.5, network = gc.net, time = 10) {
  # start_run = a non-empty dataframe with 'words' and 'activation' columns
  # retention = proportion of activation that remains in the node, ranges from 0 to 1
  # retention value default = 0.5
  # network = network where the spreading occurs
  # network value default = gc.net
  # time = number of time steps to run spread() for
  # time value default = 10

  # check if start_run is in the correct format
  if (is.data.frame(start_run) == F || colnames(start_run) != c('words', 'activation')) {
    stop('Input data is not in the correct format. Must be a dataframe with -words-
         and -activation- columns.')
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

    updated <- spread.retention(start_run, retention, network)

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

