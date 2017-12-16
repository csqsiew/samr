#' The spread function takes in a dataframe with 'words' and 'activation' columns, and
#' simulates the spread of activation of the words listed in the 'words' columns to its
#' neighbors in the specified network.
#'
#' Note: spread.decay() is a modified function of spread.retention(). The main difference is
#' that spread.decay() specifies decay rate, d, which the rate at which activation is lost
#' at each time step. Use the estimate.time() function to figure out the number of time steps
#' required for total activation in the network to decrease to 10% of its value at t = 0.
#'
#' @param start_run A non-empty dataframe with 'words' and 'activation' columns.
#' @param decay Proportion of activation that is lost at each time step, ranges from 0 to 1. Default value = 0.2.
#' @param network Network where the spreading occurs
#' @return A updated dataframe with 'words' and 'activation' columns with new activation values.
#' @examples
#' See Vignette for examples.

spread.decay <- function(start_run, decay, network) {
  # start_run = a non-empty dataframe with 'words' and 'activation' columns
  # start_run value must be specified in the butter.decay() function
  # decay = proportion of activation that is 'lost' at each time step, ranges from 0 to 1
  # decay value must be specified in the butter.decay() function
  # network = network where the spreading occurs
  # network value must be specified in the butter.decay() function

  # create an empty dataframe to store output
  end_run <- data.frame(words = character(), activation = numeric(), stringsAsFactors=FALSE)

  for (i in 1:nrow(start_run)) { # for each node (target) in the start_run dataframe

    # get a list of its neighbors from the network
    neighborlist <- igraph::neighbors(network, start_run$words[i])$name

    if(length(neighborlist) == 0) {
      stop('Error: Word has no neighbors/not found in network.')
    }

    # update the end_run dataframe with new activations
    for (j in 1:length(neighborlist)) { # for each neighbor of the target
      output <- c(as.character(neighborlist[j]),
                  start_run$activation[i]/length(neighborlist)) # new activation = activation of the target node / number of neighbors to share activation with
      if (nrow(end_run) == 0) { # if end_run is an empty set
        end_run[1, ] <- output  # add to the first row
      } else {                  # if end_run is non-empty
        if (output[2] != 0) {
          end_run <- rbind(end_run, output) } # just rbind as usual
      }
    }
  }

  # after all nodes in start_run have 'spread' their activation,
  # clean up and consolidate end_run
  end_run <- aggregate(as.numeric(activation) ~ words , data = end_run , FUN = sum)
  colnames(end_run) <- c('words', 'activation')

  # allow entire end_run dataframe to 'decay' based on decay value
  end_run$activation <- end_run$activation*(1 - decay)

  # suppress outputs that are less than 1 to speed up the process, but may want to add an option to allow for suppression or not
  end_run <- dplyr::filter(end_run, activation > 1)

  # return the output
  return(as.data.frame(end_run))
}
