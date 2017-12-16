#' The spread function takes in a dataframe with 'words' and 'activation' columns, and
#' simulates the spread of activation of the words listed in the 'words' columns to its
#' neighbors in the specified network.
#'
#' Note that spread.retention() is different from spread.decay() in that spread.retention()
#' specifies a retention rate, r, which is the proportion of activation that is retained by the
#' target node during the spreading of activation.
#'
#' @param start_run A non-empty dataframe with 'words' and 'activation' columns.
#' @param retention Proportion of activation that remains in the node, ranges from 0 to 1.
#' @param network Network where the spreading occurs
#' @return A updated dataframe with 'words' and 'activation' columns with new activation values.
#' @examples
#' See Vignette for examples.

spread <- function(start_run, retention, network) {
  # start_run = a non-empty dataframe with 'words' and 'activation' columns
  # start_run value must be specified in the butter() function
  # retention = proportion of activation that remains in the node, ranges from 0 to 1
  # retention value must be specified in the butter() function
  # network = network where the spreading occurs
  # network value must be specified in the butter() function

  # create an empty dataframe to store output
  end_run <- data.frame(words = vector(), activation = vector(), stringsAsFactors=FALSE)

  for (i in 1:nrow(start_run)) { # for each node (target) in the start_run dataframe

    # get a list of neighbors from the network
    neighborlist <- igraph::neighbors(network, start_run$words[i])$name

    if(length(neighborlist) == 0) {
      stop('Error: Word has no neighbors/not found in network.')
    }

    # update the end_run dataframe with new activations

    # for the target:
    output <- c(as.character(start_run$words[i]), start_run$activation[i]*retention) # new activation = original activation * retention
    if (nrow(end_run) == 0) { # if end_run is an empty set
      end_run[1, ] <- output  # add to the first row
    } else {                  # if end_run is non-empty
      end_run <- rbind(end_run, output)  # just rbind as usual
    }

    # for the neighbors:
    for (j in 1:length(neighborlist)) { # for each neighbor of the target
      output <- c(as.character(neighborlist[j]),
                  (start_run$activation[i]*(1-retention))/length(neighborlist)) # new activation = activation given up by the target node / number of neighbors to share activation with
      end_run <- rbind(end_run, output)
    }
  }

  # after all nodes in start_run have 'spread' their activation,
  # clean up and consolidate end_run
  end_run <- aggregate(as.numeric(activation) ~ words , data = end_run , FUN = sum) # will return an error if nrow = 0, i.e., for some words, there is no activation
  colnames(end_run) <- c('words', 'activation')

  # suppress outputs that are less than 1 to speed up the process, but may want to add an option to allow for suppression or not
  end_run <- dplyr::filter(end_run, activation > 1)

  # return the output
  return(end_run)
}
