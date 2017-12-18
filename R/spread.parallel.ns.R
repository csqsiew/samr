#' The spread function takes in a dataframe with 'words' and 'activation' columns, and
#' simulates the spread of activation of the words listed in the 'words' columns to its
#' neighbors in the specified network.
#'
#' THIS IS THE NO SUPPRESSION VERSION. RUNTIMES COULD BE SLOW.
#'
#' @param start_run A non-empty dataframe with 'words' and 'activation' columns.
#' @param decay Proportion of activation that is lost at each time step, ranges from 0 to 1.
#' @param retention Proportion of activation that remains in the node, ranges from 0 to 1.
#' @param network Network where the spreading occurs.
#' @return A updated dataframe with 'words' and 'activation' columns with new activation values.
#' @examples
#' See Vignette for examples.

spread.parallel.ns <- function(start_run, decay, retention, network) {
  # start_run = a non-empty dataframe with 'words' and 'activation' columns
  # start_run dataframe must be specified in the butter.parallel() function
  # decay = proportion of activation that is lost at each time step, ranges from 0 to 1
  # decay value must be specified in the butter.parallel() function
  # retention = proportion of activation that remains in the node, ranges from 0 to 1
  # retention value must be specified in the butter.parallel() function
  # network = network where the spreading occurs
  # network / igraph object must be specified in the butter.parallel() function

  ### NEW PARALLEL CODE

  # initialize
  # Create cluster with desired number of cores
  ncores <- detectCores()
  cl <- makeCluster(ncores)
  # Register cluster
  registerDoParallel(cl)

  # parallel code
  x <- foreach(i = 1:nrow(start_run), .combine = rbind,
               .packages = c('dplyr', 'samr', 'igraph')) %dopar% {
                 # create an empty dataframe to store output
                 end_run <- data.frame(words = character(), activation = numeric(), stringsAsFactors=FALSE)

                 # get a list of neighbors from the network
                 neighborlist <- igraph::neighbors(network, start_run$words[i])$name

                 if(length(neighborlist) == 0) {
                   stop('Error: Word has no neighbors/not found in network.')
                 }

                 # update the end_run dataframe with new activations

                 # if retention > 0, the target node will keep a proportion of the activation
                 if (retention > 0) { # if retention = 0, do not run this chunk of code
                   # for the target:
                   output <- c(as.character(start_run$words[i]), start_run$activation[i]*retention) # new activation = original activation * retention
                   if (nrow(end_run) == 0) { # if end_run is an empty set
                     end_run[1, ] <- output  # add to the first row
                   } else {                  # if end_run is non-empty
                     end_run <- rbind(end_run, output)  # just rbind as usual
                   }
                 }

                 # for the neighbors:
                 for (j in 1:length(neighborlist)) { # for each neighbor of the target
                   output <- c(as.character(neighborlist[j]),
                               (start_run$activation[i]*(1-retention))/length(neighborlist)) # new activation = activation given up by the target node / number of neighbors to share activation with
                   if (nrow(end_run) == 0) { # if end_run is an empty set
                     end_run[1, ] <- output  # add to the first row
                   } else {                  # if end_run is non-empty
                     if (output[2] != 0) {
                       end_run <- rbind(end_run, output) } # just rbind as usual
                   }
                 }
                 end_run
               }

  # close cluster
  stopCluster(cl)

  end_run <- x

  ### END PARALLEL CODE

  # after all nodes in start_run have 'spread' their activation,
  # clean up and consolidate end_run
  end_run <- aggregate(as.numeric(activation) ~ words , data = end_run , FUN = sum) # there is a bug here where aggregate doesn't work when nrow = 0, i.e., when words do not have  activation
  colnames(end_run) <- c('words', 'activation')

  # allow entire end_run dataframe to 'decay' based on decay value
  end_run$activation <- end_run$activation*(1 - decay)

  # suppress outputs that are less than 1 to speed up the process, but may want to add an option to allow for suppression or not
  # end_run <- dplyr::filter(end_run, activation > 1)

  # return the output
  return(as.data.frame(end_run))
}
