#' The make.initial() function generates the initial activation space for a set of words based on
#' the target word. The basic idea is that when a target word is presented, not only the target word but
#' other words in the lexicon are also activated, albeit partially. The amount of partial activation
#' received by non-target words is inversely proportional to the edit distance between the nontarget
#' and specified target word. I.e., words that are immediate neighbors will receive 1/2 of the activation
#' received by the target, and words that have an edit distance of 4 will receive 1/5 of the activation
#' received by the target. In this function one must specify the number of activation units that the
#' target receives, and the nontarget words' initial activation will be calculated based on that value.
#' Another argument that could be interesting is to designate a cutoff for the number of nontarget words
#' to be (partially) activated, for e.g., neighbors only (cutoff = 2) or up to 3-hop neighbors (cutoff = 4).
#' Words with edit distance larger than or equal to the cutoff will have an initial activation value of 0.
#'
#' @param target Character string of target node.
#' @param reference Reference lexicon to compare target node against.
#' @param target_act Number of activation units for the target word
#' @param cutoff Words with edit distance equal or larger than cutoff will not have an initial activation. To include all words, use a ridiculously large number for the cutoff, leave blank and use default of 100.
#' @return A dataframe with 2 colummns words and activation. Be sure to convert the words column to the label column in hml before using it as the start_run dataframe in butter().
#' @examples
#' # aback_initial <- make.initial('aback', gc$Ortho, 100, 5)

make.initial <- function(target, reference, target_act, cutoff = 100) {
  # target = character string, 1 word
  # reference = character vector of reference lexicon
  # target_act = number of activation units for the target word
  # cutoff = words with edit distance equal or larger than cutoff will not have an initial activation
  # to include all words, use a ridiculously large number for the cutoff, leave blank and use default of 100

  if (target %in% reference == F) { # return error message if word is not found in the GC
    stop('Word not in reference vector.')
  }

  # check if cutoff is > 0
  if (cutoff < 0) {
    stop('cutoff value is a negative number.')
  }

  # check if target_act is a non-negative number
  if (target_act < 0 || is.numeric(target_act) == F) {
    stop('Something is off with the target_act value.')
  }

  target_initialact <- data.frame(words = reference,
                                  actspace = vwr::levenshtein.distance(target, reference),
                                  stringsAsFactors = FALSE)

  target_initialact <- target_initialact %>%
    dplyr::mutate(activation = target_act / (1 + actspace)) %>%
    filter(actspace < cutoff) %>%
    select(words, activation) %>%
    filter(activation > 1)

  return(target_initialact)

}
