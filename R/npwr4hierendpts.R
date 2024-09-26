library(tidyverse)
library(skellam)
library(knitr)
library(kableExtra)
library(brio)

# function 1: this creates the Probability: win, lose, tie, WR, NB, WO, DOOR
huiman.power.fun1 <- function(endpoint=NA, mu.a=NA, mean.diff=NA, mu.b=NA, sd.a=NA, sd.b=NA, delta=NA,
                              pi.a=NA, prob.diff=NA, pi.b=NA,
                              er.a=NA, hr=NA, er.b=NA, s=NA,
                              lam.a=NA, rr=NA, lam.b=NA,
                              pi.ordinal.a=NA, pi.ordinal.b=NA,
                              tte.winning.direction=NA,
                              continuous.winning.direction=NA,
                              binary.winning.direction=NA,
                              count.winning.direction=NA,
                              ordinal.winning.direction=NA) {

  # Convert endpoint-specific parameters if not provided based on other inputs
  if(endpoint == "TTE") {
    if(is.null(hr)) hr <- NA
    if(is.null(er.a)) er.a <- NA
  }

  if(endpoint == "Binary") {
    if(is.null(prob.diff)) prob.diff <- NA
    if(is.null(pi.a)) pi.a <- NA
  }

  if(endpoint == "Continuous") {
    if(is.null(mean.diff)) mean.diff <- NA
    if(is.null(mu.a)) mu.a <- NA
  }

  if(endpoint == "Count") {
    if(is.null(rr)) rr <- NA
    if(is.null(lam.a)) lam.a <- NA
  }

  # Initialize combined result vectors
  p_win <- p_lose <- p_tie <- wr <- nb <- wo <- door <- numeric(0)

  # Store the results for each endpoint type separately
  results_list <- list()

  # TTE part
  if (endpoint=="TTE") {

    # Check if either er.a or hr is provided, not both, along with er.b
    if((!is.na(hr) && !is.na(er.a)) || (is.na(hr) && is.na(er.a)) || is.na(er.b)) {
      stop("For TTE: Must enter exactly one of 'er.a' or 'hr', and must provide 'er.b'.")
    }

    # Additional checks for required fields
    if(is.null(s) || is.null(tte.winning.direction)) {
      stop("For TTE: 's' and 'tte.winning.direction' must be provided.")
    }

    p_win.tte <- p_lose.tte <- p_tie.tte <- NA
    wr.tte <- wo.tte <- nb.tte <- door.tte <- NA
    hr.a <- hr.b <- NA

    if (!is.na(hr) && is.na(er.a)) {
      hr.b <- -log(1-er.b)/s
      hr.a <- hr*hr.b
    }

    if (!is.na(er.a) && is.na(hr)) {
      hr.a <- -log(1-er.a)/s
      hr.b <- -log(1-er.b)/s
    }

    if (tte.winning.direction=="GT") {
      p_win.tte <- hr.b/(hr.a+hr.b)*(1-exp(-(hr.a+hr.b)*s))
      p_lose.tte <- hr.a/(hr.a+hr.b)*(1-exp(-(hr.a+hr.b)*s))
    }

    if (tte.winning.direction=="LT") {
      p_win.tte <- hr.a/(hr.a+hr.b)*(1-exp(-(hr.a+hr.b)*s))
      p_lose.tte <- hr.b/(hr.a+hr.b)*(1-exp(-(hr.a+hr.b)*s))
    }

    p_tie.tte <- 1 - p_win.tte - p_lose.tte
    wr.tte <- p_win.tte / p_lose.tte
    nb.tte <- p_win.tte - p_lose.tte
    wo.tte <- (p_win.tte + 0.5 * p_tie.tte) / (p_lose.tte + 0.5 * p_tie.tte)
    door.tte <- p_win.tte + 0.5 * p_tie.tte


    # Store TTE results
    results_list <- list(p_win = p_win.tte, p_lose = p_lose.tte, p_tie = p_tie.tte,
                         wr = wr.tte, nb = nb.tte, wo = wo.tte, door = door.tte)

    return(results_list)

  }

  # Binary part
  if (endpoint=="Binary") {

    # Check if either pi.a or prob.diff is provided, not both, along with pi.b
    if((!is.na(prob.diff) && !is.na(pi.a)) || (is.na(prob.diff) && is.na(pi.a)) || is.na(pi.b)) {
      stop("For Continuous: Must enter exactly one of 'pi.a' or 'prob.diff', and must provide 'pi.b'.")
    }

    # Additional checks for required fields
    if(is.null(binary.winning.direction)) {
      stop("For Binary: 'binary.winning.direction' must be provided.")
    }

    if (!is.na(prob.diff) && is.na(pi.a)) {
      pi.a <- prob.diff + pi.b
    }

    p_win.bin <- p_lose.bin <- p_tie.bin <- NA
    wr.bin <- wo.bin <- nb.bin <- door.bin <- NA

    if (binary.winning.direction=="LT") {
      p_win.bin <- (1-pi.a)*pi.b
      p_lose.bin <- (1-pi.b)*pi.a
    }

    if (binary.winning.direction=="GT") {
      p_win.bin <- (1-pi.b)*pi.a
      p_lose.bin <- (1-pi.a)*pi.b
    }


    p_tie.bin <- 1 - p_win.bin - p_lose.bin
    wr.bin <- p_win.bin / p_lose.bin
    nb.bin <- p_win.bin - p_lose.bin
    wo.bin <- (p_win.bin + 0.5 * p_tie.bin) / (p_lose.bin + 0.5 * p_tie.bin)
    door.bin <- p_win.bin + 0.5 * p_tie.bin


    # Store Binary results
    results_list <- list(p_win = p_win.bin, p_lose = p_lose.bin, p_tie = p_tie.bin,
                         wr = wr.bin, nb = nb.bin, wo = wo.bin, door = door.bin)
    return(results_list)
  }

  # Ordinal part
  if (endpoint=="Ordinal") {

    # Check if either pi.a or prob.diff is provided, not both, along with pi.b
    if (length(pi.ordinal.b) != length(pi.ordinal.a)) {
      stop("You must enter 'Prob(Y=1), …, Prob(Y=J) in group A' along with 'Prob(Y=1), …, Prob(Y=J) in group B'")
    }

    # Additional checks for required fields
    if(is.null(ordinal.winning.direction)) {
      stop("For Ordinal: 'ordinal.winning.direction' must be provided.")
    }

    p_win.ordinal <- p_lose.ordinal <- 0

    J <- length(pi.ordinal.a) # Number of categories in the i-th endpoint

    if (ordinal.winning.direction=="GT") {
      for (i in 1:(J-1)) {
        sum_a <- sum(pi.ordinal.a[(i + 1):J]) # Sum of pi.ordinal.a from (i+1) to J
        sum_b <- sum(pi.ordinal.b[(i + 1):J]) # Sum of pi.ordinal.b from (i+1) to J

        p_win.ordinal <- p_win.ordinal + sum_a * pi.ordinal.b[i]
        p_lose.ordinal <- p_lose.ordinal + sum_b * pi.ordinal.a[i]
      }
    }

    if (ordinal.winning.direction=="LT") {
      for (i in 2:J) {
        sum_a <- sum(pi.ordinal.a[1:(i - 1)]) # Sum of pi.ordinal.a up to the (i-1)th element
        sum_b <- sum(pi.ordinal.b[1:(i - 1)]) # Sum of pi.ordinal.b up to the (i-1)th element

        p_win.ordinal <- p_win.ordinal + sum_a * pi.ordinal.b[i]
        p_lose.ordinal <- p_lose.ordinal + sum_b * pi.ordinal.a[i]
      }
    }

    p_tie.ordinal <- wr.ordinal <- wo.ordinal <- nb.ordinal <- door.ordinal <- NA

    p_tie.ordinal <- 1 - p_win.ordinal - p_lose.ordinal
    wr.ordinal <- p_win.ordinal / p_lose.ordinal
    nb.ordinal <- p_win.ordinal - p_lose.ordinal
    wo.ordinal <- (p_win.ordinal + 0.5 * p_tie.ordinal) / (p_lose.ordinal + 0.5 * p_tie.ordinal)
    door.ordinal <- p_win.ordinal + 0.5 * p_tie.ordinal


    # Store Ordinal results
    results_list <- list(p_win = p_win.ordinal, p_lose = p_lose.ordinal, p_tie = p_tie.ordinal,
                         wr = wr.ordinal, nb = nb.ordinal, wo = wo.ordinal, door = door.ordinal)

    return(results_list)

  }

  # Count part
  if (endpoint=="Count") {

    # Check if either lam.a or rr is provided, not both, along with lam.b
    if((!is.na(rr) && !is.na(lam.a)) || (is.na(rr) && is.na(lam.a)) || is.na(lam.b)) {
      stop("For Count: Must enter exactly one of 'lam.a' or 'rr', and must provide 'lam.b'.")
    }

    # Additional checks for required fields
    if(is.null(count.winning.direction)) {
      stop("For Count: 'count.winning.direction' must be provided.")
    }

    if (!is.na(rr) && is.na(lam.a)) {
      lam.a <- rr * lam.b
    }

    p_win.count <- p_lose.count <- p_tie.count <- NA
    wr.count <- wo.count <- nb.count <- door.count <- NA

    if (count.winning.direction=="LT") {
      p_win.count <- pskellam(0, lam.a, lam.b)-dskellam(0,lam.a, lam.b)
      p_lose.count <- 1-pskellam(0, lam.a, lam.b)
    }

    if (count.winning.direction=="GT") {
      p_win.count <- 1-pskellam(0, lam.a, lam.b)
      p_lose.count <- pskellam(0, lam.a, lam.b)-dskellam(0,lam.a, lam.b)
    }

    p_tie.count <- 1 - p_win.count - p_lose.count
    wr.count <- p_win.count / p_lose.count
    nb.count <- p_win.count - p_lose.count
    wo.count <- (p_win.count + 0.5 * p_tie.count) / (p_lose.count + 0.5 * p_tie.count)
    door.count <- p_win.count + 0.5 * p_tie.count

    # Store Count results
    results_list <- list(p_win = p_win.count, p_lose = p_lose.count, p_tie = p_tie.count,
                         wr = wr.count, nb = nb.count, wo = wo.count, door = door.count)

    return(results_list)

  }

  # Continuous part
  if (endpoint=="Continuous") {

    # Check if either mu.a or mean.diff is provided, not both, along with mu.b
    if((!is.na(mean.diff) && !is.na(mu.a)) || (is.na(mean.diff) && is.na(mu.a)) || is.na(mu.b)) {
      stop("For Binary: Must enter exactly one of 'mu.a' or 'mean.diff', and must provide 'mu.b'.")
    }

    # Additional checks for required fields
    if(is.null(sd.a) || is.null(sd.b) || is.null(delta) || is.null(continuous.winning.direction)) {
      stop("For Continuous: 'sd.a', 'sd.b', 'delta', and 'continuous.winning.direction' must be provided.")
    }

    if (!is.na(mean.diff) && is.na(mu.a)) {
      mu.a <- mean.diff + mu.b
    }

    p_win.cont <- p_lose.cont <- p_tie.cont <- NA
    wr.cont <- wo.cont <- nb.cont <- door.cont <- NA

    if (continuous.winning.direction=="GT") {
      p_win.cont <- pnorm((mu.a - mu.b - delta) / (sqrt(sd.a^2 + sd.b^2)))
      p_lose.cont <- pnorm((mu.b - mu.a - delta) / (sqrt(sd.a^2 + sd.b^2)))
    }

    if (continuous.winning.direction=="LT") {
      p_win.cont <- pnorm((mu.b - mu.a - delta) / (sqrt(sd.a^2 + sd.b^2)))
      p_lose.cont <- pnorm((mu.a - mu.b - delta) / (sqrt(sd.a^2 + sd.b^2)))
    }

    p_tie.cont <- 1 - p_win.cont - p_lose.cont
    wr.cont <- p_win.cont / p_lose.cont
    nb.cont <- p_win.cont - p_lose.cont
    wo.cont <- (p_win.cont + 0.5 * p_tie.cont) / (p_lose.cont + 0.5 * p_tie.cont)
    door.cont <- p_win.cont + 0.5 * p_tie.cont

    # Store Continuous results
    results_list <- list(p_win = p_win.cont, p_lose = p_lose.cont, p_tie = p_tie.cont,
                         wr = wr.cont, nb = nb.cont, wo = wo.cont, door = door.cont)

    return(results_list)

  }


}

# function 2: this creates sample size or power using results from function 1
huiman.power.results <- function(results_in=NA,
                                 sample.size=NA, power=NA,
                                 alpha=0.05, rratio=0.5, output = "ALL") {

  p_win <- results_in$p_win
  p_lose <- results_in$p_lose
  p_tie <- results_in$p_tie
  wr <- results_in$wr
  nb <- results_in$nb
  wo <- results_in$wo
  door <- results_in$door

  # total endpoints
  k <- length(p_win) # Number of endpoints

  term <- rep(NA, k)

  # Calculating each term
  term[1] <- p_lose[1] # First term is just p_lose[1]
  # Only execute the loop if k > 1
  if (k > 1) {
    for (i in 2:k) {
      term[i] <- prod(p_tie[1:(i-1)]) * p_lose[i]
    }
  }

  allterm <- sum(term)

  # Calculate p_tie_overall
  p_tie_overall <- prod(p_tie)

  # Calculate w vector
  #### output this weight for WR output
  w <- term / allterm

  # for win odds
  # term for k + 1
  term_final <- 0.5*p_tie_overall

  wo.allterm <- allterm + term_final

  ep <- term / wo.allterm

  #ep for k + 1
  ep_final <- term_final / wo.allterm

  #win odds weights marginal win ratio k
  ep_k <- term/(allterm + 0.5*p_tie_overall)

  # Aggregate calculations for overall values
  wr_c <- sum(wr * w)
  wo_c <- sum(wr * ep) + ep_final

  #### output this weight for WO output
  wo_weights <- wr * (ep_k / (wo))
  wo_weights <- c(wo_weights, ep_final)

  #settting NB and DOOR to 0 before iteration
  nb_c <- 0
  door_c <- 0
  for (i in 1:k) {
    # Calculate nb_c iteratively
    if (i == 1) {
      nb_c <- nb[i]
    } else {
      nb_c <- nb_c + nb[i] * prod(p_tie[1:(i-1)])
    }

    # Calculate door_c iteratively
    if (i == 1) {
      door_c <- door[i]
    } else {
      door_c <- door_c + (door[i] - 0.5) * prod(p_tie[1:(i-1)])
    }
  }

  #### output this weight for NB and DOOR output
  #NB and DOOR weights
  nb_door_weight <- rep(NA, k)

  for (i in 1:k) {
    if (i == 1) {
      nb_door_weight[i] <- 1
    } else {
      nb_door_weight[i] <- prod(p_tie[1:(i-1)])
    }
  }

  nb_c_a = (wo_c-1)/(wo_c+1)
  door_c_a = wo_c/(1+wo_c)

  # Calculate the powers
  if (!is.na(sample.size)) {
    power_c.WR = 1-pnorm(qnorm(1-alpha/2) - abs(log(wr_c))*sqrt(3*sample.size*(1-p_tie_overall)/(16*(1+p_tie_overall))))

    power_c.WO <- 1-pnorm(qnorm(1-alpha/2) - abs(log(wo_c))*sqrt(3*sample.size/(16*(1+p_tie_overall)*(1-p_tie_overall))))

    power_c.NB <- 1-pnorm(qnorm(1-alpha/2) - abs(nb_c)*sqrt(3*sample.size/(4*(1+p_tie_overall)*(1-p_tie_overall))))

    power_c.DOOR <- 1-pnorm(qnorm(1-alpha/2) - abs((door_c-0.5))*sqrt(3*sample.size/((1+p_tie_overall)*(1-p_tie_overall))))

  }

  #calculate the sample size
  if (!is.na(power)) {
    n.wr <- round(4*(1+p_tie_overall)*(qnorm(1-alpha/2)+qnorm(power))^2/(3*rratio*(1-rratio)*(1-p_tie_overall)*(log(wr_c))^2))

    n.wo <- round(4*(1+p_tie_overall)*(1-p_tie_overall)*(qnorm(1-alpha/2)+qnorm(power))^2/(3*rratio*(1-rratio)*(log(wo_c))^2))

    n.nb <- round((1+p_tie_overall)*(1-p_tie_overall)*(qnorm(1-alpha/2)+qnorm(power))^2 / (3*rratio*(1-rratio)*(nb_c^2)))

    n.door <- round((1+p_tie_overall)*(1-p_tie_overall)*(qnorm(1-alpha/2)+qnorm(power))^2 / (12*rratio*(1-rratio)*(door_c-0.5)^2))

  }

  # Return results

  # Conditional return based on output argument
  if (output == "ALL") {
    if (!is.na(sample.size)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,

                  wr = wr, wr_c = wr_c,
                  wo = wo, wo_c = wo_c,
                  nb = nb, nb_c = nb_c,

                  door = door, door_c = door_c,

                  power_c.WR = power_c.WR,
                  power_c.WO = power_c.WO,
                  power_c.NB = power_c.NB,
                  power_c.DOOR = power_c.DOOR))
    }
    if (!is.na(power)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,

                  wr = wr, wr_c = wr_c,
                  wo = wo, wo_c = wo_c,
                  nb = nb, nb_c = nb_c,

                  door = door, door_c = door_c,


                  n.wr = n.wr, n.wo = n.wo,
                  n.nb = n.nb, n.door = n.door))
    }
  }

  if (output == "WR") {
    if (!is.na(sample.size)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  w = w,
                  wr = wr, wr_c = wr_c,
                  power_c.WR = power_c.WR))
    }
    if (!is.na(power)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  w = w,
                  wr = wr, wr_c = wr_c,
                  n.wr = n.wr))
    }
  }

  if (output == "WO") {
    if (!is.na(sample.size)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  wo_weights = wo_weights,
                  wo = wo, wo_c = wo_c,
                  power_c.WO = power_c.WO))
    }
    if (!is.na(power)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  wo_weights = wo_weights,
                  wo = wo, wo_c = wo_c,
                  n.wo = n.wo))
    }
  }

  if (output == "NB") {
    if (!is.na(sample.size)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  nb_door_weight = nb_door_weight,
                  nb = nb, nb_c = nb_c,
                  power_c.NB = power_c.NB))
    }
    if (!is.na(power)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  nb_door_weight = nb_door_weight,
                  nb = nb, nb_c = nb_c,
                  n.nb = n.nb))
    }
  }

  if (output == "DOOR") {
    if (!is.na(sample.size)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  nb_door_weight = nb_door_weight,
                  door = door, door_c = door_c,
                  power_c.DOOR = power_c.DOOR))
    }
    if (!is.na(power)) {
      return(list(p_tie = p_tie,
                  p_tie_overall = p_tie_overall,
                  nb_door_weight = nb_door_weight,
                  door = door, door_c = door_c,
                  n.door = n.door))
    }
  }

}

# Main function that is to be used. This combines function 1 and 2

#' Hierarchical Endpoints
#'
#' This creates the Probability: win, lose, tie, WR, NB, WO, DOOR and creates sample size or power using results.
#' @param
#' @keywords endpoints
#' @export
#' @examples
#' hierarchical_endpoints()
hierarchical_endpoints <- function(endpoints_input, sample.size = NA, power = NA, alpha = 0.05, rratio = 0.5, output = "ALL") {

  # Check if power or sample_size is provided
  if (is.na(power) && is.na(sample.size)) {
    stop("Either power or sample.size must be provided.")
  }

  # Check for valid output argument
  valid_outputs <- c("ALL", "WR", "WO", "NB", "DOOR")
  if (!(output %in% valid_outputs)) {
    stop("Invalid output type. Choose from: ALL, WR, WO, NB, DOOR")
  }

  # Initialize vectors to store combined results
  combined_results <- list(p_win = numeric(0), p_lose = numeric(0), p_tie = numeric(0),
                           wr = numeric(0), nb = numeric(0), wo = numeric(0), door = numeric(0))

  # Iterate through each endpoint in the list
  for (endpoint_data in endpoints_input) {
    # Process each endpoint
    result <- huiman.power.fun1(
      endpoint = endpoint_data$type,
      mu.a = endpoint_data$mu.a,
      mean.diff = endpoint_data$mean.diff,
      mu.b = endpoint_data$mu.b,
      sd.a = endpoint_data$sd.a,
      sd.b = endpoint_data$sd.b,
      delta = endpoint_data$delta,
      pi.a = endpoint_data$pi.a,
      prob.diff = endpoint_data$prob.diff,
      pi.b = endpoint_data$pi.b,
      er.a = endpoint_data$er.a,
      hr = endpoint_data$hr,
      er.b = endpoint_data$er.b,
      s = endpoint_data$s,
      lam.a = endpoint_data$lam.a,
      rr = endpoint_data$rr,
      lam.b = endpoint_data$lam.b,
      pi.ordinal.a = endpoint_data$pi.ordinal.a,
      pi.ordinal.b = endpoint_data$pi.ordinal.b,
      tte.winning.direction = endpoint_data$tte.winning.direction,
      continuous.winning.direction = endpoint_data$continuous.winning.direction,
      binary.winning.direction = endpoint_data$binary.winning.direction,
      count.winning.direction = endpoint_data$count.winning.direction,
      ordinal.winning.direction = endpoint_data$ordinal.winning.direction
    )

    # Check if there was an error
    if (is.null(result)) {
      stop("Missing output, please check input parameters")
    }

    # Append the results of this endpoint to the combined results
    combined_results$p_win <- c(combined_results$p_win, result$p_win)
    combined_results$p_lose <- c(combined_results$p_lose, result$p_lose)
    combined_results$p_tie <- c(combined_results$p_tie, result$p_tie)
    combined_results$wr <- c(combined_results$wr, result$wr)
    combined_results$nb <- c(combined_results$nb, result$nb)
    combined_results$wo <- c(combined_results$wo, result$wo)
    combined_results$door <- c(combined_results$door, result$door)
  }

  # Now, calculate overall results using the combined results
  final_results <- huiman.power.results(
    results_in = combined_results,
    sample.size = sample.size,
    power = power,
    alpha = alpha,
    rratio = rratio,
    output = output
  )

  return(final_results)
}

# Define a function to format the results
#' Format Results
#'
#' This formats the results
#' @param
#' @keywords format
#' @export
#' @examples
#' formatResults()
formatResults <- function(result) {
  # Initialize vectors for labels and values
  labels <- c()
  values <- c()

  # Iterate through each result and assign label and value
  for (name in names(result)) {
    value <- result[[name]]
    # Round numeric values to 3 decimal places or convert numeric arrays to comma-separated strings
    if (is.numeric(value)) {
      if (length(value) > 1) {  # More than one element
        value <- paste(round(value, 3), collapse=", ")
      } else {  # Single numeric value
        value <- round(value, 3)
      }
    }

    # Determine the label based on the name of the result
    label <- switch(name,
                    p_tie = "Marginal probability of ties",
                    p_tie_overall = "Overall probability of ties",
                    wr = "Marginal win ratios",
                    wr_c = "Overall win ratios",
                    w = "Win ratios weights",
                    wo = "Marginal win odds",
                    wo_c = "Overall win odds",
                    wo_weights = "Win odds weights",
                    nb = "Marginal net benefits",
                    nb_c = "Overall net benefits",
                    nb_c_a = "Alternative overall net benefits",
                    door = "Marginal DOORs",
                    door_c = "Overall DOORs",
                    door_c_a = "Alternative overall DOORs",
                    nb_door_weight = "Weights for NB and DOOR",
                    power_c.WR = "Power for win ratios",
                    power_c.WO = "Power for win odds",
                    power_c.NB = "Power for net benefits",
                    power_c.DOOR = "Power for DOORs",
                    n.wr = "Sample size for win ratios",
                    n.wo = "Sample size for win odds",
                    n.nb = "Sample size for net benefits",
                    n.door = "Sample size for DOORs",
                    name)  # Default to name if not mapped

    # Append the label and value to the vectors
    labels <- c(labels, label)
    values <- c(values, as.character(value))  # Ensure value is character for consistency
  }

  # Combine labels and values into a data frame
  res_df <- data.frame(Label = labels, Value = values, stringsAsFactors = FALSE)

  return(res_df)

}
