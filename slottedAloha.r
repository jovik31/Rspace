slotted_aloha <- function() {
  n_total <- 10
  n_thinking <- n_total
  n_sim <- 0
  success_transmissions <- 0
  failed_transmissions <- 0
  slots <- 1000
  n_backlogged <- 0
  t_transmission <- 0
  b_transmission <- 0
  count <- 0
  throughput <- 0
  while (n_sim < slots) {
    t_transmission <- transmission(n_thinking, 0.01)
    b_transmission <- transmission(n_backlogged, 0.3)
    if (t_transmission == 1 && b_transmission == 0) {
      n_thinking <- n_thinking
      n_backlogged <- n_backlogged
      success_transmissions <- success_transmissions + 1
    } else if (b_transmission == 1 && t_transmission == 0) {
      n_thinking <- n_thinking + 1
      success_transmissions <- success_transmissions + 1
      n_backlogged <- n_backlogged - 1
    } else {
      n_backlogged <- n_backlogged + t_transmission
      n_thinking <- n_thinking - t_transmission
      failed_transmissions <- failed_transmissions + 1
    }
    count <- count + n_thinking / n_total
    n_sim <- n_sim + 1
  }
  throughput <- success_transmissions / slots
  cat("\nSigma:", 0.01, "p:", 0.3, "N:", 10)
  cat("\nthroughput exprimental:", throughput)
}
transmission <- function(n_users, p) {
  n_transmissions <- 0
  if (n_users == 0) {
    return(n_transmissions)
  } else {
    for (i in 1:n_users) {
      transmit <- success_transmission(p)
      if (transmit == 1) {
        n_transmissions <- n_transmissions + 1
      }
    }
    return(n_transmissions)
  }
}
success_transmission <- function(p) {
  success <- rbinom(1, 1, p)
  return(success)
}

slotted_aloha()
