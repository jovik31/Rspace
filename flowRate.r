link_capacities <<- c(256e3, 256e3, 256e3, 256e3, 256e3, 256e3, 256e3)
packet_size <<- 1000
flows <<- list(list(arrivalRate = 134.431209 + 36.800703, route = c(1, 3, 6)),
               list(arrivalRate = 35.670306, route = c(1, 4)),
               list(arrivalRate = 80.568791 + 91.199297, route = c(2, 5, 7)),
               list(arrivalRate = 128, route = c(4)),
               list(arrivalRate = 28.329694, route = c(2, 5)))
total_rate <- 0
flows_arrival_rate <- c(rep(0, length(flows)))
flows_arrival_rate <- c(flows_arrival_rate, 0, 0)
links_arrival_rate <<- c(rep(0, length(link_capacities)))
flows_service_rate <<- link_capacities / packet_size
i <- 1
for (flow in flows){
  arrivalrate <- flow$arrivalRate
  flows_arrival_rate[i] <- arrivalrate
  total_rate <- total_rate + arrivalrate
  route <- flow$route
  for (j in route){
    links_arrival_rate[j] <- links_arrival_rate[j] + arrivalrate
  }
  i <- i + 1
}
avg_user <- function() {
  l_flow <- sum(flows_arrival_rate / (flows_service_rate - links_arrival_rate))
  cat("Avg number of packets in the network:", l_flow)
}
avg_delay_calc <- function() {
  initial_w <<- (1 / (flows_service_rate - links_arrival_rate))
  final_w <<- c(rep(0, length(flows)))
  final_w <<- c(final_w, 0, 0)
  for (i in seq_along(flows)){
    route <- flows[[i]]$route
    for (j in route){
      final_w[i] <<- final_w[i] + initial_w[j]
    }
    cat("\nDelay of flow", i, "is",final_w[i], "seconds")
  }
}
total_avg_time <- function() {
  totalw <- (( final_w * flows_arrival_rate) / total_rate)
  cat("\ntotal rate:", total_rate, "packets/s")
  cat("\ntotal avg delay in network is", sum(totalw), "s\n")
}

avg_user()
avg_delay_calc()
total_avg_time()
