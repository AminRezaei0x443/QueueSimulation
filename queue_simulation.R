# Title     : Queue Simulation
# Objective : Simulating Queue Systems with K-Servers
# Created by: Amin Rezaei
# Created on: 1/26/2021

# Add library for comparison
library(queuecomputer)
# Libraries for Table Data and Drawing
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

# queueCompute takes sorted arrivalTimes, serviceTimes and number of system servers
# to calculate departures + report system performance measures
queueCompute <- function(arrivalTimes, serviceTimes, serversCount) {
  departures <- NULL
  waitings <- NULL
  serveds <- NULL
  serverStates <- numeric(serversCount)
  enqueueTimes <- NULL
  dequeueTimes <- NULL
  detachTimes <- NULL

  # Calculate Departures
  serverSelect <- function (arrivalTime){
    which.max(arrivalTime - serverStates)
  }

  for(i in seq_along(arrivalTimes)){
    cArrival <- arrivalTimes[i]
    cService <- serviceTimes[i]
    selectedSrv <- serverSelect(cArrival)

    waitingTime <- max(0, serverStates[selectedSrv] - cArrival)
    serverStates[selectedSrv] <- cArrival + waitingTime + cService

    detachTimes <- c(detachTimes, serverStates[selectedSrv])
    dequeueTimes <- c(dequeueTimes, waitingTime + cArrival)
    enqueueTimes <- c(enqueueTimes, cArrival)

    serveds <- c(serveds, selectedSrv)
    waitings <- c(waitings, waitingTime)
    departures <- c(departures, serverStates[selectedSrv])
  }
  # Calculate QueueLength for Events
  queueCount <- function (cT){
    c <- 0
    if(length(enqueueTimes) == 0 || length(dequeueTimes)==0) return(0)
    for(i in seq_along(enqueueTimes)){
      eT <- enqueueTimes[i]
      dT <- dequeueTimes[i]
      if (eT <= cT && dT > cT){
        c <- c+1
      }
    }
    c
  }
  qStates <- 0
  qTimes <- 0
  for(i in seq_along(arrivalTimes)){
    cArrival <- arrivalTimes[i]
    w <- waitings[i]
    t <- 0
    if (w == 0){
      t <- 1
    }
    qTimes <- c(qTimes, cArrival)
    qStates <- c(qStates, queueCount(cArrival) + t)
    qTimes <- c(qTimes, cArrival + w)
    qStates <- c(qStates, queueCount(cArrival + w))
  }
  oD <- order(qTimes)
  qTimes <- qTimes[oD]
  qStates <- qStates[oD]

  # Calculate SystemLength for Events
  servingCount <- function (cT){
    c <- 0
    if(length(dequeueTimes) == 0 || length(detachTimes)==0) return(0)
    for(i in seq_along(dequeueTimes)){
      eT <- dequeueTimes[i]
      dT <- detachTimes[i]
      if (eT <= cT && dT > cT){
        c <- c+1
      }
    }
    c
  }

  allEvents <- sort(unique(c(detachTimes, dequeueTimes, enqueueTimes)))
  sTimes <- 0
  sStates <- 0
  for(i in seq_along(allEvents)){
    e <- allEvents[i]
    sTimes <- c(sTimes, e)
    sStates <- c(sStates, queueCount(e) + servingCount(e))
  }

  # Helper area under step function calculator
  area_under <- function(x, y){
    nx <- c(0, x[1:length(x)-1])
    ny <- c(0, y[1: length(y)-1])
    nd <- x - nx
    area <- nd * ny
    return(sum(area))
  }

  df <- data.frame(
    arrival=arrivalTimes,
    service=serviceTimes,
    departure=departures,
    waiting=waitings,
    server=serveds
  )
  ql <- data.frame(
    time=qTimes,
    len=as.integer(qStates)
  )
  sl <- data.frame(
    time=sTimes,
    len=as.integer(sStates)
  )
  return(list(
    # System Departures and Events
    departures=df,
    queueLength=ql,
    systemLength=sl,
    # System Performance Measures
    meanSystemCustomers=area_under(sl$time, sl$len)/max(departures),
    meanQueueLength=area_under(ql$time, ql$len)/max(ql$time),
    meanResponseTime=mean(serviceTimes + waitings),
    meanWaitTime=mean(waitings),
    serverUtilization=sum(serviceTimes) / (max(departures) * serversCount)
  ))
}

# Calculate Probability of having n customers in system for different times = P_n(t)
# For Checking Convergence
calculateSteadyState <- function (systemLengthsState){
  uLen <- unique(systemLengthsState)
  prob <- list()
  for(i in seq(1, length(systemLengthsState), 200)){
    for(u in uLen){
      tg <- NULL
      if(length(prob) >= u+1){
        tg <- prob[[u+1]]
      }
      prob[[u + 1]] <- c(tg,sum(systemLengthsState[1:i] == u) / i)
    }
  }
  return(prob)
}

# Calculates P_n(t) and plots to check convergence to Steady-State
# [Simulation done with implemented queueCompute]
testSteadyState <- function (){
  servers <- 2
  n <- 3000
  set.seed(9712017)
  arrivals <- rexp(n)
  arrivalTimes <- cumsum(arrivals)
  serviceTime <- rexp(n)
  od <- queueCompute(arrivalTimes, serviceTime, servers)
  prob <- calculateSteadyState(od$systemLength)
  par(mfcol = c(2, 3))
  for(i in seq_along(prob)){
    plot(prob[[i]], type="l", ylab = sprintf("P%d", i-1))
  }
}

# Calculates P_n(t) and plots to check convergence to Steady-State
# [Simulation done with queuecomputer library]
testSteadyStateLibrary <- function (){
  servers <- 2
  n <- 10000
  set.seed(9712017)
  arrivals <- rexp(n)
  arrivalTimes <- cumsum(arrivals)
  serviceTime <- rexp(n)
  od <- queue_step(arrivalTimes, serviceTime, servers)
  prob <- calculateSteadyState(od$systemlength_df)
  par(mfcol = c(2, 3))
  for(i in seq_along(prob)){
    plot(prob[[i]], type="l", ylab = sprintf("P%d", i-1))
  }
}

# Simulate call center with s servers and for 50 customers over time
callCenterSimulate <- function (s){
  set.seed(9712017)
  arrivals <- rexp(50, 1)
  servers <- s
  arrivalTimes <- cumsum(arrivals)
  serviceTime <- rexp(50, 0.5)
  od <- queueCompute(arrivalTimes, serviceTime, servers)
  r <- c(od$serverUtilization, od$meanSystemCustomers, od$meanQueueLength, od$meanWaitTime, od$meanResponseTime)
  return(r)
}

# Simulate call center for k in {1,2,3} and check system performance measures
testCallCenter <- function (){
  customGreen <- "#71CA97"
  customRed <- "#ff7f7f"

  i1 <- data.table(
    `Indicator Name` = c("Server Utilization", "Mean System Customers", "Mean Queue Length", "Mean Wait Time", "Mean Response Time"),
    `1 server` = callCenterSimulate(1),
    `2 server` = callCenterSimulate(2),
    `3 server` = callCenterSimulate(3)
  )

  formattable(i1, align =c("l","c","c","c"), list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    area(row=1, col = 2:4) ~ color_tile(customGreen, customRed),
    area(row=2, col = 2:4) ~ color_tile(customGreen, customRed),
    area(row=3, col = 2:4) ~ color_tile(customGreen, customRed),
    area(row=4, col = 2:4) ~ color_tile(customGreen, customRed),
    area(row=5, col = 2:4) ~ color_tile(customGreen, customRed)
  ))
}