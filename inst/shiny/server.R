# americanRouletteV3 server file
# Ignat Kulinka

function(input, output) {
  # roulette function & helpers----------------------------------------------
  roulette <- function(verbose = FALSE) {
    slotLanded <- sample(possibleSlots, 1)
    if (verbose)
      cat("Landed on:", slotLanded, "\n")

    tableIndex <- which(bettingTable$slotNum == slotLanded)

    return(list(slotLanded = slotLanded,
                color = bettingTable$color[tableIndex],
                even = bettingTable$even[tableIndex],
                odd = bettingTable$odd[tableIndex],
                low = bettingTable$low[tableIndex],
                high = bettingTable$high[tableIndex],
                snakeBet = bettingTable$snakeBet[tableIndex],
                dozen = bettingTable$dozen[tableIndex],
                column = bettingTable$column[tableIndex]))
  }


  # Bet amount selection ----------------------------------------------------
  bet <- reactiveValues(amount = 10)

  observeEvent(input$bet1, {
    print("Bet changed to $10")
    bet$amount <- 10
  })
  observeEvent(input$bet2, {
    print("Bet changed to $25")
    bet$amount <- 25
  })

  observeEvent(input$bet3, {
    print("Bet changed to $50")
    bet$amount <- 50
  })

  observeEvent(input$bet4, {
    print("Bet changed to $100")
    bet$amount <- 100
  })

  observeEvent(input$bet5, {
    print("Bet changed to $250")
    bet$amount <- 250
  })

  # Plots ------------------------------------------------------------------
  output$plot0 <- shiny::renderPlot({
    rouletteTable <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = df, ggplot2::aes(x, y, fill = factor(z), color = factor(z)), size = 1.5) +
      ggplot2::geom_polygon(data = twoToOne1, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = twoToOne2, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = twoToOne3, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = oneToEighteenSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = redSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = evenSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = oddSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = blackSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = thirdTwelveSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = secondTwelveSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = firstTwelveSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = ninteenThirtysixSlots, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      ggplot2::geom_polygon(data = zeroPentagon, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      ggplot2::geom_polygon(data = doubleZeroPentagon, ggplot2::aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      ggplot2::scale_fill_manual(values = cols) +
      ggplot2::scale_color_manual(values = colsTwo) +
      ggplot2::coord_equal() +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ditch_the_axes +
      # 1-3: white circles; 4: transparent
      #ggplot2::geom_circle(ggplot2::aes(x0=c(df$x,0.5, 3.5, columnBets$x, splitBets$x, dozenBets$x,
      # outsideBets$x, quadBets$x, lineBets$x, streetBets$x, trioBets$x, topLineBets$x), y0=c(df$y, 23.9, 23.9, columnBets$y,
      # splitBets$y, dozenBets$y, outsideBets$y, quadBets$y, lineBets$y, streetBets$y, trioBets$y, topLineBets$y), r=.7, color =
      # factor('4'))) +
      ggplot2::annotate("text", x = df$x, y = df$y, label = annotationLabels, color = "white") +
      ggplot2::annotate("text", x = zerosAnnotationCoords$x, y = zerosAnnotationCoords$y, label = c("0", "00"),
               color = "white") +
      ggplot2::annotate("text", x = columnAnnotationCoords$x, y = columnAnnotationCoords$y, label = "2:1",
               size = 3.5, color = "white") +
      ggplot2::annotate("text", x = rep(-2, 3), y = c(3, 11, 19), label = c("3rd 12", "2nd 12", "1st 12"),
               color = "white", angle = -90, size = 5) +
      ggplot2::annotate("text", x = rep(-4, 6), y = c(1, 5, 9, 13, 17, 21), label = c("19to36", "Odd", "Black", "Red", "Even", "1to18"), color = "white", angle = -90, size = 4) +
      # ggplot2::geom_point(data = clickable, ggplot2::aes(x=x, y=y)) +
      ggplot2::geom_point(data = NULL, ggplot2::aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = "dimgray", size = 12) +
      ggplot2::geom_point(data = NULL, ggplot2::aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = "royalblue4", size = 9) +
      ggplot2::annotate("text", x = selectedPoints$data$x, y = selectedPoints$data$y, label = selectedPoints$data$betAmount,
               color = "white", size = 4)
    rouletteTable
  }, width = 700, height = 700)

  output$mainPlot <- renderPlot({
    # [,1]: balance [,2]: count
    graph <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = outcomesList$data[, 2], y = outcomesList$data[, 1])) +
      ggplot2::geom_line() +
      # labs(title = 'Total Balance vs. Number of Bets') +
      ggplot2::geom_point() +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Number of Rounds", y = "Total Balance") +
      ggplot2::xlim(0, length(outcomesList$data) + 5) +
      ggplot2::coord_cartesian(ylim = c(min(outcomesList$data[, 1]) - 5, max(outcomesList$data[, 1]) + 5))
    graph
  })

  output$leftPlot <- renderPlot({
    df <- data.frame(num = leftPlot$data[c(1, 3)], names = c("Manual", "CPU"))

    graph <- ggplot2::ggplot(data = df, ggplot2::aes(x = names, y = num)) +
      ggplot2::geom_bar(stat = "identity", width = 0.4) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Bet Type", y = "Number of Bets Won") +
      ggplot2::coord_cartesian(ylim = c(min(df$num) - 2, max(df$num) + 2))

    graph
  })

  output$rightPlot <- renderPlot({
    df <- data.frame(x = as.character(roulette$history))
    ggplot2::ggplot(df, ggplot2::aes(factor(x))) +
      ggplot2::geom_bar(stat = "count", width = 0.6) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Slot Number", y = "Frequency") +
      ggplot2::coord_cartesian(ylim = c(0, 5))

    # graph
  })

  # Reactive stuff ----------------------------------------------------------

  selectedPoints <- reactiveValues(data = cbind(clickable[0, ],
                                                betAmount = double(),
                                                manualBet = logical()))

  roulette <- reactiveValues(winningSlot = NULL, history = NULL)

  resultsTable <- reactiveValues(data = cbind(clickable[0, ],
                                              betAmount = double(),
                                              manualBet = logical()))

  completeList <- reactiveValues(data = cbind(slots = numeric(),
                                              #clickable[0, 9:ncol(clickable)],
                                              betAmount = double(),
                                              manualBet = logical(),
                                              outcome = double(),
                                              winningSlot = double()))

  outcomesList <- reactiveValues(data = cbind(balance = 0,
                                              betNum = 0))

  leftPlot <- reactiveValues(data = cbind(manualNumWins = 0,
                                          manualWinnings = 0,
                                          cpuNumWins = 0,
                                          cpuWinnings = 0))

  observeEvent(input$spin, {


    # save the bets for results tables
    resultsTable$data <- selectedPoints$data

    # clear the betting table
    selectedPoints$data <- cbind(clickable[0, ], betAmount = double(), manualBet = logical())

    # spin the roulette
    roulette$winningSlot <- roulette()
    roulette$history <- c(roulette$history, roulette$winningSlot$slotLanded)

    if (nrow(resultsTable$data) > 0) {
      # print(str(leftPlot$data))
      tableOverall <- data.frame(slots = apply(resultsTable$data, 1, combineSlots),
                                 betAmount = resultsTable$data$betAmount,
                                 outcome = apply(resultsTable$data, 1, checkWin),
                                 manualBet = resultsTable$data$manualBet,
                                 stringsAsFactors = FALSE)

      manualTotals <- data.frame(outcome = apply(tableOverall[tableOverall$manualBet == TRUE, ], 1, computeTotal),
                                 stringsAsFactors = FALSE)

      cpuTotals <- data.frame(outcome = apply(tableOverall[tableOverall$manualBet == FALSE, ], 1, computeTotal),
                              stringsAsFactors = FALSE)

      leftPlot$data <- cbind(manualNumWins = leftPlot$data[1, 1] + sum(manualTotals$outcome > 0),
                             manualWinnings = leftPlot$data[1, 2] + sum(manualTotals$outcome),
                             cpuNumWins = leftPlot$data[1, 3] + sum(cpuTotals$outcome > 0),
                             cpuWinnings = leftPlot$data[1, 4] + sum(cpuTotals$outcome))


      totalsOverall <- data.frame(outcome = apply(tableOverall, 1, computeTotal),
                                  stringsAsFactors = FALSE)

      completeList$data <- rbind(completeList$data,
                                  cbind(slots = apply(resultsTable$data, 1, combineSlots),
                                        resultsTable$data[,10:ncol(resultsTable$data)],
                                        outcome = apply(tableOverall, 1, computeTotal),
                                        winningSlot = roulette$winningSlot$slotLanded))


      totalsList <- totalsOverall$outcome
    } else {
      totalsList <- 0
    }

    # update numBets
    currentBetNum <- outcomesList$data[nrow(outcomesList$data), 2]
    currentBalance <- outcomesList$data[nrow(outcomesList$data), 1]
    newBalance <- sum(totalsList)
    outcomesList$data <- rbind(outcomesList$data,
                               cbind(balance = currentBalance + as.numeric(newBalance),
                                     betNum = currentBetNum + 1))

  })

  observeEvent(input$reset, {
    selectedPoints$data <- cbind(clickable[0, ], betAmount = double(), manualBet = logical())
  })

  observeEvent(input$plot_click, {
    currentBet <- isolate(bet$amount)
    resultsTable$data <- cbind(clickable[0, ], betAmount = double(), manualBet = logical())
    roulette$winningSlot <- NULL
    click <- nearPoints(clickable, input$plot_click, threshold = 20, maxpoints = 1)

    if (nrow(click) != 0) {
      # make a new bet!
      newBet <- cbind(click, betAmount = currentBet, manualBet = TRUE)
      if (nrow(selectedPoints$data) == 0) {
        # first bet being placed in this case, just add the bet and return the list
        selectedPoints$data <- rbind(selectedPoints$data, newBet)
        return()

      } else {
        # iterate through all the bets already placed and compare to the newBet
        for (i in 1:nrow(selectedPoints$data)) {
          # 1st: check if the bet type is the same -> same bet this is mostly for outside bets
          if (all(newBet[1:2] == selectedPoints$data[i, 1:2])) {
            # check if the coordinates are the same -> same bet this is mostly for inside bets
            selectedPoints$data[i, 11] <- selectedPoints$data[i, 11] + currentBet
            return()
          }
          # check if the bet type is the same -> same bet : this is mostly for outside bets
          if (newBet[3] == selectedPoints$data[i, 3]) {
            selectedPoints$data[i, 11] <- selectedPoints$data[i, 11] + currentBet
            return()
          }
        }
        selectedPoints$data <- rbind(selectedPoints$data, newBet)
        return()
      }
    }
  })

  observeEvent(input$random, {
    numBets <- isolate(input$numBets)
    if (numBets > 0) {
      randomBet <- cbind(uniqueBets[sample(nrow(uniqueBets), numBets), ],
                         betAmount = sample(c(10, 25, 50, 100, 250), numBets, replace = TRUE),
                         manualBet = FALSE)
      if (nrow(selectedPoints$data) == 0) {
        # first bet being placed in this case, just add the bet and return the list
        selectedPoints$data <- rbind(selectedPoints$data, randomBet)
        return()
      } else {
        # iterate through all the bets already placed and compare to the newBet
        for (i in 1:nrow(randomBet)) {
          betOver <- FALSE
          for (j in 1:nrow(selectedPoints$data)) {
            # 1st: check if the bet type is the same -> same bet this is mostly for outside bets
            if (all(randomBet[i, 1:2] == selectedPoints$data[j, 1:2])) {
              # check if the coordinates are the same -> same bet this is mostly for inside bets
              selectedPoints$data[j, 11] <- selectedPoints$data[j, 11] + randomBet[i, 11]
              betOver <- TRUE
              break
            }

            if (randomBet[i, 3] == selectedPoints$data[j, 3]) {
              selectedPoints$data[j, 11] <- selectedPoints$data[j, 11] + randomBet[i, 11]
              betOver <- TRUE
              break
            }

          }
          if (!betOver) {
            selectedPoints$data <- rbind(selectedPoints$data, randomBet[i, ])
          }
        }
      }
    }
  })

  output$roulette <- renderText({
    if (!is.null(roulette$winningSlot)) {
      paste("The winning slot is:", roulette$winningSlot$slotLanded)
    } else {
      return(invisible(NULL))
    }
  })

  combineSlots <- function(row) {
    betList = c(row[3])
    index = 3

    while (!is.na(row[index])) {
      if (index > 8) {
        break
      }
      index = index + 1
    }

    if (index == 4) {
      return(betList)
    } else {
      for (i in 4:(index - 1)) {
        betList = paste(betList, row[i], sep = ", ")
      }
      return(betList)
    }
  }

  output$result <- DT::renderDT({
    # no bets placed before rolling the roulette
    if (nrow(resultsTable$data) == 0) {
      return(invisible(NULL))
    } else {
      table <- data.frame(slots = apply(resultsTable$data, 1, combineSlots),
                          betAmount = resultsTable$data$betAmount,
                          outcome = apply(resultsTable$data, 1, checkWin),
                          stringsAsFactors = FALSE)
      DT::datatable(table, colnames = c("Slots", "Bet Amount", "Outcome"),
                rownames = FALSE, options = list(pageLength = 5, sDom = "<\"top\">rt<\"bottom\">ip"))
    }

  })

  output$dataOutput <- DT::renderDT({
    DT::datatable(completeList$data, rownames = FALSE,
              colnames = c("Slots", "Bet Amount", "Manual Bet", "Outcome", "Winning Slot"),
              options = list(pageLength = 10, sDom = "<\"top\">rt<\"bottom\">ip",
                             language = list(zeroRecords = "Completed bets are listed here")))
  })

  checkWin <- function(row) {
    # check for inside bets first row[10] -> row$type
    if (row[10] %in% c("Single", "Split", "Square Bet", "Line Bet", "Street Bet", "Trio Bet", "Top Line Bet")) {
      payout <- as.numeric(row[11]) * as.numeric(row[9])
      winnerFlag <- any(c(row[3:8]) == roulette$winningSlot$slotLanded, na.rm = TRUE)
      if (winnerFlag) {
        return(paste("won: $", payout, sep = ""))
      } else {
        return(paste("lost: $", row[11], sep = ""))
      }
    } else if (row[10] %in% c("Column Bet", "Dozen Bet", "High", "Low", "Even", "Odd", "Red", "Black")) {
      # outside bets
      payout <- as.numeric(row[11]) * as.numeric(row[9])
      if (row[10] == "Column Bet") {
        if (substr(row[3], 1, 1) == roulette$winningSlot$column) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Dozen Bet") {
        if (substr(row[3], 1, 1) == roulette$winningSlot$dozen) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "High") {
        if (roulette$winningSlot$high) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Low") {
        if (roulette$winningSlot$low) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Even") {
        if (roulette$winningSlot$even) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Odd") {
        if (roulette$winningSlot$odd) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Red") {
        if (roulette$winningSlot$color == 1) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else if (row[10] == "Black") {
        if (roulette$winningSlot$color == 2) {
          return(paste("won: $", payout, sep = ""))
        } else {
          return(paste("lost: $", row[11], sep = ""))
        }
      } else {
        return("Unknown outside bet")
      }
    } else {
      return("Unclassified bet")
    }
  }

  computeTotal <- function(row) {
    if (substr(row[3], 1, 1) == "l") {
      return(-1 * as.numeric(row[2]))
    } else {
      return(as.numeric(row[2]))
    }
  }

  output$total <- renderText({
    if (nrow(resultsTable$data) == 0) {
      return(invisible(NULL))
    } else {
      table <- data.frame(slots = apply(resultsTable$data, 1, combineSlots),
                          betAmount = resultsTable$data$betAmount,
                          outcome = apply(resultsTable$data, 1, checkWin),
                          stringsAsFactors = FALSE)

      totals <- data.frame(outcome = apply(table, 1, computeTotal),
                           stringsAsFactors = FALSE)

      balance <- sum(totals$outcome)
      paste("Total for this round: ", balance, "$", sep = "")
    }
  })

  output$viewManual <- DT::renderDT({
    if (nrow(selectedPoints$data) == 0) {
      return(invisible(NULL))
    } else {
      manualTable <- selectedPoints$data[selectedPoints$data$manualBet == TRUE, ]
      betTable <- data.frame(betType = manualTable$type, slots = apply(manualTable, 1, combineSlots),
                             betAmount = manualTable$betAmount,
                             stringsAsFactors = FALSE)

      DT::datatable(betTable, colnames = c("Bet Type", "Slots", "Bet Amount"),
                rownames = FALSE,
                option = list(pageLength = 5, sDom = "<\"top\">rt<\"bottom\">ip",
                              language = list(zeroRecords = "Please click the plot to enter manual bets")))
    }
  })

  output$viewCPU <- DT::renderDT({
    if (nrow(selectedPoints$data) == 0) {
      return(invisible(NULL))
    } else {
      cpuBets <- selectedPoints$data[selectedPoints$data$manualBet == FALSE, ]
      betTable <- data.frame(betType = cpuBets$type, slots = apply(cpuBets, 1, combineSlots), betAmount = cpuBets$betAmount,
                             stringsAsFactors = FALSE)

      DT::datatable(betTable, colnames = c("Bet Type", "Slots", "Bet Amount"),
                rownames = FALSE,
                options = list(pageLength = 5, sDom = "<\"top\">rt<\"bottom\">ip",
                               language = list(zeroRecords = "Please enter CPU assited bets")))
    }
  })

  # Downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(completeList$data, file, row.names = FALSE)
    }
  )
}