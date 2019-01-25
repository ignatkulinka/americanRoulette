# americanRouletteV3 server file
# Ignat Kulinka

function(input, output) {

# I. Bet Amount Selection -------------------------------------------------
  # Store the bet amount
  bet <- reactiveValues(amount = 10)

  # Choose the bet amount
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


# II. Plots ---------------------------------------------------------------
  # Roulette table
  output$rTable <- renderPlot({
    rouletteTable <- ggplot() +
      geom_tile(data = df, aes(x, y, fill = factor(z), color = factor(z)), size = 1.5) +
      geom_polygon(data = twoToOne1, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = twoToOne2, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = twoToOne3, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = oneToEighteenSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = redSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = evenSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = oddSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = blackSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = thirdTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = secondTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = firstTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = ninteenThirtysixSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = zeroPentagon, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      geom_polygon(data = doubleZeroPentagon, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = colsTwo) +
      coord_equal() +
      theme_bw() +
      #ditch_the_axes +
      # 1-3: white circles; 4: transparent
      #geom_circle(aes(x0=c(df$x,0.5, 3.5, columnBets$x, splitBets$x, dozenBets$x,
      # outsideBets$x, quadBets$x, lineBets$x, streetBets$x, trioBets$x, topLineBets$x), y0=c(df$y, 23.9, 23.9, columnBets$y,
      # splitBets$y, dozenBets$y, outsideBets$y, quadBets$y, lineBets$y, streetBets$y, trioBets$y, topLineBets$y), r=.7, color =
      # factor('4'))) +
      annotate("text", x = df$x, y = df$y, label = annotationLabels, color = "white") +
      annotate("text", x = zerosAnnotationCoords$x, y = zerosAnnotationCoords$y, label = c("0", "00"),
               color = "white") +
      annotate("text", x = columnAnnotationCoords$x, y = columnAnnotationCoords$y, label = "2:1",
               size = 3.5, color = "white") +
      annotate("text", x = rep(-2, 3), y = c(3, 11, 19), label = c("3rd 12", "2nd 12", "1st 12"),
               color = "white", angle = -90, size = 5) +
      annotate("text", x = rep(-4, 6), y = c(1, 5, 9, 13, 17, 21), label = c("19to36", "Odd", "Black", "Red", "Even", "1to18"), color = "white", angle = -90, size = 4) +
      # show all clickable points
      geom_point(data = clickable, aes(x=x, y=y)) +
      geom_point(data = NULL, aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = "dimgray", size = 12) +
      geom_point(data = NULL, aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = "royalblue4", size = 9) +
      annotate("text", x = selectedPoints$data$x, y = selectedPoints$data$y, label = selectedPoints$data$betAmount,
               color = "white", size = 4)
    rouletteTable
  }, width = 700, height = 700)

  # B. Main Round Number vs Total Balance Plot
  output$mainPlot <- renderPlot({
    # Uses data from outcomesList$data ([,1]: balance [,2] round_number)
    # Extract needed values from reactive data
    # Y-axis
    current_balance <- outcomesList$data[nrow(outcomesList$data), 1]
    lower_y_bound <- round((min(outcomesList$data[, 1]) - 1) * 1.5)
    upper_y_bound <- round((max(outcomesList$data[, 1]) + 1) * 1.5)
    y_breaks <- seq(lower_y_bound, upper_y_bound)

    # X-axis
    current_bet_num <- max(outcomesList$data[,2])
    x_breaks <- seq(0, round(current_bet_num * 1.5))

    # Make the plot below
    ggplot(data = NULL, aes(x = outcomesList$data[, 2], y = outcomesList$data[, 1])) +
      geom_line() +
      geom_point() +
      scale_y_continuous(breaks = y_breaks,
                         labels = ifelse(y_breaks %in% current_balance, current_balance, "")) +
      labs(x = "Round Number", y = "Total Balance") +
      scale_x_continuous(breaks = x_breaks,
                         labels = ifelse(x_breaks %in% current_bet_num, current_bet_num, "")) +
      geom_hline(yintercept = current_balance, linetype = "dotted") +
      geom_vline(xintercept = max(outcomesList$data[,2]), linetype = "dotted") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
  })

  output$bottomLeftPlot <- renderPlot({
    # Plot amount won by CPU vs Manual
    # Data cols:
    # manualNumWins manualWinnings manualNumBets cpuNumWins cpuWinings cpuNumBets
    df <- data.frame(num = bottomPlotsData$data[c(2, 5)], names = c("Manual", "CPU"))

    ggplot(data = df, aes(x = names, y = num)) +
      geom_bar(stat = "identity", width = 0.4) +
      labs(x = "", y = "Amount Won") +
      geom_text(aes(label = df$num), position = position_dodge(width = 0.4), vjust = 1.2) +
      coord_cartesian(ylim = c(min(df$num) * 1.1, max(df$num) * 1.1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x.bottom = element_text(size = 12),
            axis.text.y.left = element_text(size = 12))

  })

  output$bottomMidPlot <- renderPlot({
    # Plot the number of bets won by CPU vs Manual
    df <- data.frame(num = bottomPlotsData$data[c(1, 4)], names = c("Manual", "CPU"))

    ggplot(data = df, aes(x = names, y = num)) +
      geom_bar(stat = "identity", width = 0.4) +
      labs(x = "", y = "Number of Bets Won") +
      geom_text(aes(label = df$num), position = position_dodge(width = 0.4), vjust = -0.25) +
      coord_cartesian(ylim = c(0, max(df$num) + 2)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x.bottom = element_text(size = 12),
            axis.text.y.left = element_text(size = 12))

  })

  output$bottomRightPlot <- renderPlot({
    # Plot the overall bets won vs list (CPU and Manual combined)
    df <- data.frame(num = c(bottomPlotsData$data[1] + bottomPlotsData$data[4],
                             bottomPlotsData$data[3] + bottomPlotsData$data[6] - (bottomPlotsData$data[1] + bottomPlotsData$data[4])),
                     names = c("Won", "Lost"))
    print(as.vector(df$num))
    ggplot(data = df, aes(x = names, y = num)) +
      geom_bar(stat = "identity", width = 0.4) +
      geom_text(aes(label = df$num), position = position_dodge(width = 0.4), vjust = -0.25) +
      labs(x = "", y = "Number of Bets") +
      scale_y_continuous(limits = c(0, max(df$num) + 2),
                         breaks = seq(0, max(df$num) + 2, ceiling(length(seq(0, max(df$num) + 2)) / 5)),
                         labels = seq(0, max(df$num) + 2, ceiling(length(seq(0, max(df$num) + 2)) / 5))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x.bottom = element_text(size = 12),
            axis.text.y.left = element_text(size = 12))

  })

  output$midPlot <- renderPlot({
    df <- data.frame(x = as.character(roulette$history))

    ggplot(df, aes(x = factor(x, levels = c("0", "00", seq(1, 36, 1))))) +
      geom_bar(stat = "count", width = 0.6) +
      labs(x = "Slot Number", y = "Frequency") +
      scale_y_continuous(breaks = seq(0, ifelse(length(df$x) > 0, max(table(df$x)), 1))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 12),
            axis.title.x.bottom = element_text(size = 12),
            axis.title.y.left = element_text(size = 12))

  })


# III. Reactive Data Frames -----------------------------------------------

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

  bottomPlotsData <- reactiveValues(data = cbind(manualNumWins = 0,
                                          manualWinnings = 0,
                                          manualNumBets = 0,
                                          cpuNumWins = 0,
                                          cpuWinnings = 0,
                                          cpuNumBets = 0))

# IV. Event Observers -----------------------------------------------------

  observeEvent(input$spin, {
    # save the bets for results tables
    resultsTable$data <- selectedPoints$data

    # clear the betting table
    selectedPoints$data <- cbind(clickable[0, ], betAmount = double(), manualBet = logical())

    # spin the roulette
    roulette$winningSlot <- roulette()
    roulette$history <- c(roulette$history, roulette$winningSlot$slotLanded)

    if (nrow(resultsTable$data) > 0) {
      tableOverall <- data.frame(slots = apply(resultsTable$data, 1, combineSlots),
                                 betAmount = resultsTable$data$betAmount,
                                 outcome = apply(resultsTable$data, 1, checkWin),
                                 manualBet = resultsTable$data$manualBet,
                                 stringsAsFactors = FALSE)
      # Grab the manual bets and sum the bet outcomes
      manualTotals <- data.frame(outcome = apply(tableOverall[tableOverall$manualBet == TRUE, ], 1, computeTotal),
                                 stringsAsFactors = FALSE)
      # Grab the CPU bets and sum the total outcomes
      cpuTotals <- data.frame(outcome = apply(tableOverall[tableOverall$manualBet == FALSE, ], 1, computeTotal),
                              stringsAsFactors = FALSE)
      # Update the data for bottomLeftPlot data
      bottomPlotsData$data <- cbind(manualNumWins = bottomPlotsData$data[1, 1] + sum(manualTotals$outcome > 0),
                             manualWinnings = bottomPlotsData$data[1, 2] + sum(manualTotals$outcome),
                             manualNumBets = bottomPlotsData$data[1, 2] + length(manualTotals$outcome),
                             cpuNumWins = bottomPlotsData$data[1, 4] + sum(cpuTotals$outcome > 0),
                             cpuWinnings = bottomPlotsData$data[1, 5] + sum(cpuTotals$outcome),
                             cpuNumWins = bottomPlotsData$data[1, 6] + length(cpuTotals$outcome))

      #print(bottomPlotsData$data)


      # Compute totals for the round
      totalsOverall <- data.frame(outcome = apply(tableOverall, 1, computeTotal),
                                  stringsAsFactors = FALSE)
      # Store the overall results
      completeList$data <- rbind(completeList$data,
                                 cbind(slots = apply(resultsTable$data, 1, combineSlots),
                                       resultsTable$data[, 10:ncol(resultsTable$data)],
                                       outcome = apply(resultsTable$data, 1, checkWin),
                                       winningSlot = roulette$winningSlot$slotLanded))

    } else {
      totalsOverall <- data.frame(outcome = 0,
                                  stringsAsFactors = FALSE)
      totalsOverall$outcome <- 0
    }

    # update numBets
    currentBetNum <- outcomesList$data[nrow(outcomesList$data), 2]
    currentBalance <- outcomesList$data[nrow(outcomesList$data), 1]
    newBalance <- sum(totalsOverall$outcome)
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


# V. Helper Functions -----------------------------------------------------

  checkWin <- function(row) {
    # check for inside bets first row[10] -> row$type
    # 1 2 3  4  5  6  7  8  9      10    11        12
    # x y b1 b2 b3 b4 b5 b6 payOut type betAmount manualBet
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
      return(as.numeric(str_extract(row[3], "(?<=won: \\$)\\d*")))
    }
  }

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


# VI. Outputs for UI ------------------------------------------------------

  output$roulette <- renderText({
    if (!is.null(roulette$winningSlot)) {
      paste("The winning slot is:", roulette$winningSlot$slotLanded)
    } else {
      return(invisible(NULL))
    }
  })

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
    filename = function(){
      paste("American Roulette", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      print(completeList$data)
      write.csv(completeList$data, file, row.names = FALSE)
    }
  )
}






