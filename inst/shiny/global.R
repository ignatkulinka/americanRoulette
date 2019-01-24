# americanRouletteV3 global file
# Ignat Kulinka


# I. bettingTable Setup ---------------------------------------------------
# Setup of the bettingTable, dataframe that keeps track of slots on the table

slotNum <- c(0, "00" , c(1:36))

color <- c(rep(3, 2),
           rep(c(1, 2), 5),
           rep(c(2, 1), 4),
           rep(c(1, 2), 5),
           rep(c(2, 1), 4))


even <- c(rep(0, 2), rep(c(0,1), 18))
odd <- c(rep(0, 2), rep(c(1,0), 18))

low <- c(rep(0,2), rep(1, 18), rep(0, 18))
high <- c(rep(0,2), rep(0, 18), rep(1, 18))

snakeBet <- c(rep(0,2),
              rep(c(1,0,0,0),2),
              1, 0, 0,
              rep(c(1,0), 3),
              0,
              rep(c(1,0,0,0), 2),
              1, 0, 0,
              rep(c(1,0),3),
              0)

dozen <- c(rep(0,2),
           rep(1,12),
           rep(2,12),
           rep(3,12))

column <- c(rep(0,2),
            rep(c(1,2,3),12))

bettingTable <- data.frame(slotNum, color, even,
                           odd, low, high,
                           snakeBet, dozen, column)



# II. Roulette Table Graphing Coordinates ---------------------------------

# A. Coordinates for the regular slots section of the roulette table
df <- expand.grid(x = seq(0,4,2), y = seq(0,22,2))
df$z <- c(rep(c(1, 2), 3), 2, 2, 1, 1,
          rep(c(2, 1), 4),
          rep(c(1, 2), 3), 2, 2, 1, 1,
          rep(c(2, 1), 4))

# B. Pentagons for the 0 and 00 slots on the table
zeroPentagon <- data.frame(z = rep(3,5),
                           x = c(-1, -1, .5, 2, 2),
                           y = c(23, 24, 25, 24, 23))

doubleZeroPentagon <- data.frame(z = rep(3,5),
                                 x = c(2, 2, 3.5, 5, 5),
                                 y = c(23, 24, 25, 24, 23))

# C. Outside bets bounding boxes
thirdTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                               y = c(-1, 7, 7, -1),
                               z = rep(3,4))

secondTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                                y = c(7, 15, 15, 7),
                                z = rep(3,4))

firstTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                               y = c(15, 23, 23, 15),
                               z = rep(3,4))

ninteenThirtysixSlots <- data.frame(x = c(-3, -3, -5, -5),
                                    y = c(-1, 3, 3, -1),
                                    z = rep(3,4))

oddSlots <- data.frame(x = c(-3, -3, -5, -5),
                       y = c(3, 7, 7, 3),
                       z = rep(3,4))

blackSlots <- data.frame(x = c(-3, -3, -5, -5),
                         y = c(7, 11, 11, 7),
                         z = rep(2,4))

redSlots <- data.frame(x = c(-3, -3, -5, -5),
                       y = c(11, 15, 15, 11),
                       z = rep(1,4))

evenSlots <- data.frame(x = c(-3, -3, -5, -5),
                        y = c(15, 19, 19, 15),
                        z = rep(3,4))

oneToEighteenSlots <- data.frame(x = c(-3, -3, -5, -5),
                                 y = c(19, 23, 23, 19),
                                 z = rep(3,4))

twoToOne1 <- data.frame(x = c(-1, -1, 1, 1),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))

twoToOne2 <- data.frame(x = c(1, 1, 3, 3),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))

twoToOne3 <- data.frame(x = c(3, 3, 5, 5),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))


# III. Coordinates for Labels ---------------------------------------------

annotationCoords <- data.frame(x = rep(c(0:2), 12),
                               y = rep(c(0:11), 1, each = 3))

zerosAnnotationCoords <- data.frame(x = c(.5, 3.5),
                                    y = c(23.8, 23.8))

columnAnnotationCoords <- data.frame(x = seq(0,4,2),
                                     y = rep(-2,3))

# Labels for the slots
annotationLabels <- c(34, 35, 36,
                      31, 32, 33,
                      28, 29, 30,
                      25, 26, 27,
                      22, 23, 24,
                      19, 20, 21,
                      16, 17, 18,
                      13, 14, 15,
                      10, 11, 12,
                      7, 8, 9,
                      4, 5, 6,
                      1, 2, 3)


# IV. Definition of Bets --------------------------------------------------
# Below are the definitions of all the possible bets. The parameters defined:
#  x,y: coordinates for the table/ggplot2
#  b1 - b6: the numbers included in the bet
#  payOut: the payout for winning the bet
#  type: name of the bet

# A. Straight up / Single Bet (0 and 00 included): Bet on any single number
slotsToBets <- data.frame(x = c(rep(c(0, 2, 4), 12), 0.5, 3.5),
                          y = c(rep(seq(0, 22, 2), 1, each = 3), 23.9, 23.9),
                          b1 = c(34, 35, 36,
                                 31, 32, 33,
                                 28, 29, 30,
                                 25, 26, 27,
                                 22, 23, 24,
                                 19, 20, 21,
                                 16, 17, 18,
                                 13, 14, 15,
                                 10, 11, 12,
                                 7, 8, 9,
                                 4, 5, 6,
                                 1, 2, 3,
                                 0, "00"),
                          b2 = rep(NA, 38),
                          b3 = rep(NA, 38),
                          b4 = rep(NA, 38),
                          b5 = rep(NA, 38),
                          b6 = rep(NA, 38),
                          payOut = rep(35, 38),
                          type = "Single",
                          stringsAsFactors = FALSE)

# B. Split Bet: Bet on any two adjoining numbers vertical/horizontal
splitBets <- data.frame(x = c(rep(1, 12),
                              rep(3, 12),
                              rep(c(0, 2, 4), each = 11)),
                        y = c(rep(seq(0, 22, 2), 2),
                              rep(seq(1,22,2), 3)),
                        b1 = c(seq(34, 1, -3),
                               seq(35, 2, -3),
                               seq(31, 1, -3),
                               seq(32, 2, -3),
                               seq(33, 3, -3)),
                        b2 = c(seq(35, 2, -3),
                               seq(36, 3, -3),
                               seq(34, 4, -3),
                               seq(35, 5, -3),
                               seq(36, 6, -3)),
                        b3 = rep(NA, 57),
                        b4 = rep(NA, 57),
                        b5 = rep(NA, 57),
                        b6 = rep(NA, 57),
                        payOut = rep(17, 57),
                        type = "Split",
                        stringsAsFactors = FALSE)

# C. Column Bets: Bet on all numbers in columns 1 - 3
columnBets <- data.frame(x = seq(0,4,2),
                         y = rep(-2,3),
                         b1 = c("1st Column",
                                "2nd Column",
                                "3rd Column"),
                         b2 = rep(NA, 3),
                         b3 = rep(NA, 3),
                         b4 = rep(NA, 3),
                         b5 = rep(NA, 3),
                         b6 = rep(NA, 3),
                         payOut = rep(2,3),
                         type = "Column Bet",
                         stringsAsFactors = FALSE)

# D. Square Bets: Bet on any 4 adjoining numbers in a block
quadBets <- data.frame(x = c(rep(1, 11),
                             rep(3, 11)),
                       y = c(seq(1, 22, 2),
                             seq(1, 22, 2)),
                       b1 = c(seq(31, 1, -3),
                              seq(32, 2, -3)),
                       b2 = c(seq(32, 2, -3),
                              seq(33, 3, -3)),
                       b3 = c(seq(34, 4, -3),
                              seq(35, 5, -3)),
                       b4 = c(seq(35, 5, -3),
                              seq(36, 6, -3)),
                       b5 = rep(NA, 22),
                       b6 = rep(NA, 22),
                       payOut = rep(5, 22),
                       type = "Square Bet",
                       stringsAsFactors = FALSE)

# C. Street Bets: Bet on any three horizontal numbers in a row
streetBets <- data.frame(x = c(rep(-1, 12),
                             rep(5, 12)),
                       y = c(seq(0, 22, 2),
                             seq(0, 22, 2)),
                       b1 = rep(seq(34, 1, -3), 2),
                       b2 = rep(seq(35, 2, -3), 2),
                       b3 = rep(seq(36, 3, -3), 2),
                       b4 = rep(NA, 24),
                       b5 = rep(NA, 24),
                       b6 = rep(NA, 24),
                       payOut = rep(5, 24),
                       type = "Street Bet",
                       stringsAsFactors = FALSE)

# D. Line Bets: Bet on any six numbers from two adjacent horizontal row
lineBets <- data.frame(x = c(rep(-1, 11),
                               rep(5, 11)),
                         y = c(seq(1, 21, 2),
                               seq(1, 21, 2)),
                         b1 = rep(seq(31, 1, -3), 2),
                         b2 = rep(seq(32, 2, -3), 2),
                         b3 = rep(seq(33, 3, -3), 2),
                         b4 = rep(seq(34, 4, -3), 2),
                         b5 = rep(seq(35, 5, -3), 2),
                         b6 = rep(seq(36, 6, -3), 2),
                         payOut = rep(5, 22),
                         type = "Line Bet",
                         stringsAsFactors = FALSE)


# E. Dozen Bets: 1st through 3rd dozen bets
dozenBets <- data.frame(x = c(rep(-2,15)),
                        y = c(seq(0, 6, 1.5),
                              seq(8, 14, 1.5),
                              seq(16, 22, 1.5)),
                        b1 = c(rep("3rd Dozen", 5),
                               rep("2nd Dozen", 5),
                               rep("1st Dozen", 5)),
                        b2 = rep(NA, 15),
                        b3 = rep(NA, 15),
                        b4 = rep(NA, 15),
                        b5 = rep(NA, 15),
                        b6 = rep(NA, 15),
                        payOut = rep(2,15),
                        type = "Dozen Bet",
                        stringsAsFactors = FALSE)

# F. Outside Bets: Low, High, Even, Odd, Red, Black
outsideBets <- data.frame(x = rep(-4, 12),
                          y = c(0.3, 1.7,
                                4.2, 5.7,
                                8.3, 9.8,
                                12.2, 13.7,
                                16.3, 17.8,
                                20.2, 21.7),
                          b1 = c(rep("High", 2),
                                 rep("Odd", 2),
                                 rep("Black", 2),
                                 rep("Red", 2),
                                 rep("Even", 2),
                                 rep("Low", 2)),
                          b2 = rep(NA, 12),
                          b3 = rep(NA, 12),
                          b4 = rep(NA, 12),
                          b5 = rep(NA, 12),
                          b6 = rep(NA, 12),
                          payOut = rep(1, 12),
                          type = c(rep("High", 2),
                                   rep("Odd", 2),
                                   rep("Black", 2),
                                   rep("Red", 2),
                                   rep("Even", 2),
                                   rep("Low", 2)),
                          stringsAsFactors = FALSE)

# G. Trio Bets: Bets on any three slot combinations including 0 or 00
trioBets <- data.frame(x = c(1, 3),
                       y = c(23),
                       b1 = c(0, "00"),
                       b2 = c(1, 2),
                       b3 = c(2, 3),
                       b4 = rep(NA, 2),
                       b5 = rep(NA, 2),
                       b6 = rep(NA, 2),
                       payOut = rep(11, 2),
                       type = "Trio Bet",
                       stringsAsFactors = FALSE)

# H. Top Line: Bet on 0-00-1-2-3
topLineBets <- data.frame(x = c(-1, 5),
                          y = c(23),
                          b1 = rep(0, 2),
                          b2 = rep("00", 2),
                          b3 = rep(1, 2),
                          b4 = rep(2, 2),
                          b5 = rep(3, 2),
                          b6 = rep(NA, 2),
                          payOut = rep(6, 2),
                          type = "Top Line Bet",
                          stringsAsFactors = FALSE)

# # G. Basket: bet on 0-00-2
# basketBets <- data.frame(x = c(),
#                          y = c(),
#                          b1 = c(0),
#                          b2 = c("00"),
#                          b3 = c(2),
#                          b4 = c(NA),
#                          b5 = c(NA),
#                          b6 = c(NA),
#                          payOut = rep(6, 2),
#                          type = "Top Line Bet",
#                          stringsAsFactors = FALSE)

# Combine all bet information in one dataframe
clickable <- rbind(columnBets, slotsToBets, splitBets,
                  dozenBets, outsideBets, quadBets,
                  lineBets, streetBets, trioBets,
                  topLineBets)

# Grab nonrepeated entries in columns 3:10
# to use with random bet (i.e. pick a bet from here)
uniqueBets <- clickable[!duplicated(clickable[3:10]),]


# V. Colors and Helpers for ggplot2 ---------------------------------------

cols <- c("1" = "red", "2" = "black", "3" = "darkgreen", "4" = "white")
colsTwo <- c("1" = "white", "2" = "white", "3" = "white", "4" = rgb(0,0,0,0))

ditch_the_axes <- ggplot2::theme(
  axis.text = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank(),
  plot.margin = ggplot2::unit(c(1,1,1,1), "cm"),
  legend.position = "none"
)



# VI. Roulette Function & Helpers -----------------------------------------

# A. Function responsible for picking a random number out of the slots a
#     and returning the winning slot as well its characteristics

roulette <- function(verbose = FALSE) {
  possibleSlots <- c(0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15, 3,
                     24, 36, 13, 1, "00", 27, 10, 25, 29, 12, 8, 19, 31, 18,
                     6, 21, 33, 16, 4, 23, 35, 14, 2)

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


