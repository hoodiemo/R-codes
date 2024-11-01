################################################################################ 
#################### Statistical Programming Languages 2024 ####################
####################            Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Caspar Moritz
# Name: Gerland
# Student ID (Matrikelnummer): 629441
#-------------------------------------------------------------------------------

### Exercise 5 -----------------------------------------------------------------

# function that "correctifies" y values (must be upside down)
transform_y <- function(y, n_col){
  return(n_col - y + 1)
}

# creates a blank plot and fills it with dots
create_canvas = function(no_row, no_col){
  # Create a blank "nude" plot (no plotting) with axes ranging from 0 to row/col 
  # length (+1 to have origin at (1,1))
  # frame.plot = FALSE to hide borders from https://stackoverflow.com/questions/22470114/removing-top-and-right-borders-from-boxplot-frame-in-r
  plot(c(0,no_row + 1), c(0,no_col + 1), type = "n", xlab = "", ylab = "", 
       frame.plot = FALSE, axes = FALSE)
  
  y_axis_labels <- no_col:1  # reverse the order of y axis and create axis
  # las = 1 was taken from https://r-charts.com/base-r/axes/
  axis(2, at = 1:no_col, labels = y_axis_labels, line = - 5, lty = 0, las = 1)
  axis(3, at = 1:no_row, labels = 1:no_row, tck = -0.01, line = -5, lty = 0)
  # add grid points by iterating through rows and columns
  j <- 1
  while (j <= no_col) {
    i <- 1
    while (i <= no_row) {
      points(i, transform_y(j, no_col), col = "black", pch = 16)
      i <- i + 1
    }
    j <- j + 1
  }
}

move_check <- function(player, n_row, n_col){
  cat("Player", player, "choose a dot (1: row, 2: column)!", "\n")
  pos_y <- readline("1: ")
  pos_x <- readline("2: ")
  
  # first check if the input is numeric and reprompt if necessary
  # (assume that 9x9 is maximum playing pitch (2x2 is minimum), 
  # gets unreadable after anyways)
  while (grepl("^[1-9]$", pos_x) == FALSE | grepl("^[1-9]$", pos_y) == FALSE){ 
    cat("Incorrect input, please enter a number between 1 and 9", "\n")
    pos_x <- as.numeric(readline("1: "))
    pos_y <- as.numeric(readline("2: "))
  }
  # now check if input is within boundaries of canvas and reprompt if not
  while (!(0 < pos_x && pos_x <= n_row) || !(0 < pos_y && pos_y <= n_col)){
    cat("Chosen dot is outside of grid. Again:", "\n")
    pos_x <- as.numeric(readline("1: "))
    pos_y <- as.numeric(readline("2: "))
  }
  # then ask for direction of the line (reprompt if invalid)
  cat("Player", player, ", choose the direction (d = down, r = right)!", "\n")
  direction <- readline("1: ")
  # check if direction is valid
  while (direction != "r" && direction != "d"){
    cat("Direction must be one of d or r, corresponding to down or right. Again:", 
        "\n")
    direction <- readline("1: ")
  }
  # check out of boundary border choices
  if ((pos_x == n_col && direction == "r") | (pos_y == n_row && direction == "d")){
    cat("Chosen direction is not available. Again:", "\n")
    move_check(player, n_row, n_col)
  }else{
    # save triplet of info
    choice <- c(pos_x, pos_y, direction)
    return(choice)
  }
}

# checks every move to see if this lead to a point
point_check <- function(drawn_lines, winning_boxes, p1_score, p2_score, p_turn, n_col){
  # transform scores to numeric again, had some issues with cat() when not doing so
  p1_score <- as.numeric(p1_score)
  p2_score <- as.numeric(p2_score)

  # iterate over all possible scoring boxes
  i <- 1
  gained_points <- 0 # allows for more than 1 point to be scored per move
  # determine color of symbol for filled box
  if (p_turn == 1){
    box_color <- "black"
  }else{
    box_color <- "red"
  }
  while (i <= length(winning_boxes)){
    sublist <- winning_boxes[[i]]
    # Check if any of the vectors in any of the sublist are in drawn_lines 
    # (would mean a point)
    # the condition of this if clause was retrieved from ChatGPT
    if (all(sapply(sublist, function(vectors){
      any(sapply(drawn_lines, identical, vectors))
    }))) {
      # get the position of the first entry of the sublist to get coordinates for symbol
      # https://csu-r.github.io/Module1/indexing.html to correctly index into a 
      # list of vectors
      x_box <- as.numeric(sublist[[1]][1]) + 0.5
      y_box <- transform_y(as.numeric(sublist[[1]][2]), n_col) - 0.5
      # mark position of check box accordingly
      points(x_box, y_box, col = box_color, cex = 5, pch = 10)
      
      # Remove the matching sublist from winning_boxes to avoid double counting
      winning_boxes <- winning_boxes[-i]
      # allow for more than one point to be added to the player's score
      gained_points <- gained_points + 1
    }else{
      i <- i + 1
    }
  }
    
    # if there has been a scoring, update players turn and points accordingly
    if (gained_points > 0){
      if (p_turn == 1){
        p1_score <- as.numeric(p1_score) + gained_points
        p_turn <- 2
      }else{
        p2_score <- as.numeric(p2_score) + gained_points
        p_turn <- 1
      }
      
      # player is just needed to correctly address the player in cat() 
      if (p_turn == 2){
        player <- 1
      }else{
        player <- 2
      }
      
      # only print the leaderboard if not all boxes are ticked already
      if (length(winning_boxes) != 0){
        cat("Player", player, "completed a square and takes another turn! Current leaderboard:", 
            "\n")
        # didn't get the df to be printed as desired hence this way
        cat("Player 1 Player 2", "\n")
        cat("      ", p1_score, "      ", p2_score, "\n")
        }
    }
  return_values <- c(p1_score, p2_score, p_turn, winning_boxes)
  return(return_values)

}

plot_line <- function(choice, p_turn, n_row, n_col){
  # unpack choice vector
  x_val <- as.numeric(choice[1])
  y_val <- as.numeric(choice[2])
  direction <- choice[3]
  
  # determine p_color (i.e. the line color of the segment)
  if (p_turn == 1){
    p_color <- "black"
  }else{
    p_color <- "red"}
  
  # determine endpoint of line
  if (direction == "r"){
    new_x_val <- x_val + 1
    new_y_val <- y_val
  }else{
    new_y_val <- y_val + 1
    new_x_val <- x_val
  }
  
  # mark the segment in the coordinate system accordingly
  # https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/segments 
  # on how to plot segments
  segments(x_val, transform_y(y_val, n_col), new_x_val, 
           transform_y(new_y_val, n_col), col = p_color)
  
}

win_box <- function(n_row, n_col){
  # generates all winning boxes based on user input
  winning_boxes <- list()
  # the consistency of this pattern allows me to later use it for plotting the 
  # symbol in completed boxes
  x_index <- 1
  while (x_index < n_col){
    y_index <- 1
    while (y_index < n_row){
      # define scoring pattern
      pattern_pt_1 <- c(x_index, y_index, "r")
      pattern_pt_2 <- c(x_index, y_index, "d")
      pattern_pt_3 <- c(x_index + 1, y_index, "d")
      pattern_pt_4 <- c(x_index, y_index + 1, "r")
      pattern = list(pattern_pt_1, pattern_pt_2, pattern_pt_3, pattern_pt_4)
      # create list of lists containing the winning pattern of 4 vectors per list
      winning_boxes <- c(winning_boxes, list(pattern))
      y_index <- y_index + 1
    }
    x_index <- x_index + 1
  }
  return(winning_boxes)
}

dots_and_boxes <- function(n_row = 3, n_col = 3){
  # set some parameters
  # max_moves formula was determined using ChatGPT
  max_moves <- (n_row - 1) * n_col + (n_col - 1) * n_row
  current_move <- 1
  drawn_lines <- list()
  winning_boxes <- win_box(n_row, n_col)
  p1_score <- 0
  p2_score <- 0
  
  # define which player starts randomly
  p_start <- rbinom(1, 1, 0.5)
  if (p_start == 0){
    p_turn <- 1
  }else{
    p_turn <- 2
  }
  cat("Player", p_turn, "starts!", "\n")
  cat("In each move you have to choose a dot and a direction (d: down, r: right).", 
      "\n")
  
  # plot the playing canvas  
  create_canvas(n_row, n_col)
  
  # allow for cell selection only if the max number of moves has not been surpassed yet
  while (current_move <= max_moves){
    # safe validated choice
    choice <- move_check(p_turn, n_row, n_col)
  
    # keep double entries in while loop until new entry happens (compare choice
    # with already drawn lines)
    # information on how to use taken from https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/identical
    # in combination with https://www.statology.org/all-any-r-function/ for any()
    while (any(sapply(drawn_lines, identical, choice))){
      cat("Vector exists in the list.", "\n")
      choice <- move_check(p_turn, n_row, n_col)
    }
  
    # mark line in coordinate system
    plot_line(choice, p_turn, n_row, n_col)
  
    # add line to existing list for tracking
    drawn_lines <- c(drawn_lines, list(choice))
    
    # check if move lead to a point for the player
    new_scores <- point_check(drawn_lines, winning_boxes, p1_score, p2_score, 
                              p_turn, n_col)
    p1_score <- new_scores[1]
    p2_score <- new_scores[2]
    p_turn <- new_scores[3]
    # remaining possibilities for points
    winning_boxes <- new_scores[4:length(new_scores)] 

    # advance one move and switch players 
    current_move <- current_move + 1
    if (p_turn == 1)
      {p_turn <- 2}
    else
      {p_turn <- 1}
  }
  
  # check who won
  p1_score <- as.numeric(p1_score)
  p2_score <- as.numeric(p2_score)

  if (p1_score > p2_score){
    cat("Player 1 wins! Final leaderboard:", "\n")
  }else if (p1_score < p2_score){
    cat("Player 2 wins! Final leaderboard:", "\n")
  }else{
    cat("Player 1 and Player 2 are tied winners! Final leaderboard:", "\n")
  }
  cat("Player 1 Player 2", "\n")
  cat("      ", p1_score, "      ", p2_score, "\n")
}

dots_and_boxes()




