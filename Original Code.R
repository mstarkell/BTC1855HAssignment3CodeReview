# Shawn Chang
# Assignment 3

# 1. Loading and sampling from the dictionary ####
dictionary <- read.table("dictionary.txt")  # this is needed to read and load the dictionary (from which the secret keyword is selected)
keyword <- sample(dictionary$V1, 1)         # this is needed to sample a random word from the dictionary (column reference is V1 as default)

# 2. Prompting the player to start the game ####
print(paste("Welcome to Hangman! This word has", nchar(keyword), "letters. Type 1 letter to begin your guess."))
print("You are allowed 10 guesses. Note that you will LOSE a try for guessing the same letter again. After all, with each new guess you make, you'll be given an updated list of all letters you've inputted. This is to test your attention to detail.")
# nchar() is needed to determine and display the number of characters of the word
# these print() functions are needed so instructions and rule are displayed in messages

# 3. Setting the conditions for when the game starts/restarts ####
i <- 10               # this is an arbitrary value to concisely represent the number of tries left (10 at the beginning of the game)

right_guesses <- c()  # this creates a new vector, allowing us to later update the repertoire of guesses the player has already correctly made with each new guess
wrong_guesses <- c()  # this creates a new vector, allowing us to later update the repertoire of guesses the player has already incorrectly made with each new guess

# 4. Creating the loop for the game to take place continuously until the player wins or loses ####
while(i>0){           # setting i as greater than 0; this way, the reader will stay in the game (from being prompted to enter a letter to winning or losing) until they run out of tries (where i would equal 0, meaning 0 tries left)
  
  repeat{
    guess <- readline(prompt = "Please enter a letter: ")   # this is needed to prompt the player to enter a letter (denoted by "guess")
    if ((grepl("[A-Za-z]", guess)) &                        # this is needed to validate if the input is a letter from a to z (upper and lower case okay)
        (nchar(guess) == 1)){                               # this is needed to check that the input is exactly 1 letter
      break                                                 # this is needed to let the player exit the repeated prompt (i.e., to enter a letter) only after they've met the above 2 conditions
    }
  }
  
  guess <- as.character(guess)                              # this is needed to ensure the input is a character so it matches the letters in the keyword (also characters)
  
  guess_nocaps <- tolower(guess)                            # this is needed to set the inputted letter in the lower case so it matches those in the secret keywords in the dictionary
  
  keyword <- as.character(keyword)                          # this is needed to ensure the secret keyword sampled from the dictionary also comprises characters
  
  keyword_letters <- unlist(strsplit(keyword, ""))          # this is needed to split the keyword into its constituent letters so we can check if the inputted letter is or is not one from in the secret keyword
  
  right_guesses <- unique(append(right_guesses, guess_nocaps[guess_nocaps %in% keyword_letters]))  # this is needed to update the vector of correctly guessed letters after the player makes a new correct guess so we can later check if they guessed all the letters in the keyword (and therefore win) and display, after guesses, all letters correctly guessed so far
  wrong_guesses <- unique(append(wrong_guesses, guess_nocaps[!guess_nocaps %in% keyword_letters])) # this is needed to update the vector of incorrectly guessed letters after the player makes a new incorrect guess so we can display, after the guess is made, all letters incorrectly guessed so far
  
  if(guess_nocaps %in% keyword_letters){
    print(paste("Correct.", guess_nocaps, "is in spot", which(guess_nocaps == keyword_letters), "of the secret word. Great work. You have", i, "tries left."))
    print(paste("Your correctly guessed letters include:"))     # this line and the line above are needed to list all the correct guesses made so far in the game
    print(right_guesses)
    if(!i==10){
      print(paste("Your incorrectly guessed letters include:")) # we need this inner if block so we don't display these contained messages if the user has not guessed any letters incorrectly so far in the game (we only display if the player guessed incorrectly at least once, meaning that i, or the number of tries left, would no longer equal 10)
      print(wrong_guesses)
    }
  } else {
    i <- i-1
    print(paste(guess_nocaps, "is not in the secret word. You have", i, "tries left."))
    print(paste("Your incorrectly guessed letters include:"))   # this line and the line above are needed to list all the incorrect guesses made so far in the game
    print(wrong_guesses)
    if(length(right_guesses) != 0){
      print(paste("Your correctly guessed letters include:"))
      print(right_guesses)                                            # we need this inner if block so we don't display these contained messages if the user has not guessed any correct letters so far in the game (in which case the length of the vector of correct guesses is 0); we only display these messages if the player guessed correctly at least once
    }
  }
  # the above if-else block (the outer if...else) is needed to check if the guessed letter is in the secret keyword
  # if it is, we want to a) tell the player they guessed correctly and b) use the which() function to tell them the spot (through indexing) in the keyword that the letter is in (e.g., "h" is in spot 2 of the keyword "shield"); we also tell them the identities of correctly and/or incorrectly guessed letters after guesses
  # if it is not, we want to tell the player they did not guess correctly and deduct a try each time they input a letter not found in the keyword; we also tell them the identities of correctly and/or incorrectly guessed letters after guesses
  
  
  if(all(keyword_letters %in% right_guesses)){
    print(paste("You won!", keyword, "is indeed the secret word!"))
    break
  }
  # the above if block is needed to check if the player has guessed all the letters in the secret keyword, and if so, instantly take them out of the loop, telling them they won, and display the "Play again" message, bypassing the below if block
  
  if(i == 0){
    print(paste0("You lost. The secret word was ", keyword, "."))
  }
  # the above if block is needed for the case where the player used up all their tries, inputting letters not in the secret keyword (in other words, i is 0 since i means the number of tries left) and so loses the game
  # paste0() is chosen as we don't want a space between keyword and the period in the displayed message
} # the while loop ends here as we only want the components inside the loop to repeat (and only display the next and final message when the player has won or lost)
print("Play again.") # this is displayed to prompt the reader to play another round regardless of whether they win or lose