#From https://static1.squarespace.com/static/512280dee4b0b5151b75de25/t/51ae6c73e4b0d19bff6e01e8/1370385523525/4and6LetterCodes.pdf

library(stringr)

ff <- function(long){
  count <- str_count(str_trim(long), "[\\s-]")+1

  short <- tolower(long)

  # Note that four of the nine collisions in the 6-letter code scheme occur
  # in the warbler group, specifically Blackpoll/Blackburnian (BLPOWA/BLBUWA),
  # Black-throated Green/Black-throated Gray (BTGNWA/BTGYWA),
  # Pallas’ Warbler (an old world vagrant to Alaska)/
  # Palm Warbler (PALLWA/PALMWA) and
  # Golden-cheeked/Golden-crowned (GOCWAR/GOCRWA).

  short[long=="Blackpoll Warbler"] <- "blpowa"
  short[long=="Blackburnian Warbler"] <- "blbuwa"
  short[long=="Black-throated Green Warbler"] <- "btgnwa"
  short[long=="Black-throated Gray Warbler"] <- "btgywa"
  short[long=="Pallas’ Warbler"] <- "pallwa"
  short[long=="Palm Warbler"] <- "palmwa"
  short[long=="Golden-cheeked Warbler"] <- "gocwar"
  short[long=="Golden-crowned Warbler"] <- "gocrwa"
  count[long=="Blackpoll Warbler"] <- 5
  count[long=="Blackburnian Warbler"] <- 5
  count[long=="Black-throated Green Warbler"] <- 5
  count[long=="Black-throated Gray Warbler"] <- 5
  count[long=="Pallas’ Warbler"] <- 5
  count[long=="Palm Warbler"] <- 5
  count[long=="Golden-cheeked Warbler"] <- 5
  count[long=="Golden-crowned Warbler"] <- 5

  # Some similar-color names are abbreviated in fixed ways: green is GRN or GN;
  # gray is GRY or GY; black is BLK or BK; blue is BLU or BU; and brown is BRN
  # or BN (Brown-crested Flycatcher = BNCFLY;
  # Black-throated Green Warbler = BTGNWA; Black-throated Gray Warbler = BTGYWA)
  str_trunc2 <- function(x){
    if(x == "black") return("bk")
    if(x == "green") return("gn")
    if(x == "gray") return("gy")
    if(x == "blue") return("bu")
    if(x == "brown") return("bn")
    str_trunc(x, width=2, ell="")
  }
  str_trunc3 <- function(x){
    if(x == "black") return("blk")
    if(x == "green") return("grn")
    if(x == "gray") return("gry")
    if(x == "blue") return("blu")
    if(x == "brown") return("brn")
    str_trunc(x, width=3, ell="")
  }

  # One-word Names: first 6 letters of the name
  # (ex. Redhead = REDHEA; Osprey = OSPREY; Sora = SORA)
  short[count==1] <- str_trunc(short[count==1], 6, ell="")

  # Two-word Names: first three letters of first word and first three letters
  # of last word.  Hyphenated words always treated as separate words (ex.
  # Common Yellowthroat = COMYEL; Storm-petrel = STOPET;
  # Painted Redstart = PAIRED)
  twos <- short[count==2]
  twos <- str_split(twos, "[\\s-]")
  f <- function(x){
    paste(str_trunc3(x[1]),
          str_trunc3(x[2]), sep="")
  }
  twos <- lapply(twos, f)
  short[count==2] <- unlist(twos)

  # Three-word Names: two letters from first word, one from second, three
  # from third:Blue-winged Warbler = BLWWAR; Wilson’s Storm-petrel = WISPET;
  # Dark-eyed Junco = DAEJUN)
  threes <- short[count==3]
  threes <- str_split(threes, "[\\s-]")
  f <- function(x){
    paste(str_trunc2(x[1]),
          str_trunc(x[2], width=1, ell=""),
          str_trunc3(x[3]), sep="")
  }

  threes <- lapply(threes, f)
  short[count==3] <- unlist(threes)


  # Four-or-more-word Names:  one letter from each first three words, then
  # first three letters of last word (ex. Great black-backed Gull = GBBGUL;
  # Black-throated Blue Warbler = BTBWAR; Yellow-crowned Night Heron = YCNHER)
  fours <- short[count==4]
  fours <- str_split(fours, "[\\s-]")
  f <- function(x){
    paste(str_trunc(x[1], width=1, ell=""),
          str_trunc(x[2], width=1, ell=""),
          str_trunc(x[3], width=1, ell=""),
          str_trunc3(x[4]), sep="")
  }

  fours <- lapply(fours, f)
  short[count==4] <- unlist(fours)


  return(short)
}
