#########################################################
################# 1. feladat ############################
#########################################################

#kod
odd_mean <- function(x) {
  x<-c(x)
  y<-x[which(x %% 2 == 1)] 
  mean_y=mean(y)
  print(mean_y)
}

#tesztek
odd_mean(c(1, 2, 3, 4, 5, 6))
odd_mean(c(-3, -2, -1, 0, 1, 2, 3, 5, 7))


#########################################################
################# 2. feladat ############################
#########################################################

#kod
is.letter <- function(x) grepl("[[:alpha:]]", x)
encode_x<- function(x) {
  x<-tolower(x)
  added_value<-str_count( x, substr(x, 1, 1))
    for (i in 1:length(as.list(unique(strsplit(x, "")[[1]])))) {
      if(is.letter(as.list(unique(strsplit(x, "")[[1]]))[[i]])==TRUE) {
        if(match(as.list(unique(strsplit(x, "")[[1]]))[[i]], myLetters)+added_value<27) 
          {x<-gsub(as.list(unique(strsplit(x, "")[[1]]))[[i]], letters[match(as.list(unique(strsplit(x, "")[[1]]))[[i]], myLetters)+added_value], x)}
        else {x<-gsub(as.list(unique(strsplit(x, "")[[1]]))[[i]], letters[match(as.list(unique(strsplit(x, "")[[1]]))[[i]], myLetters)+added_value-26], x)}
      }
        
    }
  print(x)
}

#tesztek
encode_x("apple")
encode_x("apple alarm")
encode_x("zoo")
encode_x("APple alaRm")




#########################################################
################# 3. feladat ############################
#########################################################

################ pick a random card #####################

card<-function(x) {
  x=1
  color<-sample(c("clubs", "diamonds", "hearts", "spades"), 1)
  value<-sample(c("2", "3", "4", "5", "6", "7", "8", "9", "10", "king", "queen", "jack", "ace"), 1)
  fin<-paste(value, color, sep=" ")
  print(fin)
}

#teszt
card()


################ create random deck #####################

cards_create<-function(x) {
  data.frame(colors=sample(c("clubs", "diamonds", "hearts", "spades"), x, replace=TRUE),
             values=sample(c("2", "3", "4", "5", "6", "7", "8", "9", "10", "king", "queen", "jack", "ace"), x, replace=TRUE))
}

#teszt
cards_table<-cards_create(12)
cards_table


################ print deck summary #####################

Deck<-function(x) {
  a <- table(x$colors)
  ind1<- if ("clubs" %in% x$colors==TRUE)  {a[names(a)=="clubs"][[1]]} else {0}
  ind2<- if ("diamonds" %in% x$colors==TRUE)  {a[names(a)=="diamonds"][[1]]} else {0}
  ind3<- if ("hearts" %in% x$colors==TRUE)  {a[names(a)=="hearts"][[1]]} else {0}
  ind4<- if ("spades" %in% x$colors==TRUE)  {a[names(a)=="spades"][[1]]} else {0}
  text<-paste(nrow(x)," cards - ",ind1," Clubs, ",ind2," Diamonds, ",ind3," Hearts, ",ind4," Spades", sep="")
}

#teszt
deck<-Deck(cards_table)
deck


################ drawn a random card from deck ##########

deck.drawn<-function(x) {
  x<-paste(cards_table$values[1], cards_table$colors[1], sep=" ")
  cards_table<<-cards_table[-1,]
  x
}

#teszt
drawn<-deck.drawn(deck)
deck<-Deck(cards_table)
drawn
deck


################ shuffle the cards in deck #############

cards_table
deck.shuffle<-function(x) {
  cards_table <- cards_table [sample(1:nrow(cards_table)), ]
  cards_table
}

#teszt
deck.shuffle(cards_table)



