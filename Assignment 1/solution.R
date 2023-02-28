book.total_volumes <- function(book) {
  total_volumes <- list(ask = sum(book$ask$size),bid = sum(book$bid$size))
  return(total_volumes)
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The total volume in the book.
}

book.best_prices <- function(book) {
  best_prices <- list(ask = min(book$ask$price),bid = max(book$bid$price))
  return(best_prices)
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   A list with "ask" and "bid", the values of which are the best prices in
    #       the book.
}

book.midprice <- function(book) {
  mid_price <- (min(book$ask$price) + max(book$bid$price))/2
  return(mid_price)
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The midprice of the book.
}

book.spread <- function(book) {
  spread <- (min(book$ask$price) - max(book$bid$price))
  return(spread)
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The spread of the book.
}

book.add <- function(book, message) {
  original_ask_book  <- book$ask
  original_bid_book  <- book$bid
  if(message$side == 'S')
  {
    if(message$price <= max(book$bid$price))
    {
      repeat{
        difference <- original_bid_book[which(book$bid$price == max(book$bid$price))[1],]$size - message$size
        if(difference < 0)
        {
          original_bid_book <-  original_bid_book[-c(which(original_bid_book$price == max(book$bid$price))[1]),]
          difference <- -difference
          message$size = difference
          book$bid <- original_bid_book
          if(message$price > max(book$bid$price)){
            if (length(original_ask_book$price) == 0)
            {
              original_ask_book[1,] <- c(message$oid,message$price,message$size)
            }
            else
            {
              original_ask_book <- rbind(original_ask_book,c(message$oid,message$price,message$size))
            }
            original_ask_book$price  <-  sapply(original_ask_book$price,as.numeric)
            original_ask_book$size <- sapply(original_ask_book$size,as.numeric)
            book$ask <- original_ask_book
            break
          }
        }
        else if(difference == 0)
        {
          original_bid_book <-  original_bid_book[-c(which(original_bid_book$price == max(book$bid$price))[1]),]
          book$bid <- original_bid_book
          break
        }
        else
        {
          original_bid_book[which(book$bid$price == max(book$bid$price))[1],]$size <- difference
          book$bid <- original_bid_book
          break
        }
      }
    }
    else
    {
      if (length(original_ask_book$price) == 0)
      {
        original_ask_book[1,] <- c(message$oid,message$price,message$size)
      }
      else
      {
        original_ask_book <- rbind(original_ask_book,c(message$oid,message$price,message$size))
      }
      original_ask_book$price  <-  sapply(original_ask_book$price,as.numeric)
      original_ask_book$size <- sapply(original_ask_book$size,as.numeric)
      book$ask <- original_ask_book
    }
  }
  else
  {
    if(message$price >= min(book$ask$price))
    {
      repeat{
        difference <- original_ask_book[which(book$ask$price == min(book$ask$price))[1],]$size - message$size
        if(is.na(original_ask_book[which(book$ask$price == min(book$ask$price))[1],]$size))
        {
          if (length(original_bid_book$price) == 0)
          {
            original_bid_book[1,] <- c(message$oid,message$price,message$size)
          }
          else
          {
            original_bid_book <- rbind(original_bid_book,c(message$oid,message$price,message$size))
          }
          original_bid_book$price  <-  sapply(original_bid_book$price,as.numeric)
          original_bid_book$size <- sapply(original_bid_book$size,as.numeric)
          book$bid <- original_bid_book
          break
        }
        else if(difference < 0 )
        {
          original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min(book$ask$price))[1]),]
          difference <- -difference
          message$size = difference
          book$ask <- original_ask_book
          if(length(is.infinite(book$ask$price)) == 0)
          {
            
          }
          else if(message$price < min(book$ask$price)){
            if (length(original_bid_book$price) == 0)
            {
              original_bid_book[1,] <- c(message$oid,message$price,message$size)
            }
            else
            {
              original_bid_book <- rbind(original_bid_book,c(message$oid,message$price,message$size))
            }
            original_bid_book$price  <-  sapply(original_bid_book$price,as.numeric)
            original_bid_book$size <- sapply(original_bid_book$size,as.numeric)
            book$bid <- original_bid_book
            break
          }
        }
        else if(difference == 0)
        {
          original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min(book$ask$price))[1]),]
          book$ask <- original_ask_book
          break
        }
        else
        {
          original_ask_book[which(book$ask$price == min(book$ask$price))[1],]$size <- difference
          book$ask <- original_ask_book
          break
        }
      }
    }
    else
    {
      if (length(original_bid_book$price) == 0)
      {
        original_bid_book[1,] <- c(message$oid,message$price,message$size)
      }
      else
      {
        original_bid_book <- rbind(original_bid_book,c(message$oid,message$price,message$size))
      }
      original_bid_book$price  <-  sapply(original_bid_book$price,as.numeric)
      original_bid_book$size <- sapply(original_bid_book$size,as.numeric)
      book$bid <- original_bid_book
    }
  }
  
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid", "side", "price" and "size" entries.
    #
    # Returns:
    #   The updated book.
    return(book)
}

book.reduce <- function(book, message) {
  original_ask_book  <-  book$ask
  original_bid_book  <-  book$bid
  if (length(which(original_ask_book$oid == message$oid)) == 0)
  {
    if (length(which(original_bid_book$oid == message$oid)) == 0)
    {
      
    }
    else
    {
      original_size <-  original_bid_book[which(original_bid_book$oid == message$oid),]$size
      if (original_size - message$amount > 0)
      {
        original_bid_book[which(original_bid_book$oid == message$oid),]$size <- original_size - message$amount
      }
      else
      {
        original_bid_book <-  original_bid_book[-c(which(original_bid_book$oid == message$oid)),]
      }
      book$bid  <-  original_bid_book
    }
  }
  else
  {
    original_size <-  original_ask_book[which(original_ask_book$oid == message$oid),]$size
    if (original_size - message$amount > 0)
    {
      original_ask_book[which(original_ask_book$oid == message$oid),]$size <- original_size - message$amount
    }
    else
    {
      original_ask_book <-  original_ask_book[-c(which(original_ask_book$oid == message$oid)),]
    }
    book$ask  <-  original_ask_book
  }
    return(book)    
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  #   message - A list containing "oid" and "amount".
  #
  # Returns:
  #   The updated book.
  
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
  M = sum(book$ask$size)
  if( size == M) {
    return(NA)
  }
  all_price = c()
  price_set <- unique(book$ask$price)
  for (price in price_set){
    ask_book <- book$ask
    message = list(oid='test111',side='B',size=size,price=price)
    book_midprice <- book.midprice(book.add(book,message))
    all_price = append(all_price,book_midprice)
  }
  return (mean(all_price))
  # See handout for instructions
 
}

book.extra2 <- function(book, size) {
  M = sum(book$ask$size)
  if( size == M) {
    return(NA)
  }
  all_price = c()
  price_set <- unique(book$ask$price)
  for (price in min(price_set):max(price_set)){
    ask_book <- book$ask
    message = list(oid='test1',side='B',size=size,price=price)
    book_midprice <- book.midprice(book.add(book,message))
    all_price = append(all_price,book_midprice)
  }
  return (mean(all_price))
}

book.extra3 <- function(book) {
    # See handout for instructions
  M = sum(book$ask$size)
  all_price = c()
  for (size in 1:(M-1)){
    original_ask_book = book$ask
    copy_book <- book
    repeat{
      if(length(original_ask_book$size)==0){break}
      min_price <- min(original_ask_book$price)
      if(size==0){break}
      else if(size == original_ask_book[which(original_ask_book$price == min_price)[1],]$size){
        original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min_price)[1]),]
        copy_book$ask <- original_ask_book
        break
      }
      else if(size > original_ask_book[which(original_ask_book$price == min_price)[1],]$size){
        size = size - original_ask_book[which(original_ask_book$price == min_price)[1],]$size
        original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min_price)[1]),]
        copy_book$ask <- original_ask_book
      }
      else{
        break
      }
    }
    book_midprice <- book.midprice(copy_book)
    all_price = append(all_price,book_midprice)
  }
  return(mean(all_price))
  
}

book.extra4 <- function(book, k) {
    # See handout for instructions
  
  if(length(book$ask$size) == 0){
    return(0)
  }
  else{
    flag = 0
    total_ask_max_volume <- sum(book$ask$size)
    final_midprice <- book.midprice(book)*(1+k/100)
    final_size <- 0
    all_size <- c()
    for(size in 0 :(total_ask_max_volume-1)){
      original_ask_book = book$ask
      copy_book <- book
      repeat{
        all_size = append(all_size,size)
        min_price <- min(original_ask_book$price)
        if(size==0){break}
        else if(size == original_ask_book[which(original_ask_book$price == min_price)[1],]$size){
          original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min_price)[1]),]
          copy_book$ask <- original_ask_book
          break
        }
        else if(size > original_ask_book[which(original_ask_book$price == min_price)[1],]$size){
          size = size - original_ask_book[which(original_ask_book$price == min_price)[1],]$size
          original_ask_book <-  original_ask_book[-c(which(original_ask_book$price == min_price)[1]),]
          copy_book$ask <- original_ask_book
        }
        else{
          break
        }
      }
      if(book.midprice(copy_book)<=final_midprice){
        final_size <- max(all_size)
        flag = 1
      }
  
    }
    return(final_size)
  }

}