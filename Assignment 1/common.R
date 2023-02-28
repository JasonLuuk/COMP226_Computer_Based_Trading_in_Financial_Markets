book.handle <- function(book, row) {
    if (row$type == 'A')
        return(book.add(book, list(
            oid=row$oid,
            side=row$side,
            price=as.numeric(row$price),
            size=as.numeric(row$size)
        )))
    else if (row$type == 'R')
        return(book.reduce(book, list(
            oid=row$oid,
            amount=as.numeric(row$size)
        )))
    else {
        warn("Unknown row type.")

        return(book)
    }
}

book.load <- function(path) {
    df <- read.table(
        path, fill=NA, stringsAsFactors=FALSE, header=TRUE, sep=','
    )

    book.sort(list(
        ask=df[df$side == "S", c("oid", "price", "size")],
        bid=df[df$side == "B", c("oid", "price", "size")]
    ))
}

book.summarise <- function(book, with_stats=T) {
    if (nrow(book$ask) > 0)
        book$ask <- book$ask[nrow(book$ask):1,]

    book$ask <- book$ask[, c("oid", "price", "size")]
    book$bid <- book$bid[, c("oid", "price", "size")]

    print(book)

    if (with_stats) {
        clean <- function(x) { ifelse(is.infinite(x), NA, x) }

        total_volumes <- book.total_volumes(book)
        best_prices <- lapply(book.best_prices(book), clean)
        midprice <- clean(book.midprice(book))
        spread <- clean(book.spread(book))

        cat("Total volume:", total_volumes$bid, total_volumes$ask, "\n")
        cat("Best prices:", best_prices$bid, best_prices$ask, "\n")
        cat("Mid-price:", midprice, "\n")
        cat("Spread:", spread, "\n")
    }
}

book.sort <- function(book, sort_bid=T, sort_ask=T) {
    if (sort_ask && nrow(book$ask) >= 1) {
        book$ask <- book$ask[order(book$ask$price,
                                   nchar(book$ask$oid),
                                   book$ask$oid,
                                   decreasing=F),]
        row.names(book$ask) <- 1:nrow(book$ask)
    }

    if (sort_bid && nrow(book$bid) >= 1) {
        book$bid <- book$bid[order(-book$bid$price,
                                   nchar(book$bid$oid),
                                   book$bid$oid,
                                   decreasing=F),]
        row.names(book$bid) <- 1:nrow(book$bid)
    }

    book
}

book.init <- function() {
    book <- list(
         ask=data.frame(matrix(ncol=3, nrow=0)),
         bid=data.frame(matrix(ncol=3, nrow=0))
    )

    colnames(book$ask) <- c("oid", "price", "size")
    colnames(book$bid) <- c("oid", "price", "size")

    return(book)
}

book.reconstruct <- function(data, init=NULL, log=F) {
    if (is.null(init)) init <- book.init()
    if (nrow(data) == 0) return(init)

    book <- Reduce(
         function(b, i) {
             new_book <- book.handle(b, data[i,])
             if (log) {
                 cat("Step", i, "\n\n")
                 book.summarise(new_book, with_stats=F)
                 cat("====================\n\n")
             }
             new_book
         },
         1:nrow(data), init,
    )

    book.sort(book)
}

data.load <- function(data_path, n_rows=-1) {
    data <- read.table(
        data_path,
        fill=NA,
        stringsAsFactors=FALSE,
        col.names=c("type", "oid", "side", "price", "size"),
        nrows=n_rows,
    )

    data[data$type == 'R', "size"] <- data[data$type == 'R', "side"]
    data[data$type == 'R', "side"] <- NA

    data
}
