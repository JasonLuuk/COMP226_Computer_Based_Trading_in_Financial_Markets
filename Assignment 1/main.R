options(warn=-1)
args <- commandArgs(trailingOnly = TRUE); nargs = length(args)
log <- (nargs == 4) # TRUE is there are exactly 4 arguments

arg_format <- "<--log> <solution_path> <book_path> <messages_path>"

if (nargs < 3 || nargs > 4)  # check that there are 3 or 4 arguments
    stop(paste("main.R has 3 required arguments and 1 optional flag:", arg_format))

if (nargs == 4 && args[1] != "--log") # if 4 check that --log is the first
    stop(paste("Bad arguments format, expected:", arg_format))

solution_path <- args[nargs-2]
book_path     <- args[nargs-1]
messages_path <- args[nargs]

if (!all(file.exists(c(solution_path, book_path, messages_path))))
    stop("File does not exist at path provided.")

source(solution_path); source("common.R") # source common.R from pwd

book <- book.load(book_path)
book <- book.reconstruct(data.load(messages_path), init=book, log=log)
book.summarise(book)
