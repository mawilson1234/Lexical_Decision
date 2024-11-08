library(tidyverse)
library(data.table)

# User-defined function to read in PCIbex Farm results files
read.pcibex <- function(
	filepath, 
	colnames.from,
	auto.colnames = TRUE, 
	fun.col = \(col, cols) {
			cols[cols == col] <- paste(col, 'Ibex', sep = '.')
			return (cols)
		}
	) {
	
	get.colnames <- function(filepath) {
		cols <- c()
		con <- file(filepath, 'r')
		while (TRUE) {
			line <- readLines(con, n = 1, warn = FALSE)
			if (length(line) == 0) {
				break
			}
			
			m <- regmatches(line, regexec(r'(^# (\d+)\. (.+)\.,*?$)', line))[[1]]
			if (length(m) == 3) {
				index <- as.numeric(m[2])
				value <- m[3]
				if (is.function(fun.col)) {
					cols <- fun.col(value, cols)
				}
				cols[index] <- value
				if (index == n.cols) {
					break
				}
			}
		}
		close(con)
		
		return (cols)
	}
	
	n.cols <- max(count.fields(filepath, sep = ',', quote = NULL), na.rm = TRUE)
	if (auto.colnames) {
		cols <- get.colnames(filepath)
		
		# this happens due to a data coding errors we've fixed
		if (is.null(cols)) {
			cols <- get.colnames(colnames.from)
		}
		
		return (read.csv(filepath, comment.char = '#', header = FALSE, col.names = cols))
	}
	else {
		return (read.csv(filepath, comment.char = '#', header = FALSE, col.names = seq_len(n.cols)))
	}
}

read.pcibex('results.csv') |>
	as_tibble() |>
	mutate(
		participant = paste0(Results.reception.time, MD5.hash.of.participant.s.IP.address),
		participant = match(participant, unique(participant))
	) |>
	select(-Results.reception.time, -MD5.hash.of.participant.s.IP.address) |>
	select(participant, everything()) |> 
	filter(PennElementName == 'response') |>
	select(participant, Value, word, frequency, RT) |>
	rename(
		response = Value,
		response_time = RT
	) |>
	mutate(
		response = case_when(
			response == 'F' ~ 'word',
			response == 'J' ~ 'nonword'
		),
		accuracy = case_when(
			response == 'word' & frequency != 'NONWORD' ~ 1,
			response == 'nonword' & frequency == 'NONWORD' ~ 1,
			TRUE ~ 0
		)
	) |>
	select(
		participant, frequency, word, 
		response, response_time, accuracy
	) |> 
	fwrite('cleaned_results.csv', row.names = FALSE)