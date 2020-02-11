library(httr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)

lytics_colors <- c(
	"brightblue"  = "#0093D0",
	"darkblue"    = "#262D37",
	"brightgreen" = "#39B43F",
	"gray"        = "#ACB8BC",
	"subnav"      = "#313842",
	"white"       = "#FFFFFF",
	"darktext"    = "#1B1B1B",
	"orange"      = "#FF3100",
	"red"         = "#FF1300",
	"purple"      = "#605CA8",
	"gold"        = "#FFB700"
)

lytics_theme <- function() {
	library(ggplot2)

	# Generate the colors for the chart procedurally with RColorBrewer

	brightblue_palette <- colorRampPalette(c(lytics_colors["white"], lytics_colors["brightblue"]))(9)
	darkblue_palette <- colorRampPalette(c(lytics_colors["brightblue"], lytics_colors["darkblue"]))(9)

	color.background = lytics_colors["white"] # palette[2]
	color.grid.major = lytics_colors["gray"] # palette[3]
	color.axis.text = lytics_colors["darktext"] # palette[6]
	color.axis.title = darkblue_palette[7]
	color.title = lytics_colors["darkblue"]

	# Begin construction of chart
	theme_bw(base_size = 12) +

	# use proxima nova
	theme(text = element_text(family = "Helvetica")) +
	theme(plot.title = element_text(size = 18, face = "bold")) +

	# Set the entire chart region to a light gray color
	theme(panel.background = element_rect(fill = color.background, color = color.background)) +
	theme(plot.background = element_rect(fill = color.background, color = color.background)) +
	theme(legend.background = element_rect(fill = color.background, color = color.background)) +
	theme(panel.border = element_rect(color = color.background)) +
	theme(strip.background = element_rect(colour = color.background, fill = lytics_colors['brightblue'])) +

	# Format the grid
	theme(panel.grid.major = element_line(color = paste(color.grid.major, "66", sep = ""), size = 0.3)) +
	theme(panel.grid.minor = element_line(color = paste(color.grid.major, "33", sep = ""), size = 0.15)) +
	#theme(panel.grid.minor = element_blank()) +
	theme(axis.ticks = element_blank()) +

	# Format the legend, but hide by default
	theme(legend.background = element_rect(fill = color.background, color = color.background)) +
	theme(legend.key = element_rect(fill = color.background, color = color.background)) +
	theme(legend.text = element_text(size = 10, color = color.axis.title)) +

	# default lines to bright blue
	theme(line = element_line(colour = lytics_colors["brightgreen"], size = 2)) +
	theme(rect = element_rect(fill = lytics_colors["brightblue"])) +

	# Set title and axis labels, and format these and tick marks
	theme(plot.title = element_text(color = color.title, vjust = 5)) +
	theme(axis.text.x = element_text(size = 10, color = color.axis.text)) +
	theme(axis.text.y = element_text(size = 10, color = color.axis.text)) +
	theme(axis.title.x = element_text(size = 14, color = color.axis.title, vjust = -1.0)) +
	theme(axis.title.y = element_text(size = 14, color = color.axis.title, vjust = 2.0)) +

	# Plot margins
	theme(plot.margin = unit(c(2, 1, 1, 1), "cm"))
}

set_lytics_theme <- function() {
	# use set the lytrbox::lytics_theme
	theme_set(lytics_theme())

	# some aesthetics can't be specified in a theme :(
	update_geom_defaults("point", list(fill = lytics_colors["brightblue"], color = lytics_colors["brightblue"]))
	update_geom_defaults("bar", list(fill = lytics_colors["brightblue"], alpha = 0.8))
	update_geom_defaults("line", list(fill = lytics_colors["brightblue"]))
	update_geom_defaults("smooth", list(colour = lytics_colors["brightblue"], fill = lytics_colors["gray"], size = 1))
	update_geom_defaults("density", list(colour = lytics_colors["darkblue"], fill = lytics_colors["gray"], size = 1))
	update_geom_defaults("boxplot", list(fill = lytics_colors["brightblue"], alpha = 0.8))
	update_geom_defaults("violin", list(fill = lytics_colors["brightblue"], alpha = 0.8))

	update_stat_defaults("smooth", list(size = 2)) # doesn't work
}


plot.colors <- c(lytics_colors["brightblue"], lytics_colors["brightgreen"], lytics_colors["gold"], lytics_colors["darkblue"])
names(plot.colors) <- NULL # ggplot doesn't like named color vectors

num.breaks = 20


list.to.idlist <- function(items, name = "name", id = "id") {
	result <- list()
	for(item in items) {
		if(item[[name]] == "" | is.null(item[[name]])) {
			next
		}
		result[[item[[name]]]] <- item[[id]]
	}
	result <- unlist(result)
	return(result[order(names(result))])
}

camel.case <- function(x) {
	return(unlist(lapply(strsplit(x, " "), function(s){
		return(paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse = " "))
	})))
}

split.parts <- function(parts, category = "name") {
	parts <- camel.case(parts)
	if(any(grepl("Lql", parts))) {
		if(category == "name") {
			return(paste(parts, collapse = " "))
		} else {
			return("Custom Field")
		}
	} else if(parts[1] == "Aspect") {
		if(category == "name") {
			return(paste(parts[2], paste(parts[3:length(parts)], collapse = " "), sep = ": "))
		} else {
			return("Aspect")
		}
	} else if(parts[1] == "Lytics") {
		if(category == "name") {
			return(parts[3])
		} else {
			return("Score")
		}
	} else {
		if(category == "name") {
			return(paste(parts[2:length(parts)], collapse = " "))
		} else {
			return("Content")
		}
	}
}

hasPrefix <- function(s, prefix) {
	if (is.na(s)) return (FALSE)
	if (is.na(prefix)) return (FALSE)

	pref <- strtrim(s, nchar(prefix))
	return (pref == prefix)
}

accuracy <- function(model) {
	return( (model$summary$conf$TruePositive + model$summary$conf$TrueNegative) /sum(unlist(model$summary$conf)))
}

sensitivity <- function(model) {
	summ <- model$summary$conf
	return( summ$TruePositive / (summ$TruePositive + summ$FalseNegative) )
}

specificity <- function(model) {
	summ <- model$summary$conf
	return ( summ$TrueNegative / (summ$TrueNegative + summ$FalsePositive) )
}

confusion <- function(model) {
	tmp <- model$summary$conf
	m <- matrix(c(tmp$TrueNegative, tmp$FalsePositive, tmp$FalseNegative, tmp$TruePositive), 2, 2)
	rownames(m) <- c("Predicted 0", "Predicted 1")
	colnames(m) <- c("Actual 0", "Actual 1")
	return(m)
}

plot.partial.dependencies <- function(resp) {
	flattened <- lapply(resp$fields, function(x) do.call(rbind, x))
	clean <- list()
	for (name in names(flattened)) {
		df <- as.data.frame(flattened[[name]])
		df$field = name
		clean[[name]] = as.data.frame(df)
	}

	clean <- do.call(rbind, clean)
	colnames(clean) <- c("value", "pred", "field")
	rownames(clean) = NULL
	clean$value <- as.numeric(as.character(unlist(clean$value)))
	clean$pred <- as.numeric(as.character(unlist(clean$pred)))

	p <- ggplot(clean, aes(x = value, y = pred, colour = field)) + geom_line(size = 1.5)
	return(p)
}

extract.dbl <- function(x, default = 0, digits = 4) {
	if(is.null(x) || length(x) == 0) {
		return(default)
	}
	return(round(x, digits))
}

extract.chr <- function(x, default = "") {
	if(is.null(x) || length(x) == 0) {
		return(default)
	}
	return(x)
}

importance.table <- function(models, model.name) {
	model <- models[[model.name]]
	df <- data.frame(
		name = map_chr(model$features, ~ extract.chr(.x$name)),
		importance = map_dbl(model$features, ~ extract.dbl(.x$importance)),
		type = map_chr(model$features, ~ extract.chr(.x$kind)),
		lift = map_dbl(model$features, ~ extract.dbl(.x$impact$lift)),
		correlation = map_dbl(model$features, ~ extract.dbl(.x$correlation))
	)
	return (df[rev(order(df$importance)), ])
}

plot.importance <- function(models, model.name, method = c("cloud", "gini"), threshold = 0) {
	method <- match.arg(method)

	model <- models[[model.name]]

	if(!is.null(model$features)) {
		importance.df <- data.frame(
			name = map_chr(model$features, ~ .x$name),
			value = map_dbl(model$features, ~ .x$importance),
			type = map_chr(model$features, ~ .x$kind)
		)
		importance.df <- importance.df[importance.df$value > threshold,]
	} else {
		importance <- unlist(model$importance)
		importance <- importance[importance > threshold]

		splits <- strsplit(names(importance), "_")
		types <- unlist(lapply(splits, split.parts, category = "type"))
		name <- gsub("NA ", "", unlist(lapply(splits, split.parts, category = "name")))

		importance.df <- data.frame(
			name = name,
			value = importance,
			type = types
		)
	}

	importance.sorted.df <- transform(importance.df, name = reorder(name, value))

	plot.colors <- c(lytics_colors["brightblue"], lytics_colors["brightgreen"], lytics_colors["darkblue"], lytics_colors["gold"])
	names(plot.colors) <- NULL

		p <- ggplot(importance.sorted.df, aes(x = name, y = value, fill = type)) +
		geom_bar(stat = "identity") + coord_flip() +
		scale_fill_manual(values = plot.colors) +
		labs(title = paste("SegmentML Variable Importance"), subtitle = model.name, x = "Feature", y = "Importance")
	return(p)
}

threshold.gradient <- function(input, operator = `<=`) {
	values <- seq(0, 1, by = 0.01)
	threshold <- rep(0, length(values))

	i <- 1
	input.values <- names(input)
	for(value in values) {
		index <- operator(as.numeric(input.values), value)
		if(sum(index)) {
			threshold[i] <- sum(input[input.values[index]]) / sum(input)
		}
		i <- i + 1
	}
	names(threshold) <- values
	return(threshold)
}

plot.thresholds <- function(models, model.name) {
	model <- models[[model.name]]
	success <-threshold.gradient(unlist(model$summary$success), `<`)
	failure <- threshold.gradient(unlist(model$summary$fail), `>`)

	threshold.df <- rbind.data.frame(
		data.frame(
			threshold = as.numeric(names(success)),
			value = success,
			error = "False Negative"
		),
		data.frame(
			threshold = as.numeric(names(failure)),
			value = failure,
			error = "False Positive"
		)
	)

	p <- ggplot(threshold.df, aes(x = threshold, y = value, color = error)) + geom_line(size = 1.5) +
		scale_color_manual(values = as.vector(lytics_colors[c("purple", "brightgreen")], mode = "character")) +
		labs(x = "Decision Threshold", y = "Error Rate", title = "Decision Thresholds") +
		geom_vline(xintercept = model$summary$threshold, linetype = "dotted", color = paste0(lytics_colors["subnav"], "AA"))

	return(p)
}

plot.densities <- function(models, model.name) {
	model <- models[[model.name]]

	success <- unlist(model$summary$success)
	failure <- unlist(model$summary$fail)

	success.counts <- rep(as.numeric(names(success)), times = success)
	failure.counts <- rep(as.numeric(names(failure)), times = failure)

	density.df <- data.frame(
		value = c(success.counts, failure.counts),
		Segment = c(rep("Target", length(success.counts)), rep("Source", length(failure.counts)))
	)

	p <- ggplot(density.df, aes(x = value, color = Segment)) +
		geom_density(alpha = 0.3, adjust = 2) + xlim(0, 1) +
		labs(x = "Prediction", y = "", title = "Prediction Overlap") +
		scale_color_manual(values = as.vector(lytics_colors[c("brightgreen", "purple")], mode = "character"))
	return(p)
}

plot.distributions <- function(models, model.name) {
	model <- models[[model.name]]
	cleaned <- list()

	for (column in names(sort(unlist(lapply(lapply(model$threshold_distribution, unlist), sum)), decreasing = TRUE)) ) {
		data <- model$threshold_distribution[[column]]
		data.clean <- do.call("rbind", lapply(data, unlist)) %>%
			as.data.frame %>%
			dplyr::add_rownames(var = "split") %>%
			melt %>%
			cbind(name = column)

		cleaned[[column]] = data.clean
	}

	all.cleaned <- do.call("rbind", cleaned)

	qcolors <- c(lytics_colors["brightblue"], lytics_colors["brightgreen"])
	names(qcolors) <- NULL

	p <- ggplot(all.cleaned, aes(x = split, y = value, fill = variable)) +
		geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
		scale_fill_manual(values = qcolors) +
		facet_wrap(~ name, scales = "free") +
		labs(title = paste("SegmentML Split Distributions"), subtitle = model.name) +
		theme(
			strip.text = element_text(size = 16, face = "bold", color = lytics_colors['white']),
			legend.text = element_text(size = 14)
		)
	return(p)
}

# for fieldinfo plots
prepare.aspect <- function(api, field, seg.slug) {
	aspect <- gsub("^aspect_", "", field)
	seg <- api$get.segment(id = seg.slug)
	aspect.seg <- api$get.segment(id = aspect)

	adhoc.seg  <- sprintf("FILTER AND ( INCLUDE %s, INCLUDE %s )", aspect.seg$id, seg$id)
	adhoc.size.resp <- api$get.segment.size(segments = adhoc.seg)
	adhoc.size <- unlist(adhoc.size.resp)[1]

	seg.size.resp <- api$get.segment.size(id = seg$id)
	seg.size <- unlist(seg.size.resp)[1]

	rate <- (adhoc.size) / (seg.size)

	cat(paste("\n\nAspect:",seg.slug ,": ", rate, "\n\n"))

	return(data.frame(
		value = seg.slug,
		count = rate
	))
}

prepare.fieldinfo <- function(api, field, seg.slug) {
	if(is.null(field)) {
		return(NULL)
	}

	original <- field
	data.type <- "score"
	# ignore aspects, content for now
	if(hasPrefix(field, "aspect")) {
		return(prepare.aspect(api, field, seg.slug))
	}

	# if score, remove the prefix
	if(hasPrefix(field, "lytics")) {
		field <- strsplit(field, "lytics_")[[1]][2]
	}

	# if content, modify prefix
	if(hasPrefix(field, "content")) {
		data.type <- "map[string]number"
		field <- paste0("lytics_", gsub("content_", "content.", field))
	}

	# if segment prediction, modify prefix
	if(hasPrefix(field, "prediction")) {
		data.type <- "map[string]number"
		field <- paste0("segment_", gsub("prediction_", "prediction.", field))
	}

	# if lql, remove prefix
	if(hasPrefix(field, "lql_")) {
		data.type <- "lql"
		field <- strsplit(strsplit(field, "lql_")[[1]][2], "::")[[1]][1]
	}

	if(field == original) {
		stop(paste("bad field", field))
	}

	limit <- 20
	if(data.type == "map[string]number") {
		limit <- 100
	}

	target <- api$get.segment(id = seg.slug)
	fi <- api$get.segment.fieldinfo(fields = field, ids = target$id, limit = limit)

	print(jsonlite::toJSON(fi, pretty = TRUE))

	terms.counts <- unlist(fi$segments[[1]]$fields[[1]]$terms_counts)
	if(data.type == "map[string]number") {
		if(max(as.numeric(names(terms.counts))) <= 1) {
			names(terms.counts) <- as.character(round(as.numeric(names(terms.counts)) * 100))
		}
	}

	# fill up empty slots between 0 and 100
	if(data.type == "map[string]number" | data.type == "score") {
		filled.terms.counts <- rep(0, 101)
		names(filled.terms.counts) <- as.character(0:100)

		matched <- match(names(terms.counts), names(filled.terms.counts))
		names(terms.counts) <- NULL

		filled.terms.counts[matched] <- terms.counts

		terms.counts <- filled.terms.counts
	}

	fi.tgt <- terms.counts %>% as.data.frame %>% add_rownames
	colnames(fi.tgt) <- c("value", "count")


	if(data.type == "score" | data.type == "map[string]number") {
		fi.tgt$value <- as.numeric(as.character(fi.tgt$value))
	} else if(data.type == "lql") {
		fi.tgt$count <- fi.tgt$count / sum(fi.tgt$count)
	}

	return(fi.tgt)
}

get.fieldnames.from.importance <- function(importance.names) {
	return(gsub("\\:\\:.+", "", gsub("(content_|lql_|lytics_)", "", importance.names)))
}

parse.model.name <- function(model.name) {
	parts <- list(source = NULL, target = NULL, gen = NULL)
	splat <- unlist(str_split(model.name, "::"))
	if(length(splat) > 1) {
		parts$source = splat[1]
		parts$target = splat[2]
		if(length(splat) == 3) {
			gen <- as.integer(splat[3])
			if(!is.na(gen)) {
				parts$gen <- gen
			}
		}
	}
	return(parts)
}

remove.model.gen <- function(model.name) {
	parsed <- parse.model.name(model.name)
	return(paste(parsed$source, parsed$target, sep = "::"))
}

vectorize.terms <- function(fieldinfo) {
	score.counts <- rep(0L, 101)
	names(score.counts) <- as.character(0:100)

	score.counts[match(names(fieldinfo$terms_counts), names(score.counts))] <- unlist(fieldinfo$terms_counts)
	return(score.counts)
}
