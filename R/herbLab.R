#' Make herbarium labels
#'
#' @author Domingos Cardoso
#'
#' @description This function makes herbarium labels from a field book in a CSV
#' spreadsheet format. It is currently more comprehensive for making herbarium
#' labels from specimens collected in the USA, by displaying geographic maps also
#' at county level. For specimens collected in all other countries, the label will
#' display only the country level map. If geographic coordinates are provided,
#' then the specimen record is also plotted in the map. The function also insert
#' taxon authorities and nomenclatural updates automatically, by implementing an
#' internal function from [lcvplants](https://idiv-biodiversity.github.io/lcvplants/)
#' package.
#'
#' @usage
#' herbLab(fieldcoll = NULL,
#'         dir_create = "results_herbarium_labels",
#'         file_label = "herbarium_labels.pdf")
#'
#' @param fieldcoll The CSV formatted file containing all details about the collected
#' specimens. See the README file for a complete description on how the columns of
#' the input field book should be formatted.
#'
#' @param dir_create Path to the directory where the labels(s) will be saved. The
#' default setting creates a directory named **results_herbarium_labels** where
#' the labels will be saved in a subfolder named by the current date.
#'
#' @param file_label Name of the resulting PDF formatted herbarium label files.
#' If no name is reported, the default setting creates files named as
#' *herbarium_labels_1.pdf*, *herbarium_labels_2.pdf*, etc, depending on how many
#' labels you need to prepare, because the function put six labels in each PDF file.
#'
#' @importFrom magrittr "%>%"
#' @importFrom lcvplants lcvp_search
#' @importFrom cowplot ggdraw draw_plot plot_grid save_plot
#' @importFrom ggplot2 ggplot theme geom_sf geom_point aes annotate
#' @importFrom ggthemes theme_map
#' @importFrom ggspatial layer_spatial
#' @importFrom maps map
#' @importFrom sf st_as_sf st_bbox
#'
#' @export
#'

herbLab <- function(fieldcoll = NULL,
                    dir_create = "results_herbarium_labels",
                    file_label = "herbarium_labels.pdf") {

  #_______________________________________________________________________________
  # Making corrections in the database
  for (i in seq_along(names(fieldcoll))) {
    fieldcoll[[i]] <- gsub("^$", NA, fieldcoll[[i]])
    fieldcoll[[i]] <- gsub("\\s{2,}", " ", fieldcoll[[i]])
  }

  fieldcoll$decimalLatitude <- as.numeric(fieldcoll$decimalLatitude)
  fieldcoll$decimalLongitude <- as.numeric(fieldcoll$decimalLongitude)
  fieldcoll$recordNumber <- as.character(fieldcoll$recordNumber)

  fieldcoll$country <- gsub("United States|United States of America", "USA", fieldcoll$country)
  fieldcoll$county <- gsub("\\sCounty$|\\sCo[.]$", "", fieldcoll$county)

  fieldcoll$locality <- gsub("-", "\\\u00ad", fieldcoll$locality)
  fieldcoll$vegetation <- gsub("-", "\\\u00ad", fieldcoll$vegetation)
  fieldcoll$plantDescription <- gsub("-", "\\\u00ad", fieldcoll$plantDescription)
  fieldcoll$vernacularName <- gsub("-", "\\\u00ad", fieldcoll$vernacularName)

  #_______________________________________________________________________________
  # Stop the function if NA value is found within any stateProvince column
  tf_na <- is.na(fieldcoll$stateProvince)
  if(any(tf_na)) {
    stop(paste("\nCell(s) with NA found in the stateProvince column.\n
             Please check your Excel field book!")
    )
  }

  #_______________________________________________________________________________
  # Adding a final punctuation at some cells
  tf_na <- !is.na(fieldcoll$locality)
  tf <- !grepl("[.]$", fieldcoll$locality[tf_na])
  if(any(tf)) {
    fieldcoll$locality[tf_na][tf] <- gsub("$", ".", fieldcoll$locality[tf_na][tf])
  }
  tf_na <- !is.na(fieldcoll$vegetation)
  tf <- !grepl("[.]$", fieldcoll$vegetation[tf_na])
  if(any(tf)) {
    fieldcoll$vegetation[tf_na][tf] <- gsub("$", ".", fieldcoll$vegetation[tf_na][tf])
  }
  tf_na <- !is.na(fieldcoll$plantDescription)
  tf <- !grepl("[.]$", fieldcoll$plantDescription[tf_na])
  if(any(tf)) {
    fieldcoll$plantDescription[tf_na][tf] <- gsub("$", ".", fieldcoll$plantDescription[tf_na][tf])
  }

  tf <- is.na(fieldcoll$species)
  if(any(tf)) {
    fieldcoll$species[tf] <- "sp."
  }
  tf <- is.na(fieldcoll$recordNumber)
  if(any(tf)) {
    fieldcoll$recordNumber[tf] <- "s.n."
  }

  #_______________________________________________________________________________
  # Get the world country level map
  world <- sf::st_as_sf(maps::map("world", fill=TRUE, plot =FALSE))

  full_map_list <- list()
  for (i in 1:length(fieldcoll$species)) {

    fieldcoll_temp <- fieldcoll[i, ]

    print(paste(paste0(i,"/",length(fieldcoll$species)), "Making label for", fieldcoll_temp$genus, fieldcoll_temp$species,
                fieldcoll_temp$recordedBy, fieldcoll_temp$recordNumber))

    fieldcoll_temp$locality <- ifelse(is.na(fieldcoll_temp$locality), "", fieldcoll_temp$locality)
    fieldcoll_temp$vegetation <- ifelse(is.na(fieldcoll_temp$vegetation), "", fieldcoll_temp$vegetation)
    fieldcoll_temp$locality_vegetation <- paste(fieldcoll_temp$locality, fieldcoll_temp$vegetation)
    if (fieldcoll_temp$locality_vegetation %in% " ") {
      fieldcoll_temp$locality_vegetation <- ""
    }

    ncharloc <- nchar(fieldcoll_temp$locality_vegetation)

    #_______________________________________________________________________________
    # Break the text into different lines depending on the number of characters for each line
    fieldcoll_temp$locality_vegetation <- gsub("(.{1,85})(\\s|$)", "\\1\n",
                                               fieldcoll_temp$locality_vegetation)

    #_______________________________________________________________________________
    # Get authority for each taxon automatically using lcvplants package
    if (is.na(fieldcoll_temp$infraspecies)) {
      authority <- paste(fieldcoll_temp$genus, fieldcoll_temp$species)
    } else {
      authority <- paste(fieldcoll_temp$genus, fieldcoll_temp$species, fieldcoll_temp$infraspecies)
    }
    authority <- lcvplants::lcvp_search(authority)

    if (!is.null(authority)) {
      if (is.na(fieldcoll_temp$infraspecies)) {
        taxon <- sub("^(\\S*\\s+\\S+).*", "\\1", authority$Output.Taxon)
        authority <- gsub(".*^(\\S*\\s+\\S+)", "", authority$Output.Taxon)
        # Updating taxon name
        if (paste(fieldcoll_temp$genus, fieldcoll_temp$species) != taxon) {
          fieldcoll_temp$genus <- gsub("\\s.*", "", taxon)
          fieldcoll_temp$species <- gsub(".*\\s", "", taxon)
        }
      } else {
        taxon <- sub("^(\\S*\\s+\\S+\\s+\\S+\\s+\\S+).*", "\\1", authority$Output.Taxon)
        authority <- gsub(".*^(\\S*\\s+\\S+\\s+\\S+\\s+\\S+)", "", authority$Output.Taxon)
      }
    } else {
      authority <- " "
    }

    #_______________________________________________________________________________
    # Mapping
    if (fieldcoll_temp$country == "USA") {

      ctr <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

      county <- sf::st_as_sf(maps::map("county", fieldcoll_temp$stateProvince, fill=TRUE, plot =FALSE))
      state <- ctr[ctr$ID %in% tolower(fieldcoll_temp$stateProvince), ]

      county_temp <- county[gsub(".+,", "", county$ID) %in% tolower(fieldcoll_temp$county), ]

      ctr_bbox <- ctr %>%
        sf::st_as_sf(coords = c("X2","X1"), crs = 4326) %>%
        sf::st_bbox()
    }

    if (fieldcoll_temp$country != "USA") {
      ctr <- world[world$ID %in% fieldcoll_temp$country, ]
      ctr_bbox <- ctr %>%
        sf::st_as_sf(coords = c("X2","X1"), crs = 4326) %>%
        sf::st_bbox()
    }

    # Create an empty frame to plot the label
    df <- data.frame()
    outline <- ggplot2::ggplot(df) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(colour = "black",
                                                fill = NA,
                                                size = 1.5),
        rect = ggplot2::element_blank()
      )

    # Create the country map
    ctr_map <- ggplot2::ggplot(ctr) +
      #geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
      ggplot2::geom_sf(colour = "gray70",
                       fill = ifelse(fieldcoll_temp$country == "USA", NA, "gray95"),
                       size = 0.2) +
      ggthemes::theme_map() +
      if (fieldcoll_temp$country == "USA") {
        ggspatial::layer_spatial(state, color = "gray20", fill = "gray95", size = 0.4)
        #coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
      }

    if (fieldcoll_temp$country != "USA") {
      ctr_map <- ctr_map +
        ggplot2::geom_point(data = fieldcoll_temp,
                            ggplot2::aes(x = decimalLongitude,
                                         y = decimalLatitude),
                            shape =  21,
                            alpha = 0.9,
                            size = 3,  # control for point size
                            stroke = 0.1, # control for point border width
                            colour = "black",
                            fill = "#D53E4F",
                            show.legend = FALSE)
    }

    # Create the county map for USA
    if (fieldcoll_temp$country == "USA") {
      county_map <- ggplot2::ggplot(county) +
        ggplot2::geom_sf(colour = "gray70", fill = "gray95", size = 0.2) +
        ggspatial::layer_spatial(county_temp, color = "gray20", fill = "gray95", size = 0.4) +
        ggthemes::theme_map() +
        ggplot2::geom_point(data = fieldcoll_temp,
                            ggplot2::aes(x = decimalLongitude,
                                         y = decimalLatitude),
                            shape =  21,
                            alpha = 0.9,
                            size = 3,  # control for point size
                            stroke = 0.1, # control for point border width
                            colour = "black",
                            fill = "#D53E4F",
                            show.legend = FALSE)
    }

    if (is.na(fieldcoll_temp$infraspecies)) {
      sciname <- paste0("~bold(", gsub(" ", "~", taxon), ")")
    } else {
      sciname <- paste0("~bold(", fieldcoll_temp$genus, "~", fieldcoll_temp$species, ")",
                        "~", gsub(" ", "~bold(", fieldcoll_temp$infraspecies), ")")
    }

    fullmap <- cowplot::ggdraw() +
      cowplot::draw_plot(outline) +
      ggplot2::annotate("text", x = 0.1, y = 0.76,
                        label = paste(paste0(fieldcoll_temp$herbarium, "\n"), ifelse(is.na(fieldcoll_temp$catalogNumber), "", fieldcoll_temp$catalogNumber)),
                        colour = "black",
                        size = 4.5,
                        hjust = 0) +
      ggplot2::annotate("text", x = 0.77, y = 0.8,
                        label = paste("FLORA OF\n", ifelse(fieldcoll_temp$country == "USA", toupper(fieldcoll_temp$stateProvince), toupper(fieldcoll_temp$country))),
                        colour = "black",
                        size = 8) +
      ggplot2::annotate("text", x = 0.77, y = 0.63,
                        label = fieldcoll_temp$vernacularName,
                        colour = "black",
                        size = 4) +
      ggplot2::annotate("text", x = 0.05, y = 0.4,
                        label = paste0(ifelse(is.na(fieldcoll_temp$stateProvince), toupper(fieldcoll_temp$country), toupper(fieldcoll_temp$stateProvince)),
                                       ifelse(is.na(fieldcoll_temp$county), ".", ", "),

                                       ifelse(is.na(fieldcoll_temp$county), "", paste(fieldcoll_temp$county,
                                                                                      ifelse(fieldcoll_temp$country == "USA", "Co.   ", "   "))),

                                       ifelse(is.na(fieldcoll_temp$decimalLatitude), "", fieldcoll_temp$decimalLatitude), ifelse(is.na(fieldcoll_temp$decimalLatitude), "", ", "),
                                       ifelse(is.na(fieldcoll_temp$decimalLongitude), "", fieldcoll_temp$decimalLongitude), "   ",
                                       ifelse(is.na(fieldcoll_temp$altitude), "", fieldcoll_temp$altitude), "\n",
                                       fieldcoll_temp$locality_vegetation),
                        colour = "black",
                        size = 4,
                        hjust = 0,
                        vjust = 0.7) +
      ggplot2::annotate("text", x = 0.1, y = 0.55,
                        label = sciname,
                        colour = "black",
                        size = 6,
                        hjust = 0,
                        parse = TRUE) +
      ggplot2::annotate("text", x = 0.2, y = 0.5,
                        label = authority,
                        colour = "black",
                        size = 5,
                        hjust = 0) +
      ggplot2::annotate("text", x = 0.05, y = 0.08,
                        label = paste(fieldcoll_temp$recordedBy,
                                      fieldcoll_temp$recordNumber, " ",
                                      ifelse(is.na(fieldcoll_temp$addCollector), "", fieldcoll_temp$addCollector), "   ",
                                      paste0(ifelse(is.na(fieldcoll_temp$day), "", fieldcoll_temp$day), " ",
                                             ifelse(is.na(fieldcoll_temp$month), "", fieldcoll_temp$month), " ",
                                             ifelse(is.na(fieldcoll_temp$year), "Unknown collection date", fieldcoll_temp$year))),
                        colour = "black",
                        size = 4.5,
                        hjust = 0) +
      ggplot2::annotate("text", x = 0.05, y = ifelse(ncharloc > 300, 0.17, 0.25),
                        label = paste(ifelse(is.na(fieldcoll_temp$plantDescription), "",
                                             gsub("(.{1,85})(\\s|$)", "\\1\n", fieldcoll_temp$plantDescription))),
                        colour = "black",
                        size = 3.5,
                        hjust = 0)

    if (fieldcoll_temp$country == "USA") {
      fullmap <- fullmap +
        cowplot::draw_plot(ctr_map, x = 0.02, y = 0.8, width = 0.2, height = 0.2) +
        cowplot::draw_plot(county_map, x = 0.2, y = 0.62, width = 0.35, height = 0.35)
    }
    if (fieldcoll_temp$country != "USA") {
      fullmap <- fullmap +
        cowplot::draw_plot(ctr_map, x = 0.2, y = 0.55, width = 0.45, height = 0.45)
    }


    full_map_list[[i]] <- fullmap + ggplot2::theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm"))

  }

  #_______________________________________________________________________________
  # Split the plots into chunks
  chunk_length <- 6
  full_map_list <- split(full_map_list,
                         ceiling(seq_along(full_map_list) / chunk_length))

  for (i in seq_along(full_map_list)) {

    # Adding empty labels when any chunk has less than 6 labels so as to fit the size
    if (length(full_map_list[[i]]) < 6) {
      for (l in (length(full_map_list[[i]])+1):6) {
        outline <- cowplot::ggdraw() + cowplot::draw_plot(outline)
        full_map_list[[i]][[l]] <- outline + ggplot2::theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm"))
      }
    }

    allabels <- cowplot::plot_grid(full_map_list[[i]][[1]],
                                   full_map_list[[i]][[2]],
                                   full_map_list[[i]][[3]],
                                   full_map_list[[i]][[4]],
                                   full_map_list[[i]][[5]],
                                   full_map_list[[i]][[6]],
                                   ncol = 2, nrow = 3, align = "hv")

    # Create a new directory to save the results with current date
    # If there is no directory... make one!
    todaydate <- format(Sys.time(), "%d%b%Y")
    if (!dir.exists(paste0(dir_create, "/"))) {
      dir.create(paste0(dir_create, "/"))
    }
    if (!dir.exists(paste0(dir_create, "/", todaydate))) {
      dir.create(paste0(dir_create, "/", todaydate))
    }
    # If directory was created during a previous search, get its name to save results
    folder_name <- paste0(paste0(dir_create, "/"), todaydate)
    print(paste0("Writing '", folder_name, "' on disk."))

    file_label <- gsub("[.]pdf", "", file_label)

    cowplot::save_plot(paste0(folder_name, "/", file_label, "_", i, ".pdf"), allabels,
                       ncol = 2, # we're saving a grid plot of 2 columns
                       nrow = 3, # and 3 rows
                       base_height = 6,
                       base_aspect_ratio = 1.2,
                       base_width = NULL)
  }

}
