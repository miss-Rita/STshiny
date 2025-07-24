
#SpatialDimPlot----

SpatialDimPlot <- function(
    object,
    group.by = NULL,
    images = NULL,
    cols = NULL,
    crop = TRUE,
    cells.highlight = NULL,
    cols.highlight = c('#DE2D26', 'grey50'),
    facet.highlight = FALSE,
    label = FALSE,
    label.size = 7,
    label.color = 'white',
    repel = FALSE,
    ncol = NULL,
    combine = TRUE,
    pt.size.factor = 1.6,
    alpha = c(1, 1),
    image.alpha = 1,
    image.scale = "lowres",
    shape = 21,
    stroke = NA,
    label.box = TRUE,
    interactive = FALSE,
    information = NULL
) {
  return(SpatialPlot(
    object = object,
    group.by = group.by,
    images = images,
    cols = cols,
    crop = crop,
    cells.highlight = cells.highlight,
    cols.highlight = cols.highlight,
    facet.highlight = facet.highlight,
    label = label,
    label.size = label.size,
    label.color = label.color,
    repel = repel,
    ncol = ncol,
    combine = combine,
    pt.size.factor = pt.size.factor,
    alpha = alpha,
    image.alpha = image.alpha,
    image.scale = image.scale,
    shape = shape,
    stroke = stroke,
    label.box = label.box,
    interactive = interactive,
    information = information
  ))
}

#SpatialPlot----

SpatialPlot <- function(
    object,
    group.by = NULL,
    features = NULL,
    images = NULL,
    cols = NULL,
    image.alpha = 1,
    image.scale = "lowres",
    crop = TRUE,
    slot = 'data',
    keep.scale = "feature",
    min.cutoff = NA,
    max.cutoff = NA,
    cells.highlight = NULL,
    cols.highlight = c('#DE2D26', 'grey50'),
    facet.highlight = FALSE,
    label = FALSE,
    label.size = 5,
    label.color = 'white',
    label.box = TRUE,
    repel = FALSE,
    ncol = NULL,
    combine = TRUE,
    pt.size.factor = 1.6,
    alpha = c(1, 1),
    shape = 21,
    stroke = NA,
    interactive = FALSE,
    do.identify = FALSE,
    identify.ident = NULL,
    do.hover = FALSE,
    information = NULL
) {
  if (isTRUE(x = do.hover) || isTRUE(x = do.identify)) {
    warning(
      "'do.hover' and 'do.identify' are deprecated as we are removing plotly-based interactive graphics, use 'interactive' instead for Shiny-based interactivity",
      call. = FALSE,
      immediate. = TRUE
    )
    interactive <- TRUE
  }
  if (!is.null(x = group.by) & !is.null(x = features)) {
    stop("Please specific either group.by or features, not both.")
  }
  images <- images %||% Images(object = object, assay = DefaultAssay(object = object))
  if (length(x = images) == 0) {
    images <- Images(object = object)
  }
  if (length(x = images) < 1) {
    stop("Could not find any spatial image information")
  }
  
  # Check keep.scale param for valid entries
  if (!(is.null(x = keep.scale)) && !(keep.scale %in% c("feature", "all"))) {
    stop("`keep.scale` must be set to either `feature`, `all`, or NULL")
  }
  
  cells <- unique(CellsByImage(object, images = images, unlist = TRUE))
  if (is.null(x = features)) {
    if (interactive) {
      return(ISpatialDimPlot(
        object = object,
        image = images[1],
        image.scale = image.scale,
        group.by = group.by,
        alpha = alpha
      ))
    }
    group.by <- group.by %||% 'ident'
    object[['ident']] <- Idents(object = object)
    data <- object[[group.by]]
    data <- data[cells,,drop=F]
    for (group in group.by) {
      if (!is.factor(x = data[, group])) {
        data[, group] <- factor(x = data[, group])
      }
    }
  } else {
    if (interactive) {
      return(ISpatialFeaturePlot(
        object = object,
        feature = features[1],
        image = images[1],
        image.scale = image.scale,
        slot = slot,
        alpha = alpha
      ))
    }
    data <- FetchData(
      object = object,
      vars = features,
      cells = cells,
      layer = slot,
      clean = FALSE
    )
    features <- colnames(x = data)
    # Determine cutoffs
    min.cutoff <- mapply(
      FUN = function(cutoff, feature) {
        return(ifelse(
          test = is.na(x = cutoff),
          yes = min(data[, feature]),
          no = cutoff
        ))
      },
      cutoff = min.cutoff,
      feature = features
    )
    max.cutoff <- mapply(
      FUN = function(cutoff, feature) {
        return(ifelse(
          test = is.na(x = cutoff),
          yes = max(data[, feature]),
          no = cutoff
        ))
      },
      cutoff = max.cutoff,
      feature = features
    )
    check.lengths <- unique(x = vapply(
      X = list(features, min.cutoff, max.cutoff),
      FUN = length,
      FUN.VALUE = numeric(length = 1)
    ))
    if (length(x = check.lengths) != 1) {
      stop("There must be the same number of minimum and maximum cuttoffs as there are features")
    }
    # Apply cutoffs
    data <- sapply(
      X = 1:ncol(x = data),
      FUN = function(index) {
        data.feature <- as.vector(x = data[, index])
        min.use <- SetQuantile(cutoff = min.cutoff[index], data.feature)
        max.use <- SetQuantile(cutoff = max.cutoff[index], data.feature)
        data.feature[data.feature < min.use] <- min.use
        data.feature[data.feature > max.use] <- max.use
        return(data.feature)
      }
    )
    colnames(x = data) <- features
    rownames(x = data) <- cells
  }
  features <- colnames(x = data)
  colnames(x = data) <- features
  rownames(x = data) <- cells
  facet.highlight <- facet.highlight && (!is.null(x = cells.highlight) && is.list(x = cells.highlight))
  if (do.hover) {
    if (length(x = images) > 1) {
      images <- images[1]
      warning(
        "'do.hover' requires only one image, using image ",
        images,
        call. = FALSE,
        immediate. = TRUE
      )
    }
    if (length(x = features) > 1) {
      features <- features[1]
      type <- ifelse(test = is.null(x = group.by), yes = 'feature', no = 'grouping')
      warning(
        "'do.hover' requires only one ",
        type,
        ", using ",
        features,
        call. = FALSE,
        immediate. = TRUE
      )
    }
    if (facet.highlight) {
      warning(
        "'do.hover' requires no faceting highlighted cells",
        call. = FALSE,
        immediate. = TRUE
      )
      facet.highlight <- FALSE
    }
  }
  if (facet.highlight) {
    if (length(x = images) > 1) {
      images <- images[1]
      warning(
        "Faceting the highlight only works with a single image, using image ",
        images,
        call. = FALSE,
        immediate. = TRUE
      )
    }
    ncols <- length(x = cells.highlight)
  } else {
    ncols <- length(x = images)
  }
  plots <- vector(
    mode = "list",
    length = length(x = features) * ncols
  )
  
  # Get max across all features
  if (!(is.null(x = keep.scale)) && keep.scale == "all") {
    max.feature.value <- max(apply(data, 2, function(x) max(x, na.rm = TRUE)))
  }
  
  for (i in 1:ncols) {
    plot.idx <- i
    image.idx <- ifelse(test = facet.highlight, yes = 1, no = i)
    image.use <- object[[images[[image.idx]]]]
    
    coordinates <- GetTissueCoordinates(
      object = image.use,
      scale = image.scale
    )
    
    highlight.use <- if (facet.highlight) {
      cells.highlight[i]
    } else {
      cells.highlight
    }
    for (j in 1:length(x = features)) {
      cols.unset <- is.factor(x = data[, features[j]]) && is.null(x = cols)
      if (cols.unset) {
        cols <- hue_pal()(n = length(x = levels(x = data[, features[j]])))
        names(x = cols) <- levels(x = data[, features[j]])
      }
      
      # Get feature max for individual feature
      if (!(is.null(x = keep.scale)) && keep.scale == "feature" && !inherits(x = data[, features[j]], what = "factor") ) {
        max.feature.value <- max(data[, features[j]])
      }
      
      plot <- SingleSpatialPlot(
        data = cbind(
          coordinates,
          data[rownames(x = coordinates), features[j], drop = FALSE]
        ),
        image = image.use,
        image.scale = image.scale,
        image.alpha = image.alpha,
        col.by = features[j],
        cols = cols,
        alpha.by = if (is.null(x = group.by)) {
          features[j]
        } else {
          NULL
        },
        pt.alpha = if (!is.null(x = group.by)) {
          alpha[j]
        } else {
          NULL
        },
        geom = if (inherits(x = image.use, what = "STARmap")) {
          'poly'
        } else {
          'spatial'
        },
        cells.highlight = highlight.use,
        cols.highlight = cols.highlight,
        pt.size.factor = pt.size.factor,
        shape = shape,
        stroke = stroke,
        crop = crop
      )
      if (is.null(x = group.by)) {
        plot <- plot +
          scale_fill_gradientn(
            name = features[j],
            colours = SpatialColors(n = 100)
          ) +
          theme(legend.position = 'top') +
          scale_alpha(range = alpha) +
          guides(alpha = "none")
      } else if (label) {
        plot <- LabelClusters(
          plot = plot,
          id = ifelse(
            test = is.null(x = cells.highlight),
            yes = features[j],
            no = 'highlight'
          ),
          geom = if (inherits(x = image.use, what = "STARmap")) {
            'GeomPolygon'
          } else {
            'GeomSpatial'
          },
          repel = repel,
          size = label.size,
          color = label.color,
          box = label.box,
          position = "nearest"
        )
      }
      if (j == 1 && length(x = images) > 1 && !facet.highlight) {
        plot <- plot +
          ggtitle(label = images[[image.idx]]) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      if (facet.highlight) {
        plot <- plot +
          ggtitle(label = names(x = cells.highlight)[i]) +
          theme(plot.title = element_text(hjust = 0.5)) +
          NoLegend()
      }
      
      # Plot multiple images depending on keep.scale
      if (!(is.null(x = keep.scale)) && !inherits(x = data[, features[j]], "factor")) {
        plot <- suppressMessages(plot & scale_fill_gradientn(colors = SpatialColors(n = 100), limits = c(NA, max.feature.value)))
      }
      
      plots[[plot.idx]] <- plot
      plot.idx <- plot.idx + ncols
      if (cols.unset) {
        cols <- NULL
      }
    }
  }
  
  if (combine) {
    if (!is.null(x = ncol)) {
      return(wrap_plots(plots = plots, ncol = ncol))
    }
    if (length(x = images) > 1) {
      return(wrap_plots(plots = plots, ncol = length(x = images)))
    }
    return(wrap_plots(plots = plots))
  }
  return(plots)
}


#ISpatialFeaturePlot----


ISpatialFeaturePlot <- function(
    object,
    feature,
    image = NULL,
    image.scale = "lowres",
    slot = 'data',
    alpha = c(0.1, 1)
) {
  # Set inital data values
  assay.keys <- Key(object = object)[Assays(object = object)]
  keyed <- sapply(X = assay.keys, FUN = grepl, x = feature)
  assay <- if (any(keyed)) {
    names(x = which(x = keyed))[1]
  } else {
    DefaultAssay(object = object)
  }
  features <- sort(x = rownames(x = GetAssayData(
    object = object,
    slot = slot,
    assay = assay
  )))
  feature.label <- 'Feature to visualize'
  assays.use <- vapply(
    X = Assays(object = object),
    FUN = function(x) {
      return(!IsMatrixEmpty(x = GetAssayData(
        object = object,
        slot = slot,
        assay = x
      )))
    },
    FUN.VALUE = logical(length = 1L)
  )
  assays.use <- sort(x = Assays(object = object)[assays.use])
  
  
  
  # Setup gadget UI
  ui <- miniPage(
    miniButtonBlock(miniTitleBarButton(
      inputId = 'done',
      label = 'Done',
      primary = TRUE
    )),
    miniContentPanel(
      fillRow(
        sidebarPanel(
          sliderInput(
            inputId = 'alpha',
            label = 'Alpha intensity',
            min = 0,
            max = max(alpha),
            value = min(alpha),
            step = 0.01,
            width = '100%'
          ),
          sliderInput(
            inputId = 'pt.size',
            label = 'Point size',
            min = 0,
            max = 5,
            value = 1.6,
            step = 0.1,
            width = '100%'
          ),
          selectInput(
            inputId = 'assay',
            label = 'Assay',
            choices = assays.use,
            selected = assay,
            selectize = FALSE,
            width = '100%'
          ),
          selectInput(
            inputId = 'feature',
            label = feature.label,
            choices = features,
            selected = feature,
            selectize = FALSE,
            width = '100%'
          ),
          selectInput(
            inputId = 'palette',
            label = 'Color scheme',
            choices = names(x = FeaturePalettes),
            selected = 'Spatial',
            selectize = FALSE,
            width = '100%'
          ),
          width = '100%'
        ),
        plotOutput(outputId = 'plot', height = '100%'),
        flex = c(1, 4)
      )
    )
  )
  # Prepare plotting data
  image <- image %||% DefaultImage(object = object)
  cells.use <- Cells(x = object[[image]])
  coords <- GetTissueCoordinates(object = object[[image]], scale = image.scale)
  feature.data <- FetchData(
    object = object,
    vars = feature,
    cells = cells.use,
    slot = slot
  )
  plot.data <- cbind(coords, feature.data)
  
  
  server <- function(input, output, session) {
    plot.env <- reactiveValues(
      data = plot.data,
      feature = feature,
      palette = 'Spatial'
    )
    # Observe events
    observeEvent(
      eventExpr = input$done,
      handlerExpr = stopApp(returnValue = plot.env$plot)
    )
    observe(x = {
      assay <- input$assay
      feature.use <- input$feature
      features.assay <- sort(x = rownames(x = GetAssayData(
        object = object,
        slot = slot,
        assay = assay
      )))
      feature.use <- ifelse(
        test = feature.use %in% features.assay,
        yes = feature.use,
        no = features.assay[1]
      )
      updateSelectInput(
        session = session,
        inputId = 'assay',
        label = 'Assay',
        choices = assays.use,
        selected = assay
      )
      updateSelectInput(
        session = session,
        inputId = 'feature',
        label = feature.label,
        choices = features.assay,
        selected = feature.use
      )
    })
    observe(x = {
      feature.use <- input$feature
      try(
        expr = {
          feature.data <- FetchData(
            object = object,
            vars = paste0(Key(object = object[[input$assay]]), feature.use),
            cells = cells.use,
            slot = slot
          )
          colnames(x = feature.data) <- feature.use
          plot.env$data <- cbind(coords, feature.data)
          plot.env$feature <- feature.use
        },
        silent = TRUE
      )
    })
    observe(x = {
      plot.env$palette <- input$palette
    })
    # Create plot
    output$plot <- renderPlot(expr = {
      plot.env$plot <- SingleSpatialPlot(
        data = plot.env$data,
        image = object[[image]],
        col.by = plot.env$feature,
        pt.size.factor = input$pt.size,
        crop = TRUE,
        alpha.by = plot.env$feature
      ) +
        # scale_fill_gradientn(name = plot.env$feature, colours = cols) +
        scale_fill_gradientn(name = plot.env$feature, colours = FeaturePalettes[[plot.env$palette]]) +
        theme(legend.position = 'top') +
        scale_alpha(range = c(input$alpha, 1)) +
        guides(alpha = "none")
      plot.env$plot
    })
  }
  runGadget(app = ui, server = server)
}



