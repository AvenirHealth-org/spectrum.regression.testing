#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import ggplot2
#' @noRd
app_server <- function(input, output, session) {
  meta.data = setup.meta.data()
  round.data = reactive({
    setup.round.data(meta.data, input$EstimatesRound)
  })

  round.tree = reactive({
    dat = round.data()
    tree.data = list(Global=convert.tree(dat$location$tree))
    attr(tree.data[[1]], "stselected") = TRUE # "Global" node is selected initially
    attr(tree.data[[1]], "stopened")   = TRUE # "Global" node is expanded initially, so that regions are visible
    return(tree.data)
  })

  observeEvent(round.data(), {
    dat = round.data()
    updateRadioButtons(session=session, inputId="area", selected=var.levels$area[1])
    updateTabsetPanel(session=session, inputId="Navigator", selected="Version")
    updateSelectInput(session=session, inputId="version1", choices=dat$version$name, selected=dat$version$name[1])
    updateSelectInput(session=session, inputId="version2", choices=dat$version$name, selected=dat$version$name[2])
  })

  observeEvent(
    c(
      input$useOrderlyData1,
      input$orderlySpectrum1,
      input$orderlyLeapfrog1
    ),
    {
      if (input$useOrderlyData1) {
        orderly_version1_choices <- list_orderly_versions(ORDERLY_ROOT, input$orderlySpectrum1, input$orderlyLeapfrog1)
        updateSelectInput(session = session,
                          inputId = "orderlyVersion1",
                          choices = orderly_version1_choices,
                          selected = orderly_version1_choices[1])
      }
    }
  )

  observeEvent(
    c(
      input$useOrderlyData2,
      input$orderlySpectrum2,
      input$orderlyLeapfrog2
    ),
    {
      if (input$useOrderlyData2) {
        orderly_version2_choices <- list_orderly_versions(ORDERLY_ROOT, input$orderlySpectrum2, input$orderlyLeapfrog2)
        updateSelectInput(session = session,
                          inputId = "orderlyVersion2",
                          choices = orderly_version2_choices,
                          selected = orderly_version2_choices[1])
      }
    }
  )

  output$locationTree = shinyTree::renderTree(round.tree())

  selected.files = reactive({
    req(input$locationTree)
    return(selected.pjnzs(input$locationTree))
  })

  load.extract1 = eventReactive(
    c(
      input$useOrderlyData1,
      input$version1,
      input$orderlyVersion1
    ),
    {
      if (input$useOrderlyData1) {
        req(input$orderlyVersion1)
        dat <- round.data()
        process.extract.orderly(input$orderlyVersion1, dat$location$data, dat$indicator$data)
      } else {
        req(input$version1)
        # print(sprintf("Version1 = %s", input$version1))
        dat = round.data()
        hash = lookup.version(input$version1, dat$version)
        process.extract(hash, dat$version, dat$location$data, dat$indicator$data)
      }
    }
  )

  load.extract2 = eventReactive(
    c(
      input$useOrderlyData2,
      input$version2,
      input$orderlyVersion2
    ),
    {
      if (input$useOrderlyData2) {
        req(input$orderlyVersion2)
        dat <- round.data()
        process.extract.orderly(input$orderlyVersion2, dat$location$data, dat$indicator$data)
      } else {
        req(input$version2)
        # print(sprintf("Version2 = %s", input$version2))
        dat = round.data()
        hash = lookup.version(input$version2, dat$version)
        process.extract(hash, dat$version, dat$location$data, dat$indicator$data)
      }
    }
  )

  merge.extracts = reactive({
    dplyr::bind_rows(list(Version1 = load.extract1(),
                          Version2 = load.extract2()),
                     .id="Version")
  })

  selected.data = reactive({
    dat        = merge.extracts()
    pjnz.list  = selected.files()
    area       = input$area
    indicators = input$indicators
    sex        = input$sex
    age        = input$age
    years      = input$years
    generate.frame(dat, area=area, indicators=indicators, sex=sex, age=age, years=years, pjnzs=pjnz.list)
  })

  ## Generate the plots
  plot.trend = reactive(generate.trend(selected.data()))
  plot.diffs = reactive(generate.diffs(selected.data()))

  ## Render the plots
  observeEvent(plot.trend(), {output$plotTrend = renderPlot(plot.trend()$p, height=max(300, 200*plot.trend()$r + 100))})
  observeEvent(plot.diffs(), {output$plotDiffs = renderPlot(plot.diffs()$p, height=max(300, 200*plot.diffs()$r + 100))})
  output$table = DT::renderDT(generate.table(selected.data()))

  ## This allows us to set up the location tree before the location tab is
  ## opened. If we did not do this then plots and tables would not render until
  ## the location tab is opened.
  outputOptions(output, "locationTree", suspendWhenHidden = FALSE)
}
