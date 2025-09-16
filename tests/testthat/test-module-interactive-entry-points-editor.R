test_that("interactive entry points editor module creates UI correctly", {
  # Test UI function doesn't error
  expect_no_error(interactiveEntryPointsEditorUI("test"))
})

test_that("interactive entry points editor handles empty data", {
  # Mock session and inputs
  session <- MockShinySession$new()

  # Mock reactive functions
  input_data <- reactiveVal(data.frame(
    point_id = character(0),
    point_name = character(0),
    type = character(0),
    mode = character(0),
    lng = numeric(0),
    lat = numeric(0),
    stringsAsFactors = FALSE
  ))

  emission_scores <- reactiveVal(data.frame(
    iso3 = c("USA", "CAN"),
    country = c("United States", "Canada"),
    emission_risk = c(5.2, 3.1),
    stringsAsFactors = FALSE
  ))

  map_proxy <- reactive(NULL)
  map_click <- reactiveVal(NULL)
  map_marker_click <- reactiveVal(NULL)

  # Test server function doesn't error with empty data
  expect_no_error({
    testServer(interactiveEntryPointsEditorServer,
      args = list(
        input_data = input_data,
        emission_scores = emission_scores,
        map_proxy = map_proxy,
        map_click = map_click,
        map_marker_click = map_marker_click
      ),
      {
        # Should return NULL initially
        expect_null(session$returned())
      }
    )
  })
})

test_that("interactive entry points editor initializes with data", {
  # Mock session and inputs
  session <- MockShinySession$new()

  # Mock reactive functions with sample data
  sample_entry_points <- data.frame(
    point_id = c("00001", "00002"),
    point_name = c("Airport A", "Port B"),
    type = c("AIR", "SEA"),
    mode = c("C", "NC"),
    lng = c(-122.4194, -122.3200),
    lat = c(37.7749, 47.6062),
    stringsAsFactors = FALSE
  )

  input_data <- reactiveVal(sample_entry_points)

  emission_scores <- reactiveVal(data.frame(
    iso3 = c("USA", "CAN", "MEX"),
    country = c("United States", "Canada", "Mexico"),
    emission_risk = c(5.2, 3.1, 4.7),
    stringsAsFactors = FALSE
  ))

  map_proxy <- reactive(NULL)
  map_click <- reactiveVal(NULL)
  map_marker_click <- reactiveVal(NULL)

  # Test server function initializes correctly
  testServer(interactiveEntryPointsEditorServer,
    args = list(
      input_data = input_data,
      emission_scores = emission_scores,
      map_proxy = map_proxy,
      map_click = map_click,
      map_marker_click = map_marker_click
    ),
    {
      # Should initialize with input data
      expect_equal(nrow(session$returned()), 2)
      expect_equal(session$returned()$point_name, c("Airport A", "Port B"))
    }
  )
})