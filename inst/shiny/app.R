library(riskintroapp)
library(riskintrodata)
library(riskintroanalysis)

# Run the application
shinyApp(
  ui = riskintroapp:::ui,
  server = riskintroapp:::server,
  onStart = riskintroapp:::onstart
  )

