
# riskintroapp

Shiny application for estimating the risk of an animal disease being
introduced into a geographical area. The app can be used solely with
user data but also provides datasets derived from World Animal Health
Information System (WAHIS).

## Installation

You can install riskintroapp from GitLab using the remotes package:

``` r
# Install pak if you don't have it
install.packages("pak")
pak::pak("git::https://gitlab.cirad.fr/astre/riskintro-app/riskintroapp.git")

# Accept updates to 
#  + riskintrodata    
#  + riskintroanalysis
#  + riskintroapp
#  + ideally any other dependencies
```

## Running the Application

Launch the Shiny application with:

``` r
riskintroapp::run_riskintro()
```

## Workflow

The typical workflow in riskintroapp follows these steps:

1.  Import epidemiological units: Load geospatial data (shapefile,
    GeoPackage, or GeoJSON) defining the administrative boundaries or
    regions you want to analyze

2.  Configure emission risk factors: Either import WAHIS outbreak data
    for your disease of interest or manually define emission risk
    factors for source countries/regions

3.  Calculate introduction risks: Analyze risk from multiple pathways:

    - Border risk: Risk from shared land borders with neighboring
      regions
    - Entry point risk: Risk from ports, airports, and border crossings
    - Animal mobility risk: Risk from livestock movements between
      regions
    - Road access risk: Risk based on transportation infrastructure
      connectivity
    - Additional risks: Additional user-defined risk factors

4.  Rescale risk scores: Transform calculated risks to a common 0-100
    scale using linear, quadratic, exponential, or sigmoid methods

5.  Aggregate results: Combine risks from different pathways using mean,
    maximum, minimum, or median aggregation

6.  Visualize and export: View results on interactive maps and tables

## Saving your work

Use the Workspace tab to save and restore your complete analysis
session:

- Save workspace: Exports all imported data, calculated risks, scaling
  parameters, and settings to a single file
- Load workspace: Restores a previously saved session to continue your
  analysis
- Session persistence: All data, configurations, and results are
  preserved between sessions

This allows you to: - Share complete analyses with colleagues - Return
to previous work without re-importing data - Maintain reproducible
analysis workflows - Compare different risk scenarios

## Related Packages

riskintroapp depends on two companion packages that provide the
underlying analysis methods and data management tools:

### riskintroanalysis

Analysis methods for calculating animal disease introduction risk
scores.

- Documentation: <https://astre.gitlab.cirad.fr/riskintroanalysis>
- Source Code:
  <https://gitlab.cirad.fr/astre/riskintro-app/riskintroanalysis>
- Bug Reports:
  <https://gitlab.cirad.fr/astre/riskintro-app/riskintroanalysis/-/issues>

### riskintrodata

Data management tools for importing and validating risk analysis
datasets.

- Documentation: <https://astre.gitlab.cirad.fr/riskintrodata>
- Source Code:
  <https://gitlab.cirad.fr/astre/riskintro-app/riskintrodata>
- Bug Reports:
  <https://gitlab.cirad.fr/astre/riskintro-app/riskintrodata/-/issues>

## License

MIT + file LICENSE
