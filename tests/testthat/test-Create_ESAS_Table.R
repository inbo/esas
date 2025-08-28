test_that("Create_ESAS_Table() returns data.frame with expected columns", {
  # QUESTION: Should all these columns always be present?
  expect_s3_class(Create_ESAS_Table(), "data.frame")

  expect_named(
    Create_ESAS_Table(),
    c(
      "DataRightsHolder",
      "Country",
      "CampaignID",
      "DataAccess",
      "StartDate",
      "EndDate",
      "CampaignNotes",
      "SampleID",
      "Date",
      "PlatformCode",
      "PlatformClass",
      "PlatformSide",
      "PlatformHeight",
      "TransectWidth",
      "SamplingMethod",
      "PrimarySampling",
      "TargetTaxa",
      "DistanceBins",
      "UseOfBinoculars",
      "NumberOfObservers",
      "SampleNotes",
      "PositionID",
      "Time",
      "Latitude",
      "Longitude",
      "Distance",
      "Area",
      "WindForce",
      "Visibility",
      "Glare",
      "SunAngle",
      "CloudCover",
      "Precipitation",
      "IceCover",
      "ObservationConditions",
      "ObservationID",
      "GroupID",
      "Transect",
      "SpeciesCodeType",
      "SpeciesCode",
      "SpeciesScientificName",
      "SpeciesEnglishName",
      "WormsAphiaID",
      "WormsScientificName",
      "Count",
      "ObservationDistance",
      "LifeStage",
      "Moult",
      "Plumage",
      "Sex",
      "TravelDirection",
      "Prey",
      "Association",
      "Behaviour",
      "Notes"
    )
  )
})

test_that("Create_ESAS_Table() returns sum of rows of tables", {

})

test_that("Create_ESAS_Table() returns correct column types", {
  table_list <-
    Read_ESAS_Tables(system.file("extdata", "ESAS_0827343782", package = "esas"))
  expect_identical(
    purrr::map(Create_ESAS_Table(table_list), class),
    list(
      DataRightsHolder = "integer",
      Country = "character",
      CampaignID = "integer",
      DataAccess = "character",
      StartDate = "character",
      EndDate = "character",
      CampaignNotes = "logical",
      SampleID = "integer",
      Date = "character",
      PlatformCode = "character",
      PlatformClass = "integer",
      PlatformSide = "logical",
      PlatformHeight = "logical",
      TransectWidth = "integer",
      SamplingMethod = "integer",
      PrimarySampling = "character",
      TargetTaxa = "integer",
      DistanceBins = "character",
      UseOfBinoculars = "integer",
      NumberOfObservers = "integer",
      SampleNotes = "logical",
      PositionID = "integer",
      Time = "character",
      Latitude = "numeric",
      Longitude = "numeric",
      Distance = "numeric",
      Area = "numeric",
      WindForce = "integer",
      Visibility = "character",
      Glare = "character",
      SunAngle = "logical",
      CloudCover = "logical",
      Precipitation = "character",
      IceCover = "integer",
      ObservationConditions = "logical",
      ObservationID = "integer",
      GroupID = "integer",
      Transect = "character",
      SpeciesCodeType = "character",
      SpeciesCode = "integer",
      SpeciesScientificName = "character",
      SpeciesEnglishName = "character",
      WormsAphiaID = "integer",
      WormsScientificName = "character",
      Count = "integer",
      ObservationDistance = "character",
      LifeStage = "character",
      Moult = "logical",
      Plumage = "character",
      Sex = "character",
      TravelDirection = "character",
      Prey = "integer",
      Association = "character",
      Behaviour = "integer",
      Notes = "character"
    )
  )
})

test_that("Create_ESAS_Table() returns error when table missing", {
  # All 4 input tables need to be present
})
