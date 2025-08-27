# esas
<!-- badges: start -->
[![R-CMD-check](https://github.com/inbo/esas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inbo/esas/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Goal

This R package has the goal of lowering the barrier to prepare data for upload to the ESAS database. 
Additionally, it also provides functionality to read data from ESAS, 
as well as some basic analysis functions.

The package starts at 5 tables as described by the ICES Data Model. And works towards a single table, as expected by the [ESAS Data Portal](https://www.ices.dk/data/data-portals/Pages/European-Seabirds-at-sea.aspx).


ICES: International Council for the Exploration of the Sea hosts the ESAS
database (European Seabirds at Sea).

Inventory of the available data: https://esas.ices.dk/inventory

Uploading to ESAS happens as a single large csv file ('mega table'), that consists of the 5 tables from the ICES Data Model combined.

Downloading from ESAS returns the 5 tables according to the ESAS data model.

## ESAS Data Model

https://esas-docs.ices.dk/

### 5 Tables

The tables are also described in [DATSU](http://datsu.ices.dk/web/selRep.aspx?Dataset=148).

Some fields are required.

- File information
- Campaign: Information about campaign (= Survey) where data was collected
- Sample: Every Survey has a slightly different protocol, the height of the platform is dependent on the ship used. You can also count from a plane.
- Position: The platform moves during sampling, positions are midpoints calculated during a trajectory. Surveys are counted on a set interval (eg 2 minutes), so all counts are precise to 2 minutes, and the position is matched on a calculated midpoint of this interval. It also records observations about positions, like glare and cloud cover. This table is location dependent.
- Observation: What did you see in the time intervals: linked to position by PositionID.

Some fields allow multiple values, these are combined with `~`. Most tables have a free text Notes field.



## ICES API

link: https://esas.ices.dk/webservices

There was a comment in inbo/esas#2 that the API was quite slow.

## Functions
inbo/esas#1

### Read data from ESAS
- `read_esas()` : empty stub function
- `Read_ESAS_Tables()` : Read data & convert to ESAS 'mega-table', returns a list
### Prepare data for ESAS upload
- `Transform_ESAS_Tables_4_Upload()` : creates binded data.frame for upload from 5 tables
- `Export_ESAS_Upload_Matrix()` : creates tsv in expected format to upload from returned data.frame by `Transform_ESAS_Tables_4_Upload()`

### Analysis functions
- `Create_ESAS_Table()` create combined table as input for analysis functions
- `Calculate_Detection_P_Ship_Based_Surveys()`
- `Create_Seabird_Density_Cross_Table()`

## Example Scripts
- `ESAS functions example Execute distance analysis.R`
- `ESAS functions example Prepare for upload.R`
