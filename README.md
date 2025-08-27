# esas
<!-- badges: start -->
[![R-CMD-check](https://github.com/inbo/esas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inbo/esas/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Goal

This R package has the goal lowering the barrier to prepare data for upload to the ESAS database. 
Additionally, it also provides functionality to read data from ESAS, 
as well as some basic analysis functions.


ICES: International Council for the Exploration of the Sea hosts the ESAS
database (European Seabirds at Sea).

Inventory of the available data: https://esas.ices.dk/inventory

## ICES Data Model

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

https://esas.ices.dk/webservices

## Functions
inbo/esas#1

### Read data from ESAS
- read_esas()
- 
### Prepare data for ESAS upload
### Analysis functions
- 
