## Helicopter Noise and Safety



On April 16, 2024, the Committee on Economic Development, chaired by Majority Leader Amanda Farías, held an oversight hearing entitled “Helicopter Noise and Safety”. This repo contains the code and visuals for the associated [webpage](https://council.nyc.gov/data/helicopter-noise-and-safety/). 

The data team analyzed helicopter flight and 311 data to:  

- Understand the flight paths from each heliport
- Analyze which flights were most frequently associated with 311 noise complaints
- Identify the best policy solutions to reduce the negative effects of helicopter noise on New York City residents

### Main Takeaways

- Complaints to 311 for helicopter noise are concentrated near heliports and the common flight paths in Manhattan, Brooklyn, and Queens.
- Tour flights originating outside of NYC fly over Manhattan unlike tour flights originating from the Downtown Manhattan Heliport.
- Commuter flights are likely to fly over Brooklyn and Queens.
- Commuter flights to or from JFK Airport and tour flights from New Jersey are the mostly likely to be associated with a 311 complaint.

### Data Sources 
- [FlightRadar24](https://www.flightradar24.com/40.70,-74.05/12)

- [311 Service Requests](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9)

### Code

#### Load Dependencies and Clean Data

The three scripts clean and process the helicopter data provided by FlightRadar24 to identify distinct flights and allow for analysis of the data. 

#### Folder: Analysis

Contains scripts for performing the data analysis. It is necessary to run `01_wrangle_flights.R` prior to the analysis, so a source line is included at the top of each script. Each folder contains the scripts related to a specific analysis topic. For instance, the folder `connect_311_flights/` contains the code that analyzes which flights were most frequently associated with 311 noise complaints. 

### Data Files

Data provided by FlightRadar24 is not available for public use so only the heliport csv is provided in the repo. 

#### input/heliport.csv

A collection of identified heliports along with manually set radii used to determine
helicopter landings.