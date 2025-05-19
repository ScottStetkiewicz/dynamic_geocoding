## Dynamic Geocoding App

This app is a tool to manually QA/QC particularly problematic addresses that are not able to be geocoded easily. Following batch geocoding, this approach can be useful if fully automated solutions aren't available but human support (such as students, volunteers, etc.) are.

In this simplified MVP, two addresses and sets of coordinates attributable to locations in Ashe County, North Carolina are loaded into an in-memory database. One is incorrectly geocoded outside the county boundary, and is initially flagged as problematic.

To fix this address, the user can search for the correct location and input the new coordinates, visualize the new point dynamically on a map, and update the database entry for that location.

**A hosted version of this app is available here**: https://scottstetkiewicz.shinyapps.io/dynamic_geocoding/

### Step 1

Selecting a row in the data table will highlight the corresponding marker on the map. Conversely, selecting a map marker will highlight the corresponding row in the data table.

Click the "Edit Coordinates" checkbox to edit a point/row.

https://github.com/user-attachments/assets/535a06c7-4bcc-47bb-bee2-146ee9e2f03b

### Step 2

Input a new set of lat/lon coordinates to plot a new test point on the map, checking the new marker is correctly placed. When you are ready, click the "Save Changes to DB" button to commit the updated coordinates to the underling SQLite database. 

The marker icon will change to a "thumbs up" to indicate it has been fixed, and the coordinates will be re-intersected with the county boundary to color-code the new icon. There is an "edited" boolean in the dataframe that tracks these changes as well.

https://github.com/user-attachments/assets/4438a3be-499e-4a1f-b5fa-4704bb9a5954

### Step 3

To work iteratively on very long datasets, there is an option to remove rows that have had their coordinates edited. This will only show uncorrected addresses in the both the data table and on the map. 

https://github.com/user-attachments/assets/3b8006d6-767c-4526-bb95-b201fc840e85


