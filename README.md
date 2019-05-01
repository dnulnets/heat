# HEAT System
This is a solution that allows anyone to report different events or disturbances, such as "I slipped here",
"Route blocked", "Traffic light broken", "Potholes" etc. in a city together with positional data to be able
to create a heatmap of events that can be used by others to plan their day.

In addition to this the specific event "I Slipped" is used for gathering and uploading accelerometer data
from mobile phones with the intent of training a neural network to determine based on your walking if it is slippery outside.

It contains a REST API server, a mobile phone application and a web portal for managing the server and displaying the heatmap etc.

THIS IS WORK IN PROGRESS!

## HEAT Server
This is a REST API for uploading events and accelerometer data from devices out on the field and to provide functionality
for the HEAT Web portal.

### Technology
The HEAT Server is developed in haskell using postgresql as a database using Ubuntu 18.04.2 LTS.

## HEAT Web Portal
This is the HEAT Web portal that allows you to manage users, heatmaps, routes, devices and uploaded data.

### Technology
The HEAT Web portal is developed in purescript using halogen 5 and uses the HEAT Server REST API using Ubuntu 18.04.2 LTS.

## Data gathering devices
The device, i.e. an iPhone and Android solution is not yet started. It will sample the device accelerometers and
send the data up to the HEAT server if the user of the mobile phone tells the app that he or she as had a slip. It will also
allow you to report other types of event.

Other types of devices are considered, such as black oxes for bicycles, cars, busses etc.

### Technology
This phone applications is planned to be developed in nativescript.

