# SLIP System
This is a solution that allows anyone to report different events or disturbances, such as "I slipped", "Route blocked", "Traffic light broken"
in a city together with positional data to be able to create a heatmap of events that can be used by others to plan their day.

In addition to this the specific event "I Slipped" is used for gathering and uploading accelerometer data from mobile phones with the intent of
training a neural network to determine based on your walking if it is slippery outside.

It contains a REST API server and a web portal for managing the server and displaying the heatmap etc.

## SLIP Server
This is a REST API for uploading events and accelerometer data from devices out on the field and to provide functionality
for the SLIP Web Portal.

It is developed in haskell and mongo.

## SLIP Web Portal
This is a web portal that allows you to manage users, heatmaps, devices and uploaded data.

It is developed in purescript using halogen and uses the SLIP Server REST API.

## Data gathering devices "I Slipped"
The device, i.e. an iPhone and Android solution is under development. It will sample the device accelerometers and
send the data up to the SLIP server if the user of the mobile phone tells the app that he or she as had a slip. It will also
allow you to report other types of event.

It is developed in nativescript.

