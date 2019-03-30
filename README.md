# SLIP Solution
This is a solution that allows you to report events or disturbances in a city together with positional data to
be able to create a heatmap of events that can be used by others to plan their day.

One specific event "Slippery" is meant for gathering and uploading accelerometer data mobile phones with the intent of
training a neural network to determine if it is slippery outside.

It contains a REST API server and a web portal for managing the server and displaying the heatmap etc.

## SLIP Server
This is a REST API for uploading events and accelerometer data from devices out on the field and to provide functionality
for the portal.

It is developed in haskell and mongo.

## SLIP Web Portal
This is a web portal that allows you to manage users, heatmaps, devices and uploaded data.

It is developed in purescript and halogen and uses the SLIP Server REST API.

## Data gathering devices
The current device, i.e. an iPhone and Android solution is under development. It will sample the device accelerometers and
send the data up to the SLIP server if the user of the mobile phone tells the app that he or she as had a slip. It will also
allow you to report other types of event.

It is developed in nativescript.

