# SLIP System
This is a solution that allows anyone to report different events or disturbances, such as "I slipped", "Route blocked", "Traffic light broken"
in a city together with positional data to be able to create a heatmap of events that can be used by others to plan their day.

In addition to this the specific event "I Slipped" is used for gathering and uploading accelerometer data from mobile phones with the intent of
training a neural network to determine based on your walking if it is slippery outside.

It contains a REST API server, a mobile phone application and a web portal for managing the server and displaying the heatmap etc.

THIS IS WORK IN PROGRESS and also a learning project for purescript/haskell.

## SLIP Server
This is a REST API for uploading events and accelerometer data from devices out on the field and to provide functionality
for the SLIP Web Portal.

### Technology
The SLIP server is developed in haskell using mongo as a database. It is work in progress and currently a npm/node/javascript
version exists in the slip repository.

## SLIP Web Portal
This is a web portal that allows you to manage users, heatmaps, devices and uploaded data. This is the part of the project which is most activity
in.

### Technology
The web portal is developed in purescript using halogen 5 and uses the SLIP Server REST API. It is work in progress and currently
a angular 7/typescript version exists in the slip repository.

## Data gathering devices "I Slipped"
The device, i.e. an iPhone and Android solution is under development. It will sample the device accelerometers and
send the data up to the SLIP server if the user of the mobile phone tells the app that he or she as had a slip. It will also
allow you to report other types of event.

### Technology
This phone applications is planned to be developed in nativescript.

