/**
 * Created by Jon on 05/07/16.
 */

var node = document.getElementById("puzzle2-ui");
var app = Elm.WheelApp.embed(node);

app.ports.showWheel.subscribe( data => {

  var dataSz = [];

  // sz property determines wheel size
  dataSz[0] = data[0].map(val => {
    return {name: val.name, value: 9, sz: 8}
  });

  dataSz[1] = data[1].map(val => {
    return {name: val.name, value: 9, sz: 6}
  });

  dataSz[2] = data[2].map(val => {
    return {name: val.name, value: 9, sz: 4}
  });

  dataSz[3] = data[3].map(val => {
    return {name: val.name, value: 9, sz: 2}
  });

  showCircle(dataSz);

  var names = data[0].map(val => val.name);

  // return no-op data back to elm
  var dataProcessedItems = ["2"];
  app.ports.dataProcessedItems.send(dataProcessedItems);
});
