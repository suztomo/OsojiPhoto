function onMessageLoad(data) {
    var i;
    for (i=0; i<data.length; ++i) {
        var m = data[i];
        console.log(m);
    }
}

$(document).ready(function() {
var url = "/messages/all.json";
$.ajax({
  url: url,
  dataType: 'json',
  success: onMessageLoad
});
})
