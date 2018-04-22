$(function() {
  $.getJSON("/data.json", function (data) {
    var d = $.map(data, function (item, i) {
      var parts = item[0].split("-");
      var date = new Date(parseInt(parts[0]), parseInt(parts[1]) - 1, parseInt(parts[2]));
      return [[date, item[1]]];
    });

    d.sort(function (a, b) {
      return a[0] - b[0];
    });

    $.plot("#chart", [d], {
      xaxis: {
        mode: "time",
        timeformat: "%Y/%m/%d"
      },
      yaxis: {
        min: 180
      }
    });

    var count = d.length;
    var lastDate = d[count - 1][0];
    var p = $("<p>");
    p.text(`Most recent sample: ${lastDate.toString()}`);
    p.insertAfter("#chart");

  }).fail(function (e) {
    console.dir(e);
    alert("ERROR: " + e.status);
  });
});
