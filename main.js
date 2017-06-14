/*
    A small JavaScript program that gets sample text in separate files
    as a learning material for the Gibberish Detector.
    Needs nodejs and npm installed, as well as the unirest library
*/
var unirest = require('unirest');
var fs = require('fs');

for (var i = 0; i < 100; ++i) {
    unirest.post("https://andruxnet-random-famous-quotes.p.mashape.com/?cat=movies")
    .header("X-Mashape-Key", "x2UFv4dJEymshn3n6DjsjuNhzFEap1PiTmAjsnHP5CUwURP6K2")
    .header("Content-Type", "application/x-www-form-urlencoded")
    .header("Accept", "application/json")
    .end(function (result) {
        var obj = JSON.parse(result.body);
        console.log(obj.quote);
        var str = obj.quote.toString();
        str = str.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "");
        fs.writeFileSync("../quotes/" + Math.random(), str, function(err){})
    });
}
