/*
* This file is for shinyjs code
* 
* Author: Kaihua Liu
*
* biclustering plot history
*/

// bicluster history status
shinyjs.statBi = {
  now: 0,
  all: 0
};

shinyjs.updateBi = function() {
  
  if(shinyjs.statBi.all > 0){
  
    $('#biHistory').append(
      $('<div/>')
      .attr('class', 'biPiece biPiece' + shinyjs.statBi.all)
      //.append('<p>all:' + shinyjs.statSeg.all)
      //.append('<p>now:' + shinyjs.statSeg.now)
      .append($('#bipars table'))
      .append($('#biGraph #biDensity img'))
      .append($('#biGraph #biDygraphAll'))
      
    );
  }

  ++shinyjs.statBi.all;
  
  $('#biLatest').attr('class', 'biPiece biPiece' + shinyjs.statBi.all)

  shinyjs.statBi.now = shinyjs.statBi.all;

  shinyjs.showBi(shinyjs.statBi.now)
}

shinyjs.prevBi = function() {
  // panel 0 contains nothing
  if(shinyjs.statBi.now > 1){
    shinyjs.statBi.now -= 1
  }
  shinyjs.showBi(shinyjs.statBi.now)
}
shinyjs.nextBi = function() {
  if(shinyjs.statBi.now < shinyjs.statBi.all) {
    shinyjs.statBi.now += 1
  }
  shinyjs.showBi(shinyjs.statBi.now)
}

shinyjs.showBi = function(i) {
  $('.biPiece').hide()
  $('.biPiece' + i).show()

  // check status and disable/re-enable two buttons
  if(shinyjs.statBi.all < 2){
    // less than 2 plots no need to use buttons
    shinyjs.disable('biPrev')
    shinyjs.disable('biNext')
  }
  else{
    if(i == shinyjs.statBi.all){
      shinyjs.enable('biPrev')
      shinyjs.disable('biNext')
    }
    else if(i == 1){
      shinyjs.disable('biPrev')
      shinyjs.enable('biNext')
    }
    else{
      shinyjs.enable('biPrev')
      shinyjs.enable('biNext')
    }
  }
}


// dependencies: html2canvas.js
shinyjs.saveBi = function(){
  target = $('#biHistory')
  if(!target.length){
    return false;
  }
  
  time = new Date();
  html2canvas(target, {
    onrendered: function(canvas) {
      // document.body.appendChild(canvas);
      // var data = canvas.toDataURL("image/jpeg");
      // var data = canvas.toDataURL("image/png")//.replace("image/png", "image/octet-stream");
      var data = canvas.toDataURL("image/png").replace("image/png", "image/octet-stream");
      // window.location.href = data;
      var img = document.createElement('img');
      img.src = data;

      var a = document.createElement('a');
      a.setAttribute("download", "bicluster " + time.toString() + ".png");
      a.setAttribute("href", data);
      a.style.display = "none";
      a.appendChild(img);

      document.body.appendChild(a);
      a.click();

      // var img = document.createElement('img')
      // img.src = canvas.toDataURL()
      // document.body.appendChild(img);
      // shinyjs.pngBi = img
      // window.open(canvas.toDataURL());
      // download(data, "bicluster " + time.toString() + ".png", "image/png");
    },
    width: target.width(),
    height: target.height()
  });
}

$(document).ready(function () {
    var content = '<p>A screenshot will be saved to your disk folder.</p>'
    if(!!window.devicePixelRatio
      && window.devicePixelRatio > 1
    ){
      // it is Retina screen
      content += "<p>Screenshots of higher quality are avaiable via screenshot shortcut <code>Mac: Command-Shift-4</code> since you are using a high pixel ratio screen.</p>"
    }
    setTimeout(function () {
        shinyBS.addTooltip('biSave', 'popover', {
            'placement': 'top',
            'trigger': 'hover',
            'title': 'Save plots',
            'content': content
        })
    }, 500)
});


