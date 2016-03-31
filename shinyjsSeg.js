/*
* This file is for shinyjs code
* 
* Author: Kaihua Liu
*
* segmentation plot history
*/

// seg history status
shinyjs.statSeg = {
  now: 0,
  all: 0
};

shinyjs.updateSeg = function() {
  
  if(shinyjs.statSeg.all > 0){
    // keep the wrong information in history
    // var info = $('#segplot img').length ?
    //   $('#segplot img'):
    //   $('#segplot0').html()
  
    $('#segHistory').append(
      $('<div/>')
      .attr('class', 'segPiece segPiece' + shinyjs.statSeg.all)
      .append($('#segpars table'))
      // .append(info)
      .append($('#segplot0').html())
      
    );
  }

  ++shinyjs.statSeg.all;
  
  $('#segLatest').attr('class', 'segPiece segPiece' + shinyjs.statSeg.all)

  shinyjs.statSeg.now = shinyjs.statSeg.all;

  shinyjs.showSeg(shinyjs.statSeg.now)
}

shinyjs.prevSeg = function() {
  // panel 0 contains nothing
  if(shinyjs.statSeg.now > 1){
    shinyjs.statSeg.now -= 1
  }
  shinyjs.showSeg(shinyjs.statSeg.now)
}
shinyjs.nextSeg = function() {
  if(shinyjs.statSeg.now < shinyjs.statSeg.all) {
    shinyjs.statSeg.now += 1
  }
  shinyjs.showSeg(shinyjs.statSeg.now)
}

shinyjs.showSeg = function(i) {
  $('.segPiece').hide()
  $('.segPiece' + i).show()

  // check status and disable/re-enable two buttons
  if(shinyjs.statSeg.all < 2){
    // less than 2 plots no need to use buttons
    shinyjs.disable('segPrev')
    shinyjs.disable('segNext')
  }
  else{
    if(i == shinyjs.statSeg.all){
      shinyjs.enable('segPrev')
      shinyjs.disable('segNext')
    }
    else if(i == 1){
      shinyjs.disable('segPrev')
      shinyjs.enable('segNext')
    }
    else{
      shinyjs.enable('segPrev')
      shinyjs.enable('segNext')
    }
  }
}



// dependencies: html2canvas.js
shinyjs.saveSeg = function(){
  target = $('#segHistory')
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
      a.setAttribute("download", "segments " + time.toString() + ".png");
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
        shinyBS.addTooltip('segSave', 'popover', {
            'placement': 'top',
            'trigger': 'hover',
            'title': 'Save plots',
            'content': content
        })
    }, 500)
});

