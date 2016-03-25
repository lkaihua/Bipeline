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


// dependencies: download.js, html2canvas.js
shinyjs.saveBi = function(){
  target = $('#biHistory .biPiece:visible')
  if(!target.length){
    return false;
  }
  time = new Date();
  html2canvas(target, {
    onrendered: function(canvas) {
      // document.body.appendChild(canvas);
      // window.open(canvas.toDataURL());
      download(canvas.toDataURL(), "bicluster " + time.toString() + ".jpg", "image/jpeg");
    },
    width: target.width(),
    height: target.height()
  });
}
