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

