/******************************************************************************/
/************** Setting Up SlideShow ******************************************/
/******************************************************************************/

var allSlides  = $('section'); // $('.slide');

/* progPaneSlide :: (Int) => Int */
function progPaneSlide(paneId){
  var paneId = "#program-pane-" + paneId;
  // var elem   = $(paneId).closest(".slide");
  var elem   = $(paneId).closest("section");
  return allSlides.index(elem);
}
