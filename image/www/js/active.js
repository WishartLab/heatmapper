$(document).ready(function() {
    
    setInterval(function(){
      if ($('html').attr('class')=='shiny-busy') {
        setTimeout(function() {
          if ($('html').attr('class')=='shiny-busy') {
            $('div.busy').show()
          }
        }, 1000)
      } else {
        $('div.busy').hide()
      }
    }, 100)
    
    var fileInputButtonCount = 0;
    document.getElementById("fileInputOptionsButton").addEventListener("click", toggleFileInputButton);
    
    function toggleFileInputButton(e){
        generalToggleButtons("#fileInputPanel","fileInputOptionsButton", "File Input", fileInputButtonCount);
        fileInputButtonCount++;
    } 
       
    var editButtonCount = 0;
    document.getElementById("editOptionsButton").addEventListener("click", toggleEditButton);
    
    function toggleEditButton(e){
        generalToggleButtons("#editPanel","editOptionsButton", "Editing", editButtonCount);
        editButtonCount++;
    }   
       
    var plotButtonCount = 0;
    document.getElementById("plotOptionsButton").addEventListener("click", togglePlotButton);
    
    function togglePlotButton(e){
        generalToggleButtons("#plotPanel","plotOptionsButton", "Plot", plotButtonCount);
        plotButtonCount++;
    }
    
    var downloadButtonCount = 0;
    document.getElementById("downloadOptionsButton").addEventListener("click", toggleDownloadButton);
    
    function toggleDownloadButton(e){
        generalToggleButtons("#downloadPanel","downloadOptionsButton", "Download", downloadButtonCount);
        downloadButtonCount++;
    }
    
    
    function generalToggleButtons(panelId, messageId, message, count) {
        if(count%2){
            $("#"+messageId).removeClass("fa fa-angle-down").addClass("fa fa-angle-up");
            /* current state is hidden */
            $(panelId).show();
            document.getElementById(messageId).innerHTML = "Hide " + message + " Options";
        }
        else{
            $("#"+messageId).removeClass("fa fa-angle-up").addClass("fa fa-angle-down");
            $(panelId).hide();
            document.getElementById(messageId).innerHTML = "Show " + message + " Options";
        }
    }
	
})