$(document).ready(function() {
    
    var fileInputButtonCount = 0;
    document.getElementById("fileInputOptionsButton").addEventListener("click", toggleFileInputButton);
    
    function toggleFileInputButton(e){
        generalToggleButtons("#fileInputPanel","fileInputOptionsButton", "File", fileInputButtonCount);
        fileInputButtonCount++;
    } 
       
    var colourButtonCount = 0;
    document.getElementById("colourOptionsButton").addEventListener("click", toggleColourButton);
    
    function toggleColourButton(e){
        generalToggleButtons("#colourPanel","colourOptionsButton", "Colour", colourButtonCount);
        colourButtonCount++;
    }
    
    var mapButtonCount = 0;
    document.getElementById("mapOptionsButton").addEventListener("click", toggleMapButton);
    
    function toggleMapButton(e){
        generalToggleButtons("#mapPanel","mapOptionsButton", "Map", mapButtonCount);
        mapButtonCount++;
    }
     
    var labelButtonCount = 0;
    document.getElementById("labelOptionsButton").addEventListener("click", toggleLabelButton);
    
    function toggleLabelButton(e){
        generalToggleButtons("#labelPanel","labelOptionsButton", "Label", labelButtonCount);
        labelButtonCount++;
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