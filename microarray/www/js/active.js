$(document).ready(function() {
    
    var fileInputButtonCount = 0;
    document.getElementById("fileInputOptionsButton").addEventListener("click", toggleFileInputButton);
    
    function toggleFileInputButton(e){
        generalToggleButtons("#fileInputPanel","fileInputOptionsButton", "File Input", fileInputButtonCount);
        fileInputButtonCount++;
    } 
       
    var clusterButtonCount = 0;
    document.getElementById("clusterOptionsButton").addEventListener("click", toggleClusterButton);
    
    function toggleClusterButton(e){
        generalToggleButtons("#clusterPanel","clusterOptionsButton", "Cluster", clusterButtonCount);
        clusterButtonCount++;
    }   
       
    var colourButtonCount = 0;
    document.getElementById("colourOptionsButton").addEventListener("click", toggleColourButton);
    
    function toggleColourButton(e){
        generalToggleButtons("#colourPanel","colourOptionsButton", "Colour", colourButtonCount);
        colourButtonCount++;
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
    
    var colourDropdown = document.getElementById("colour");
    colourDropdown.addEventListener("change", modifyColourSection);
    
    function modifyColourSection(e){
        var selection = e.target.value;
        if(selection == 'custom'){
            document.getElementById("colourSection").setAttribute('style', "");
        }
        else{
            var style = "filter:alpha(opacity=50); opacity: 0.5; -moz-opacity:0.50; z-index: 20;"
            document.getElementById("colourSection").setAttribute('style', style);
        }
    }
    
    var tabSelections = document.getElementById("tabSelections"); 
    tabSelections.addEventListener("click", modifyTabs);
    
    function modifyTabs(e) {
        
        var activeTab = e.target.innerHTML;
        
        if(activeTab == 'Plot'){
        	document.getElementById('ylab').readOnly = false;
        	document.getElementById('xlab').readOnly = false;
        	document.getElementById('title').readOnly = false;
            document.getElementById('colour').disabled = false;
            if(document.getElementById("colour").value == "custom"){
                document.getElementById("colourSection").setAttribute('style', "");
            }  
        }
        else{
        	document.getElementById('ylab').readOnly = true;
        	document.getElementById('xlab').readOnly = true;
        	document.getElementById('title').readOnly = true;
            
            if(activeTab == 'Table'){
                document.getElementById('colour').disabled = true;
                document.getElementById('customVars').disabled = true;
                var style = "filter:alpha(opacity=50); opacity: 0.5; -moz-opacity:0.50; z-index: 20;"
                document.getElementById("colourSection").setAttribute('style', style);
            }
            else{
                document.getElementById('colour').disabled = false;
                if(document.getElementById("colour").value == "custom"){
                    document.getElementById("colourSection").setAttribute('style', "");
                }  
            }
        }
    }
    
})	