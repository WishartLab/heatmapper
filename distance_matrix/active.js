$(document).ready(function() {
    
    var labelButtonCount = 0;
    document.getElementById("labelOptions").addEventListener("click", toggleLabelButton);
    
    function toggleLabelButton(e){
        generalToggleButtons("#labelPanel","labelOptions", "Label", labelButtonCount);
        labelButtonCount++;
    }
    
    var colourButtonCount = 0;
    document.getElementById("colourOptions").addEventListener("click", toggleColourButton);
    
    function toggleColourButton(e){
        generalToggleButtons("#colourPanel","colourOptions", "Colour", colourButtonCount);
        colourButtonCount++;
    }
    
    var fileInputButtonCount = 0;
    
    
    
    function generalToggleButtons(panelId, messageId, message, count) {
        if(count%2){
            /* current state is hidden */
            $(panelId).show();
            document.getElementById(messageId).innerHTML = "Hide " + message + " Options";
        }
        else{
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