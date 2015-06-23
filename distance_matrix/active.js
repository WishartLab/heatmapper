$(document).ready(function() {
    
    var toggleLabel = document.getElementById("labelOptions");
    toggleLabel.addEventListener("click", toggleButtons);
    var num = 0;
    function toggleButtons(e){
        
        if(num%2){
            /* current state is hidden */
            $("#labelPanel").show();
            document.getElementById("labelOptions").innerHTML = "Hide Label Options";
        }
        else{
            $("#labelPanel").hide();
            document.getElementById("labelOptions").innerHTML = "Show Label Options";
        }
        num++;
        
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