$(document).ready(function() {
    
    var tabSelections = document.getElementById("tabSelections"); 
    tabSelections.addEventListener("click", modifyTabs);
    
    function modifyTabs(e) {
        
        var activeTab = e.target.innerHTML;
        
        if(activeTab == 'Plot'){
        	document.getElementById('ylab').readOnly = false;
        	document.getElementById('xlab').readOnly = false;
        	document.getElementById('title').readOnly = false;
            document.getElementById('colour').disabled = false;
            document.getElementById("colourSection").setAttribute('style', "");
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
                document.getElementById("colourSection").setAttribute('style', "");
            }
        }
    }
    
})	