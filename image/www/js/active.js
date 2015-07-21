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
    

    $( "#imageFile" ).change(function() {
      document.getElementById("imageFile_progress").setAttribute('style', "height:20px; margin-top:5px;");
    });
    $( "#gridFile" ).change(function() {
      document.getElementById("gridFile_progress").setAttribute('style', "height:20px; margin-top:5px;");
    });
    
    
    var advancedOptionsButtonCount = 1;
    document.getElementById("advancedOptionsButton").addEventListener("click", toggleAdvancedOptionsButton);
    
    function toggleAdvancedOptionsButton(e){
        generalToggleButtons("#advancedPanel","advancedOptionsButton", "Advanced", advancedOptionsButtonCount);
        advancedOptionsButtonCount++;
    } 
    
    
    function generalToggleButtons(panelId, messageId, message, count) {
        if(count%2){
            $("#"+messageId).removeClass("fa fa-angle-down").addClass("fa fa-angle-up");
            /* current state is hidden */
            /*$(panelId).show(); */
            document.getElementById(messageId).innerHTML = "Hide " + message + " Options";
        }
        else{
            $("#"+messageId).removeClass("fa fa-angle-up").addClass("fa fa-angle-down");
            /*$(panelId).hide();*/
            document.getElementById(messageId).innerHTML = "Show " + message + " Options";
        }
    }
	
})