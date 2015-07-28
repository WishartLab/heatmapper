$(document).ready(function() {

    /* loading spinner */
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
    
    /* clear file button control */
    var fileControl = $("#file");

    $("#clearFile").on("click", function () {
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        $("#file_progress").hide();
    });
    
    /* file input progress bar control */
    $( "#file" ).change(function() {
      document.getElementById("file_progress").setAttribute('style', "height:20px; margin-top:5px;");
    });    
    
    
    /* advanced options section */
    var advancedOptionsButtonCount = 1;
    document.getElementById("advancedOptionsButton").addEventListener("click", toggleAdvancedOptionsButton);
    
    function toggleAdvancedOptionsButton(e){
        generalToggleButtons("#advancedPanel","advancedOptionsButton", "Advanced", advancedOptionsButtonCount);
        advancedOptionsButtonCount++;
    } 
    
    function generalToggleButtons(panelId, messageId, message, count) {
        if(count%2){
            $("#"+messageId).removeClass("fa fa-angle-down").addClass("fa fa-angle-up");
            document.getElementById(messageId).innerHTML = "Hide " + message + " Options";
        }
        else{
            $("#"+messageId).removeClass("fa fa-angle-up").addClass("fa fa-angle-down");
            document.getElementById(messageId).innerHTML = "Show " + message + " Options";
        }
    }
    
    
    /* readonly */ 
    var tabSelections = document.getElementById("tabSelections"); 
    tabSelections.addEventListener("click", modifyTabs);
    
    function modifyTabs(e) {

        var activeTab = e.target.innerHTML;
        
        if(activeTab == 'Plot'){
        	document.getElementById('ylab').readOnly = false;
        	document.getElementById('xlab').readOnly = false;
        	document.getElementById('title').readOnly = false;
        }
        else{
        	document.getElementById('ylab').readOnly = true;
        	document.getElementById('xlab').readOnly = true;
        	document.getElementById('title').readOnly = true;
        }
    }
    
})	