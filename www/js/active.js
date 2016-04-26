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
    
    /* redirect to home page when navbar-brand is clicked */
    $("#heatmapper-logo").on("click", function(){
            window.location.href = "/";
    });
    
    /* clear file button control */
    var fileControl = $("#file");

    /* clear file input when upload format selection changed */
    $("#uploadFormat").on("change", function () {
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        $("#file_progress").hide();
    });

    $("#clearFile").on("click", function () {
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        $("#file_progress").hide();
    });
    
    $("#clearColClusterFile").on("click", function () {
        var fileControl = $("#colClusterFile");
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        $("#colClusterFile_progress").hide();
    });
    
    $("#clearRowClusterFile").on("click", function () {
        var fileControl = $("#rowClusterFile");
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        $("#rowClusterFile_progress").hide();
    });

    /* clear multiple file upload button control */
    var fileMultiControl = $("#fileMulti");

    $("#clearFileMulti").on("click", function () {
        fileMultiControl.replaceWith( fileMultiControl = fileMultiControl.clone( true ) );
        $("#fileMulti_progress").hide();
    });

    /* file input progress bar control */
    $('#file,#colClusterFile,#rowClusterFile').change(function() {
      $(this).siblings('.progress').get(0).setAttribute('style', "height:20px; margin-top:5px;");
    })

    $( "#fileMulti" ).change(function() {
      document.getElementById("file_progress").setAttribute('style', "height:20px; margin-top:5px;");
    });

    /* example file info close button */
    $( "#closeExampleButton" ).on("click", function(){
        $("#exampleInfo").hide();
    });
    
    /* example file info open button */
    $( "#exampleButton" ).on("click", function(){
        $("#exampleInfo").show();
    });
    
    /* matrix type info close button */
    $( "#closeMatrixInfoButton" ).on("click", function(){
        $("#matrixInfo").hide();
    });
    
    /* matrix type info open button */
    $( "#matrixInfoButton" ).on("click", function(){
        $("#matrixInfo").show();
    });
    
    /* more info close button */
    $( "#closeMoreInfoButton" ).on("click", function(){
        $("#moreInfo").hide();
    });

    /* more info open button */
    $( "#moreButton" ).on("click", function(){
        $("#moreInfo").show();
    });

    /* advanced options section */
    var advancedOptionsButtonCount = 1;
    document.getElementById("advancedOptionsButton").addEventListener("click", toggleAdvancedOptionsButton);
    
    function toggleAdvancedOptionsButton(e){
        generalToggleButtons("advancedOptionsButton", "Advanced Options", advancedOptionsButtonCount);
        advancedOptionsButtonCount++;
    } 
    
    function generalToggleButtons(messageId, message, count) {
        if(count%2){
            $("#"+messageId).removeClass("fa fa-angle-down").addClass("fa fa-angle-up");
            document.getElementById(messageId).innerHTML = "Hide " + message;
        }
        else{
            $("#"+messageId).removeClass("fa fa-angle-up").addClass("fa fa-angle-down");
            document.getElementById(messageId).innerHTML = "Show " + message;
        }
    }

})	
