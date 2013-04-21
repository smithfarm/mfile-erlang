$(document).ready(function() {

  $("#confirm").focus();

  // Display/erase help message for experimental Confirm screen
  $("#confirm").focus(function(event) {
    $("#helpmesg").html('Experimental "confirm" function');
  }); 
  $("#confirm").blur(function(event) {
    $("#helpmesg").html('');
  });

  // Handle function keys in Confirm field
  $("#confirm").keydown(function(event) {
    if (event.keyCode == 45) // Ins
    {
      event.preventDefault();
      console.log("INSERT KEY PRESSED IN CONFIRM FIELD");
      $("#savedcode").val($("#newcode").html());
      $("#savedsern").val($("#newsern").html());
      $("#savedkeyw").val($("#newkeyw").html());
      $("#saveddesc").val($("#newdesc").html());
      $.ajax({                                      
        url: 'mainarea',              
        type: "post",          
        success: function(s, result) { 
          console.log("Returned from confirm with result "+$("#savedresult").val());
          $("#mfilemainarea").html(s);
        }
      });
      return false;
    }
    event.preventDefault();
    console.log("KEY OTHER THAN INSERT PRESSED IN CONFIRM FIELD");
    $("#savedresult").val("");
    $.ajax({                                      
      url: 'mainarea',              
      type: "post",          
      success: function(s, result) { 
        $("#mfilemainarea").html(s);
      }
    });
  });
});
