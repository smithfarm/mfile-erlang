$(document).ready(function() {

  $("#confirm").focus();

  // Display/erase help message for experimental Confirm screen
  $("#confirm").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='Experimental "confirm" function';
  }); 
  $("#confirm").blur(function(event) {
    document.getElementById('helpmesg').innerHTML='';
  });

  // Handle function keys in Confirm field
  $("#confirm").keydown(function(event) {
    if (event.keyCode == 45) // Ins
    {
      event.preventDefault();
      console.log("INSERT KEY PRESSED IN CONFIRM FIELD");
      $("#savedresult").val("update");
      $("#savedcode").val(document.getElementById("newcode").innerHTML);
      $("#savedsern").val(document.getElementById("newsern").innerHTML);
      $("#savedkeyw").val(document.getElementById("newkeyw").innerHTML);
      $("#saveddesc").val(document.getElementById("newdesc").innerHTML);
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
