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
      $.ajax({                                      
        url: 'mainarea',              
        type: "get",          
        beforeSend: function() {
            $('#current_page').append("loading..");
            },
        success: function(s, result) { 
          console.log("Success callback status: "+result);
          $("#mfilemainarea").html(s);
        }
      });
    }
    return false;
  });
});
