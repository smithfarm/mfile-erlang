
// Define object to hold our records
var MfileObj = function(theKeyw, theDesc) {

  this.mfileData = { "mfileKeyw": theKeyw, 
                     "mfileDesc": theDesc 
  };

  this.mfileInsert = function() {
    console.log("About to insert the following record:");
    console.log(this.mfileData);
    $.ajax({
      url: "insert",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log(result);
        $("#mesg1").empty();
        $("#keywords").val('');
	$("#keywords").focus();
        $("#description").val('');
        $("#mesg1").append("New record no. "+result.mfileID+" added to database.")
      }
    });
  }
}

// When the document finishes loading, add event listeners
$(document).ready(function() {
  $("#insertform").keypress(function(event) {
    if (event.keyCode == 124)   // '|'
    {
        console.log("IGNORING ILLEGAL KEY");
        return false;
    }
    if (event.keyCode == 27)    // ESC
    {
        console.log("ESCAPE KEY PRESSED");
        mfileProcessEsc();
    }
  });
  $("#insertform").keydown(function(event) {
    if (event.keyCode == 45)
    {
        console.log("INSERT KEY PRESSED");
        var currentRec = new MfileObj(
          document.getElementById("keywords").value,
	  document.getElementById("description").value
        );
        currentRec.mfileInsert();
    }
    else
    {
        console.log("KEYCODE == "+event.keyCode);
    }
  });
});

function mfileProcessEsc() {
     //
   }
