// Define object to hold our records
var MfileObj = function(theId, theKeyw, theDesc) {

  this.mfileData = { "mfileId": theId,
  		     "mfileKeyw": theKeyw, 
                     "mfileDesc": theDesc 
  };

  this.mfileId = theId;

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
        $("#id").val('');
        $("#mesg1").empty();
        $("#keywords").val('');
	$("#keywords").focus();
        $("#description").val('');
        $("#cmd").val('');
        $("#mesg1").append("New record no. "+result.mfileId+" added to database.")
      }
    });
  }

  this.mfileSearch = function() {
    console.log("About to search for record #"+this.mfileId);
    $.ajax({
      url: "search",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log(result);
        $("#id").val(result.mfileId);
        $("#mesg1").empty();
        $("#keywords").val(result.mfileKeyw);
	$("#keywords").focus();
        $("#description").val(result.mfileDesc);
        $("#cmd").val('');
        $("#mesg1").append("Found record no. "+result.mfileId+".")
      }
    });
  }
}

// When the document finishes loading, add event listeners
$(document).ready(function() {
  $("#id").keypress(function(event) {
    if (!isNumberKey(event))
    {
       console.log("ILLEGAL -- NOT A NUMBER");
       return false;
    }
  });
  $("#cmd").keypress(function(event) {
    console.log("WHICH: "+event.which+", KEYCODE: "+event.keyCode);
    if (event.which == 0 && event.keyCode == 9)
    {
       console.log("TAB PRESSED");
       $("#cmd").val('');
       $("#description").focus();
       return true;
    } 
    else 
    {
      if (event.which == 73 || event.which == 105) // 'i' or 'I'
      {
         mfileProcessInsert();
      }
      else
      {
        if (event.which == 83 || event.which == 115) // 's' or 'S'
	{
	  mfileProcessSearch();
	}
      }
    }
  });
  $("#cmd").keyup(function(event) {
    $("#cmd").val('');
  });
});

function mfileProcessInsert() {
        console.log("INSERT KEY PRESSED");
        var currentRec = new MfileObj(
          document.getElementById("id").value,
          document.getElementById("keywords").value,
	  document.getElementById("description").value
        );
        currentRec.mfileInsert();
}

function mfileProcessSearch() {
    console.log("SEARCH KEY PRESSED");
    var currentRec = new MfileObj(
      document.getElementById("id").value,
      document.getElementById("keywords").value,
      document.getElementById("description").value
    );
    currentRec.mfileSearch();
  }

function mfileProcessEsc() {
     // Reset the form, losing all data that might be in it
        $("#id").val('');
        $("#mesg1").empty();
        $("#keywords").val('');
	$("#keywords").focus();
        $("#description").val('');
   }

function isNumberKey(evt)
       {
          var charCode = (evt.which) ? evt.which : evt.keyCode;
          if (charCode != 46 && charCode > 31 
            && (charCode < 48 || charCode > 57))
             return false;

          return true;
       }
