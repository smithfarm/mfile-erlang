// Define object to hold our records
var MfileObj = function(theId, theDate, theCode, theSern, theKeyw, theDesc) {

  this.mfileData = { "mfileId"  : theId,
  		     "mfileDate": theDate,
  		     "mfileCode": theCode,
		     "mfileSern": theSern,
  		     "mfileKeyw": theKeyw, 
                     "mfileDesc": theDesc 
  };

  this.mfileId = theId;
  this.mfileDate = theDate;

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
        $("#id").empty();
	$("#id").append("Record ID: "+result.mfileId+" &nbsp;Date: "+result.mfileDate);
	$("#code").val(result.mfileCode);
	$("#sernum").val(result.mfileSern);
        $("#keywords").val(result.mfileKeyw);
        $("#description").val(result.mfileDesc);
        $("#mesg1").empty();
        $("#mesg1").append("New record ID '"+result.mfileId+"' added to database.")
      }
    });
  }

  this.mfileSearch = function() {
    console.log("Attempting to fetch record #"+this.mfileId);
    $.ajax({
      url: "search",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log(result);
        $("#id").empty();
	$("#id").append("ID No. "+result.mfileId+"  Date: "+result.mfileDate);
	$("#code").val(result.mfileCode);
	$("#sernum").val(result.mfileSern);
        $("#keywords").val(result.mfileKeyw);
        $("#description").val(result.mfileDesc);
        $("#mesg1").empty();
        $("#mesg1").append("Found record no. "+result.mfileId+" last modified on "+result.mfileDate)
      }
    });
  }
}

// When the document finishes loading, add event listeners
$(document).ready(function() {

  // Display/erase help message for ID field
  $("#id").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='ESC=Clear, F3=Fetch';
  });
  $("#id").blur(function(event) {
    document.getElementById('helpmesg').innerHTML='';
  });

  // Display/erase help message for Key Words field
  $("#keywords").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='ESC=Clear, Ins=Write, F3=Search';
  });
  $("#keywords").blur(function(event) {
    document.getElementById('helpmesg').innerHTML='';
  });

  // Display/erase help message for Description field
  $("#description").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='ESC=Clear, Ins=Write, F3=Search';
  });
  $("#description").blur(function(event) {
    document.getElementById('helpmesg').innerHTML='';
  });

  // Handle function keys in ID field
  $("#id").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    handleEsc(event);
    handleF3(event);
  });
  // Handle function keys in Keywords field
  $("#keywords").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    handleEsc(event);
    handleIns(event);
    handleF3(event);
  });
  // Handle function keys in Description field
  $("#description").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    handleEsc(event);
    handleIns(event);
    handleF3(event);
    if (event.which == 9 && event.shiftKey)
    {
       event.preventDefault();
       console.log("SHIFT-TAB PRESSED");
       $("#keywords").focus();
       return true;
    } 
    if (event.which == 9)
    {
       event.preventDefault();
       console.log("TAB PRESSED");
       $("#description").focus();
       return true;
    } 
  });

  // Handle keypresses in ID field
  $("#id").keypress(function(event) {
    logKeyPress(event);
    if (!isNumberKey(event))
    {
       console.log("ILLEGAL -- NOT A NUMBER");
       return false;
    }
  });

  // Handle keypresses in Keywords field
  $("#keywords").keypress(function(event) {
    logKeyPress(event);
  });

  // Handle keypresses and TAB in Description field (prevent cursor from exiting form)
  $("#description").keypress(function(event) {
    logKeyPress(event);
  });
});

function handleEsc(evt) {
    if (evt.keyCode == 27) // ESC
    {
      evt.preventDefault();
      console.log("ESC PRESSED");
      mfileProcessEsc();
      return true;
    }
}

function handleIns(evt) {
    if (evt.keyCode == 45) // Ins
    {
      evt.preventDefault();
      console.log("INSERT KEY PRESSED");
      mfileProcessInsert();
      return true;
    }
}

function handleF3(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      mfileProcessSearch();
      return true;
    }
}

function logKeyPress(evt) {
   console.log("WHICH: "+evt.which+", KEYCODE: "+evt.keyCode);
}

function mfileProcessInsert() {
    console.log("INSERT FUNCTION ACTIVATED");
    var currentRec = new MfileObj(
      "",
      "",
      document.getElementById("code").value,
      document.getElementById("sernum").value,
      document.getElementById("keywords").value,
      document.getElementById("description").value
    );
    currentRec.mfileInsert();
}

function mfileProcessSearch() {
    console.log("SEARCH FUNCTION ACTIVATED");
    var currentRec = new MfileObj(
      "",
      "",
      document.getElementById("code").value,
      document.getElementById("sernum").value,
      document.getElementById("keywords").value,
      document.getElementById("description").value
    );
    currentRec.mfileSearch();
  }

// Reset the form, losing all data that might be in it
function mfileProcessEsc() {
    $("#id").empty();
    $("#code").val('');
    $("#code").focus();
    $("#sernum").val('');
    $("#keywords").val('');
    $("#description").val('');
    $("#mesg1").empty();
}

function isNumberKey(evt) {
    var charCode = (evt.which) ? evt.which : evt.keyCode;
    if (charCode != 46 && charCode > 31 && (charCode < 48 || charCode > 57))
    {
       return false;
    }
    return true;
}
