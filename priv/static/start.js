//
// start.js  (jQuery code for mfile)
//

    // Table of contents:
    //
    // *OBJ object definitions
    // *LIS event listeners
    // *KEY helper functions for processing keys
    // *DBA database interaction functions


// *OBJ mfile object definitions
var MfileObj = function(theId, 
                        theDate, 
			theCodeId,
			theCode, 
			theSern, 
			theKeyw, 
			theDesc ) {

  this.mfileData = { "mfileId"    : theId,
  		     "mfileDate"  : theDate,
		     "mfileCodeId": theCodeId,
  		     "mfileCode"  : theCode,
		     "mfileSern"  : theSern,
  		     "mfileKeyw"  : theKeyw, 
                     "mfileDesc"  : theDesc 
  };

  this.mfileId = theId;
  this.mfileDate = theDate;
  this.mfileCodeId = theCodeId;
  this.mfileCode = theCode;

  // Insert an mfile record into the database
  // - interfaces with "insert" URL handler in main.erl
  this.mfileInsert = function() {
    console.log("Attempting to insert mfile record:");
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
	$("#codeid").val(result.mfileCodeId);
	$("#sernum").val(result.mfileSern);
        $("#keywords").val(result.mfileKeyw);
        $("#description").val(result.mfileDesc);
        $("#mfilestatus").empty();
        $("#mfilestatus").append("New record ID '"+result.mfileId+"' added to database.")
      }
    });
  }

  // Fetch mfile record by its Code + CodeID 
  // - interfaces with "fetch" URL handler in main.erl
  this.mfileFetch = function() {
    console.log("Attempting to fetch file '"+this.mfileCode+"-"+this.mfileSern+"'");
    $.ajax({
      url: "fetch",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log(result);
        $("#id").empty();
	$("#id").append("ID No. "+result.mfileId+"  Date: "+result.mfileDate);
	$("#code").val(result.mfileCode);
	$("#codeid").val(result.mfileCodeId);
	$("#sernum").val(result.mfileSern);
        $("#keywords").val(result.mfileKeyw);
        $("#description").val(result.mfileDesc);
        $("#mfilestatus").empty();
        $("#mfilestatus").append("Found record no. "+result.mfileId+" last modified on "+result.mfileDate)
      }
    });
  }
}

// mfilecode object
var MfilecodeObj = function(theId, theCode) {

  this.mfilecodeData = { "mfilecodeId"  : theId,
                         "mfilecodeCStr": theCode
  };

  this.mfilecodeId = theId;
  this.mfilecodeCStr = theCode;

  this.mfilecodeInsert = function() {
    console.log("About to insert the following mfilecode record:");
    console.log(this.mfilecodeData);
    $.ajax({
      url: "insertcode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        console.log(result);
	$("#code").val(result.mfilecodeCStr);
	$("#codeid").val(result.mfilecodeId);
        $("#mfilestatus").empty();
        $("#mfilestatus").append("New code ID '"+result.mfilecodeId+"' added to database.")
      }
    });
  }

  this.mfilecodeSearch = function() {
    console.log("Attempting to fetch code "+this.mfilecodeCStr);
    $.ajax({
      url: "searchcode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        console.log(result);
	$("#code").val(result.mfilecodeCStr);
	$("#codeid").val(result.mfilecodeId);
        $("#mfilestatus").empty();
        $("#mfilestatus").append("Found '"+result.mfilecodeId+"' == '"+result.mfilecodeCStr+"'");
      }
    });
  }
}

// *LIS* When the document finishes loading, add event listeners
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

  // Handle function keys in Code field
  $("#code").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    handleEsc(event);
    handleInsCode(event);
    handleF3Code(event);
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

}); // END $(document).ready


// *HEL* helper functions for processing keys

// log keypresses
function logKeyPress(evt) {
   console.log("WHICH: "+evt.which+", KEYCODE: "+evt.keyCode);
}

// was a number key pressed?
function isNumberKey(evt) {
    var charCode = (evt.which) ? evt.which : evt.keyCode;
    if (charCode != 46 && charCode > 31 && (charCode < 48 || charCode > 57))
    {
       return false;
    }
    return true;
}

// handle ESC keypress
function handleEsc(evt) {
    if (evt.keyCode == 27) // ESC
    {
      evt.preventDefault();
      console.log("ESC PRESSED");
      mfileProcessEsc();
      return true;
    }
}

// handle Ins keypress (INSERT key), except in Code field
function handleIns(evt) {
    if (evt.keyCode == 45) // Ins
    {
      evt.preventDefault();
      console.log("INSERT KEY PRESSED");
      mfileProcessInsert();
      return true;
    }
}

// handle Ins keypress (INSERT key) in Code field
function handleInsCode(evt) {
    if (evt.keyCode == 45) // Ins
    {
      evt.preventDefault();
      console.log("INSERT KEY PRESSED");
      mfilecodeProcessInsert();
      return true;
    }
}

// handle F3 keypress ("look up"/"search"), except in Code field
function handleF3(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      mfileProcessSearch();
      return true;
    }
}

// handle F3 keypress ("look up"/"search") in Code field
function handleF3Code(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      mfilecodeProcessSearch();
      return true;
    }
}

// *DBA* database interaction functions
// These functions gather form data and send it to the server
// when the user issues a command.

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

function mfilecodeProcessInsert() {
    console.log("CODE INSERT FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj(
      "",
      document.getElementById("code").value
    );
    currentRec.mfilecodeInsert();
}

function mfilecodeProcessSearch() {
    console.log("CODE SEARCH FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj(
      "",
      document.getElementById("code").value
    );
    currentRec.mfilecodeSearch();
}

// Reset the form, losing all data that might be in it
function mfileProcessEsc() {
    $("#id").empty();
    $("#code").val('');
    $("#code").focus();
    $("#sernum").val('');
    $("#keywords").val('');
    $("#description").val('');
    $("#mfilestatus").empty();
}

