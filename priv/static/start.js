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
			theDesc,
			theQueryResult ) {

  this.mfileData = { "mfileId"    : theId,
  		     "mfileDate"  : theDate,
		     "mfileCodeId": theCodeId,
  		     "mfileCode"  : theCode,
		     "mfileSern"  : theSern,
  		     "mfileKeyw"  : theKeyw, 
                     "mfileDesc"  : theDesc,
		     "mfileQueryResult" : theQueryResult
  };

  this.mfileId = theId;
  this.mfileDate = theDate;
  this.mfileCodeId = theCodeId;
  this.mfileCode = theCode;
  this.mfileQueryResult = theQueryResult;

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
        if (result.mfileQueryResult == "success")
        { 
	   console.log("SUCCESS")
           $("#id").empty();
	   $("#id").append("ID No. "+result.mfileId+"  Date: "+result.mfileDate);
	   $("#code").val(result.mfileCode);
	   $("#codeid").val(result.mfileCodeId);
	   $("#sern").val(result.mfileSern);
           $("#keywords").val(result.mfileKeyw);
           $("#description").val(result.mfileDesc);
           $("#mfileresult").empty();
           $("#mfileresult").append("Inserted record ID "+result.mfileId)
	}
	else
	{
	   console.log("FAILURE")
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.mfileQueryResult+"'")
	}
      }
    });
  }

  // Fetch mfile record by its Code + Serial Number (Sern)
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
        if (result.mfileQueryResult == "success")
        { 
	   console.log("SUCCESS")
           $("#id").empty();
	   $("#id").append("ID No. "+result.mfileId+"  Date: "+result.mfileDate);
	   $("#code").val(result.mfileCode);
	   $("#codeid").val(result.mfileCodeId);
	   $("#sern").val(result.mfileSern);
           $("#keywords").val(result.mfileKeyw);
           $("#description").val(result.mfileDesc);
           $("#mfileresult").empty();
           $("#mfileresult").append("Found record no. "+result.mfileId+" last modified on "+result.mfileDate)
	}
	else
	{
	   console.log("FAILURE")
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.mfileQueryResult+"'")
	}
      }
    });
  }
}

// mfilecode object
var MfilecodeObj = function( theId, 
                             theDate,
			     theCode,
			     theDesc ) {

  this.mfilecodeData = { "mfilecodeId"  : theId,
                         "mfilecodeDate": theDate,
                         "mfilecodeCode": theCode,
			 "mfilecodeDesc": theDesc
  };

  this.mfilecodeId = theId;
  this.mfilecodeCode = theCode;

  this.mfilecodeInsert = function() {
    console.log("About to insert the following mfilecode record:");
    console.log(this.mfilecodeData);
    $.ajax({
      url: "insertcode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        if (result.mfilecodeDesc == "success")
        { 
	   console.log("SUCCESS")
	   console.log(result);
  	   $("#code").val(result.mfilecodeCode);
	   $("#codeid").val(result.mfilecodeId);
           $("#mfileresult").empty();
           $("#mfileresult").append("New code '"+result.mfilecodeCode+"' added to database.")
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.mfilecodeDesc+"'")
	}
      }
    });
  }

  this.mfilecodeFetch = function() {
    console.log("Attempting to fetch code "+this.mfilecodeCode);
    $.ajax({
      url: "fetchcode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        if (result.mfilecodeDesc == "success")
	{ 
	   console.log("SUCCESS");
           console.log(result);
	   $("#code").val(result.mfilecodeCode);
	   $("#codeid").val(result.mfilecodeId);
           $("#mfileresult").empty();
           $("#mfileresult").append("Found '"+result.mfilecodeId+"' == '"+result.mfilecodeCode+"'");
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.mfilecodeDesc+"'")
	}
      }
    });
  }
}

// *LIS* When the document finishes loading, add event listeners
$(document).ready(function() {

  // Display/erase help message for Code field
  $("#code").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='ESC=Clear, Ins=Write Code, F3=Validate Code';
  });
  $("#code").blur(function(event) {
    document.getElementById('helpmesg').innerHTML='';
  });

  // Display/erase help message for Serial Number (Sern) field
  $("#sern").focus(function(event) {
    document.getElementById('helpmesg').innerHTML='ESC=Clear, F3=Fetch File';
  });
  $("#sern").blur(function(event) {
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

  // Handle function keys in Code field
  $("#code").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    if (event.which == 9 && event.shiftKey)
    {
       event.preventDefault();
       console.log("SHIFT-TAB PRESSED");
       $("#code").focus();
       return true;
    } 
    handleEsc(event);
    handleInsCode(event);
    handleF3Code(event);
  });

  // Handle function keys in Serial Number (sern) field
  $("#sern").keydown(function(event) {
    console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
    handleEsc(event);
    handleF3Sern(event);
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

  // Handle keypresses in Code field
  $("#code").keypress(function(event) {
    logKeyPress(event);
    if (!isLetterKey(event))
    {
       console.log("ILLEGAL -- NOT A LETTER");
       $("#mfileresult").empty();
       $("#mfileresult").append("* * * LETTERS ONLY, PLEASE * * *");
       return false;
    }
  });

  // Handle keypresses in Serial Number (sern) field
  $("#sern").keypress(function(event) {
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
    if (charCode > 31 && charCode != 46 && (charCode < 48 || charCode > 57))
    {
       return false;
    }
    return true;
}

// was a letter key pressed?
function isLetterKey(evt) {
    var charCode = (evt.which) ? evt.which : evt.keyCode;
    if (charCode < 31 || charCode == 46 || (charCode >= 65 && charCode <= 90) || (charCode >= 97 && charCode <= 122))
    {
       return true;
    }
    return false;
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
      return mfileProcessInsert();
    }
}

// handle F3 keypress ("fetch") in Serial Number field
function handleF3Sern(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      return mfileProcessFetch();
    }
}

// handle F3 keypress ("look up"/"search"), except in Code field
function handleF3(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      return mfileProcessSearch();
    }
}

// handle Ins keypress (INSERT key) in Code field
function handleInsCode(evt) {
    if (evt.keyCode == 45) // Ins
    {
      evt.preventDefault();
      console.log("INSERT KEY PRESSED");
      return mfilecodeProcessInsert();
    }
    return false;
}

// handle F3 keypress ("Fetch Code") in Code field
function handleF3Code(evt) {
    if (evt.keyCode == 114) // F3
    {
      evt.preventDefault();
      console.log("F3 PRESSED");
      mfilecodeProcessFetch();
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
      "",
      document.getElementById("code").value,
      document.getElementById("sern").value,
      document.getElementById("keywords").value,
      document.getElementById("description").value,
      ""
    );
    currentRec.mfileInsert();
}

function mfilecodeProcessInsert() {
    console.log("FILE CODE INSERT FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj( "",
                                       "",
                                       document.getElementById("code").value,
                                       ""
                                     );
    currentRec.mfilecodeInsert();
}

function mfileProcessFetch() {
    console.log("FETCH FUNCTION ACTIVATED");
    var currentRec = new MfileObj(
      "",
      "",
      "",
      document.getElementById("code").value,
      document.getElementById("sern").value,
      "",
      "",
      ""
    );
    currentRec.mfileFetch();
}


function mfilecodeProcessFetch() {
    console.log("FILE CODE FETCH FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj( "",
                                       "",
                                       document.getElementById("code").value,
                                       ""
                                     );
    currentRec.mfilecodeFetch();
}

// Reset the form, losing all data that might be in it
function mfileProcessEsc() {
    $("#id").empty();
    $("#code").val('');
    $("#code").focus();
    $("#sern").val('');
    $("#keywords").val('');
    $("#description").val('');
    $("#mfileresult").empty();
}

