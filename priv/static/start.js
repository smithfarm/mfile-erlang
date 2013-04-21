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
var MfileObj = function(
			theQueryResult,
                        theId, 
                        theDate, 
			theCodeId,
			theCode, 
			theSern, 
			theKeyw, 
			theDesc
			) {

  this.mfileData = { 
		       "queryResult" : theQueryResult,
                       "mfileId"    : theId,
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
  this.mfileSern = theSern;
  this.queryResult = theQueryResult;

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
        if (result.queryResult == "success")
        { 
	   console.log("SUCCESS")
           $("#id").empty();
	   $("#id").append("ID No. "+result.mfileId+" &nbsp;Date: "+result.mfileDate);
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
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
	}
      }
    });
  }

  // Update an mfile record
  // - interfaces with "update" URL handler in main.erl
  this.mfileUpdate = function() {
    console.log("Attempting to update mfile record:");
    console.log(this.mfileData);
    $.ajax({
      url: "update",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log(result);
        if (result.queryResult == "success")
        { 
	   console.log("SUCCESS")
           $("#id").empty();
	   $("#id").append("ID No. "+result.mfileId+" &nbsp;Date: "+result.mfileDate);
	   $("#code").val(result.mfileCode);
	   $("#codeid").val(result.mfileCodeId);
	   $("#sern").val(result.mfileSern);
           $("#keywords").val(result.mfileKeyw);
           $("#description").val(result.mfileDesc);
           $("#mfileresult").empty();
           $("#mfileresult").append("Updated record ID "+result.mfileId)
	}
	else
	{
	   console.log("FAILURE")
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
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
        if (result.queryResult == "success")
        { 
	   console.log("SUCCESS")
           $("#id").empty();
	   $("#id").append("ID No. "+result.mfileId+" &nbsp;Date: "+result.mfileDate);
	   $("#code").val(result.mfileCode);
	   $("#codeid").val(result.mfileCodeId);
	   $("#sern").val(result.mfileSern);
           $("#keywords").val(result.mfileKeyw);
           $("#description").val(result.mfileDesc);
           $("#mfileresult").empty();
           $("#mfileresult").append("Found record "+result.mfileCode+"-"+result.mfileSern)
	}
	else
	{
	   console.log("FAILURE")
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
	}
      }
    });
  }

  this.mfileDelete = function() {
    console.log("Attempting to delete file "+this.mfileCode+"-"+this.mfileSern);
    $.ajax({
      url: "delete",
      type: "POST",
      dataType: "json",
      data: this.mfileData,
      success: function(result) { 
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult == "success")
	{ 
	   console.log("SUCCESS");
           console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
	   $("#sern").empty();
	   $("#mfileresult").empty();
           $("#mfileresult").append("File deleted");
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
	}
      }
    });
  }
}

var ConfirmObj = function(
			theCommand,
			theNewCode, 
			theNewSern, 
			theNewKeyw, 
			theNewDesc
			) {

  this.confirmData = { 
		       "command" : theCommand,
                       "newcode" : theNewCode,
                       "newsern" : theNewSern,
                       "newkeyw" : theNewKeyw,
		       "newdesc" : theNewDesc
  };

  this.opconfirm = function() {
    console.log("Running experimental confirm AJAX call");
    console.log("The URL is confirm"+theCommand);
    $.ajax({
      url: "confirm"+theCommand,
      type: "POST",
      data: this.confirmData,
      dataType: "html",
      success: function(s, result) { 
        console.log("Confirmation dialog is now on-screen");
        $("#mfilemainarea").html(s);
        $('#confirm').focus();
      }
    });
  }
}

var MainareaObj = function(
			theCode, 
			theSern, 
			theKeyw, 
			theDesc
			) {

  this.confirmData = { 
                       "filecode" : theCode,
                       "filesern" : theSern,
                       "filekeyw" : theKeyw,
		       "filedesc" : theDesc
  };

  this.dispmain = function() {
    $.ajax({
      url: "mainarea",
      type: "POST",
      data: this.confirmData,
      dataType: "html",
      success: function(s, result) { 
        $("#mfilemainarea").html(s);
        $('#confirm').focus();
      }
    });
  }
}

// mfilecode object
var MfilecodeObj = function( 
			     theQueryResult,
                             theId, 
                             theDate,
			     theCode,
			     theDesc
			     ) {

  this.mfilecodeData = { "queryResult" : theQueryResult,
			 "mfilecodeId"  : theId,
                         "mfilecodeDate": theDate,
                         "mfilecodeCode": theCode,
			 "mfilecodeDesc": theDesc 
  };

  this.mfilecodeId = theId;
  this.mfilecodeCode = theCode;
  this.queryResult = theQueryResult;

  this.mfilecodeInsert = function() {
    console.log("About to insert the following mfilecode record:");
    console.log(this.mfilecodeData);
    $.ajax({
      url: "insertcode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult == "success")
        { 
	   console.log("SUCCESS")
	   console.log(result);
  	   $("#code").val(result.mfilecodeCode);
	   $("#codeid").val(result.mfilecodeId);
           $("#mfileresult").empty();
           $("#mfileresult").append("New code "+result.mfilecodeCode+" (ID "+result.mfilecodeId+") added to database.")
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
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
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult == "success")
	{ 
	   console.log("SUCCESS");
           console.log(result);
	   $("#code").val(result.mfilecodeCode);
	   $("#codeid").val(result.mfilecodeId);
           $("#mfileresult").empty();
           $("#mfileresult").append("Found code '"+result.mfilecodeCode+"'");
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
	}
      }
    });
  }

  this.mfilecodeDelete = function() {
    console.log("Attempting to delete code "+this.mfilecodeCode);
    $.ajax({
      url: "deletecode",
      type: "POST",
      dataType: "json",
      data: this.mfilecodeData,
      success: function(result) { 
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult == "success")
	{ 
	   console.log("SUCCESS");
           console.log(result);
	   $("#code").empty();
	   $("#codeid").empty();
	   $("#mfileresult").empty();
           $("#mfileresult").append("Code deleted");
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
           $("#mfileresult").empty();
           $("#mfileresult").append("FAILED: '"+result.queryResult+"'")
	}
      }
    });
  }

}
 
$(document).ready(function (){
  if ($("#savedresult").val() == "lost")
  {
    console.log("LOST!!!");
  }
  else
  {
    console.log("Normal operation.");
    var currentRec = new MainareaObj(
      $("#savedcode").val(),
      $("#savedsern").val(),
      $("#savedkeyw").val(),
      $("#saveddesc").val()
    );  
    currentRec.dispmain();
  }
});
