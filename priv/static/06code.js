//-------------------------------
// ADMINISTER CODES FUNCTIONALITY
//-------------------------------


MFILE.administerCodes = function () {
   MFILE.administerCodes.handleDisplay();
   MFILE.administerCodes.handleKeyboard();
}


MFILE.administerCodes.handleDisplay = function () {

   $('#result').empty();

   $('#codebox').html(MFILE.html.code_box);
   if (MFILE.activeCode.cstr.length === 0) {
      //$('#code').val('<EMPTY>');
      $('#code').val('');
   } else {
      $('#code').val(MFILE.activeCode.cstr);
   }

   $("#code").focus(function(event) {
      $("#topmesg").html("ESC=Back, Ins=Insert, F3=Lookup, F5=Delete, ENTER=Accept");
      $("#mainarea").html(MFILE.html.change_code);
   });

   $("#code").blur(function(event) {
      $('#topmesg').empty();
   });

}


MFILE.administerCodes.handleKeyboard = function () {

   // Handle function keys in Code field
   $("#code").keydown(function (event) {
      MFILE.administerCodes.processKey(event);
   }); 

   $('#code').focus();
}


MFILE.administerCodes.processKey = function (event) {

   console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);

   // Function key handler
   if (event.which === 9)          { // tab, shift-tab
      event.preventDefault();
      console.log("IGNORING TAB");
      return true;
   } else if (event.which === 27)  { // ESC
      event.preventDefault();
      MFILE.activeCode.cstr = "";
      $('#code').val('');
      $('#code').blur();
      $('#codebox').empty();
      $('#result').empty();
      MFILE.state = 'MAIN_MENU';
      MFILE.actOnState();
   } else if (event.which === 13)  { // ENTER
      event.preventDefault();
      $('#result').empty();
      MFILE.fetchCode("ACCEPT");
   } else if (event.which === 45)  { // Ins
      event.preventDefault();
      MFILE.activeCode.cstr = $('#code').val();
      console.log("Asking server to insert code '"+MFILE.activeCode.cstr+"'");
      MFILE.insertCode();
      $('#result').html(MFILE.activeCode.result);
   } else if (event.which === 114) { // F3
      event.preventDefault();
      MFILE.activeCode.cstr = $('#code').val();
      console.log("Consulting server concerning the code '"+MFILE.activeCode.cstr+"'");
      MFILE.searchCode();
   } else if (event.which == 116)  { // F5
      event.preventDefault();
      MFILE.activeCode.cstr = $('#code').val();
      console.log("Asking server to delete code '"+MFILE.activeCode.cstr+"'");
      MFILE.fetchCode('DELETE');
      $('#result').html(MFILE.activeCode.result);
   }
}

//---------------
// AJAX FUNCTIONS
//---------------
MFILE.insertCode = function() {
   console.log("About to insert code string "+MFILE.activeCode["cstr"]);
   $.ajax({
      url: "insertcode",
      type: "POST",
      dataType: "json",
      data: MFILE.activeCode,
      success: function(result) { 
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult === "success")
        { 
	   console.log("SUCCESS")
	   console.log(result);
  	   $("#code").val(result.mfilecodeCode);
           $("#result").empty();
           $("#result").append("New code "+result.mfilecodeCode+" (ID "+result.mfilecodeId+") added to database.")
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
	   $("#code").empty();
           $("#result").empty();
           $("#result").append("FAILED: '"+result.queryResult+"'")
	}
      },
      error: function(xhr, status, error) {
         $("#result").html("AJAX ERROR: "+xhr.status);
      }
   });
}

MFILE.fetchCode = function (action) {   // we fetch it in order to delete it
   console.log("Attempting to fetch code "+MFILE.activeCode["cstr"]);
   $.ajax({
      url: "fetchcode",
      type: "POST",
      dataType: "json",
      data: MFILE.activeCode,
      success: function(result) { 
         console.log("AJAX POST success, result is: '"+result.queryResult+"'");
         if (result.queryResult === "success") {
            MFILE.activeCode.cstr = result.mfilecodeCode;
  	    $("#code").val(MFILE.activeCode.cstr);
            if (action === "DELETE") {
               MFILE.deleteCodeConf();
            } else {
               MFILE.state = 'MAIN_MENU';
               MFILE.actOnState();
            }
         } else {
            $('#result').html("FAILED: '"+result.queryResult+"'");
            return false;
         }
      },
      error: function(xhr, status, error) {
         $("#result").html("AJAX ERROR: "+xhr.status);
      }
   });
}

MFILE.deleteCodeConf = function () {   // for now, called only from fetchCode
   console.log("Asking for confirmation to delete "+MFILE.activeCode["cstr"]);
   $("#mainarea").html(MFILE.html.code_delete_conf1);
   $("#mainarea").append(MFILE.activeCode.cstr+"<BR>");
   $("#mainarea").append(MFILE.html.code_delete_conf2);
   $("#yesno").focus();
   console.log("Attempting to fetch code "+MFILE.activeCode["cstr"]);
   $("#yesno").keydown(function(event) {
       event.preventDefault();
       logKeyPress(event);
       if (event.which === 89) {
          MFILE.deleteCode();
       }
       MFILE.actOnState();
   });
}
       
MFILE.searchCode = function () {
   console.log("Attempting to search code "+MFILE.activeCode["cstr"]);
   $.ajax({
      url: "searchcode",
      type: "POST",
      dataType: "json",
      data: MFILE.activeCode,
      success: function(result) { 
         console.log("AJAX POST success, result is: '"+result.result+"'");
         if (result.result === "success") { 
            if (result.values.length === 0) {
               $("#result").html("FAILED: 'Nothing matches'");
            } else if (result.values.length === 1) {
               $("#result").html("SUCCESS: Code found");
               MFILE.activeCode.cstr = result.values[0];
               $("#code").val(MFILE.activeCode.cstr);
            } else {
               $("#mainarea").html(MFILE.html.code_search_results1);
               $.each(result.values, function (key, value) {
                  $("#mainarea").append(value+" ");
               });
               $("#mainarea").append(MFILE.html.press_any_key);
               $("#continue").focus();
               $("#continue").keydown(function(event) {
                  event.preventDefault();
                  MFILE.actOnState();
               });
               $("#result").html("Search found multiple matching codes. Please narrow it down.");
            }
         } else {
            console.log("FAILURE: "+result);
            $("#code").empty();
            $("#result").html("FAILED: '"+result.result+"'");
         } 
      },
      error: function(xhr, status, error) {
         $("#result").html("AJAX ERROR: "+xhr.status);
      }
   });
}

MFILE.deleteCode = function() {
   console.log("Attempting to delete code "+MFILE.activeCode["cstr"]);
   $.ajax({
      url: "deletecode",
      type: "POST",
      dataType: "json",
      data: MFILE.activeCode,
      success: function(result) { 
        console.log("Query result is: '"+result.queryResult+"'");
	$("#id").empty();
        if (result.queryResult === "success")
	{ 
	   console.log("SUCCESS");
           console.log(result);
	   $("#code").empty();
	   $("#result").empty();
           $("#result").append("Code deleted");
	}
	else
	{
	   console.log("FAILURE")
	   console.log(result);
           $("#result").empty();
           $("#result").append("FAILED: '"+result.queryResult+"'")
	}
      },
      error: function(xhr, status, error) {
         $("#result").html("AJAX ERROR: "+xhr.status);
      }
   });
}

