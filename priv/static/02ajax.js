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
      }
   });
}

MFILE.fetchCode = function() {
   console.log("Attempting to fetch code "+MFILE.activeCode["cstr"]);
   $.ajax({
      url: "fetchcode",
      type: "POST",
      dataType: "json",
      data: MFILE.activeCode,
      success: function(result) { 
         console.log("AJAX POST success, result is: '"+result.result+"'");
         if (result.result === "success") { 
            if (result.values.length === 0) {
               $("#result").html("FAILED: 'No codes found'");
            } else if (result.values.length === 1) {
               $("#result").html("SUCCESS: Code found");
               MFILE.activeCode.cstr = result.values[0];
               $("#code").val(MFILE.activeCode.cstr);
            } else {
               $("#mainarea").append("<BR><BR>Your query returned "+result.values.length+" values<BR>");
               $.each(result.values, function (key, value) {
                  $("#mainarea").append(value+" ");
               });
               $("#result").html("Found multiple code(s). Please narrow it down.");
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
      }
   });
}

