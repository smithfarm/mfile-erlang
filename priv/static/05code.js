MFILE.displayCode = function () {
   $('#result').empty();
   $('#codebox').html(MFILE.html.code_box);
   if (MFILE.activeCode.cstr.length === 0) {
      //$('#code').val('<EMPTY>');
      $('#code').val('');
   } else {
      $('#code').val(MFILE.activeCode.cstr);
   }
}

MFILE.changeCode = function () {
   MFILE.displayCode();

   // Display/erase help message for Code field
   $("#code").focus(function(event) {
      $("#topmesg").html("ESC=Back, Ins=Insert, F3=Lookup, F5=Delete, ENTER=Accept");
      $("#mainarea").html(MFILE.html.change_code);
   });

   $("#code").blur(function(event) {
      $('#topmesg').empty();
   });

   // Handle function keys in Code field
   $("#code").keydown(function(event) {
     console.log("KEYDOWN. WHICH "+event.which+", KEYCODE "+event.keyCode);
     // ignore keydowns that could cause complications in this context
     if ( (event.which === 9)  ||      // tab, shift-tab
          (event.which === 13)    )    // enter
     {
        event.preventDefault();
        console.log("IGNORING KEYDOWN");
        //$("#code").focus();
        return true;
     }
     handleEsc(event);
     handleInsCode(event);
     handleF3Code(event);
     handleF5Code(event);
   });

   $('#code').focus();
}

// handle ESC keypress
function handleEsc(evt) {
    if (evt.keyCode == 27) // ESC
    {
      evt.preventDefault();
      console.log("ESC PRESSED");
      $('#result').html('');
      MFILE.state = 'MAIN_MENU';
      MFILE.actOnState();
    }
}

// handle Ins keypress (INSERT key) in Code field
function handleInsCode(evt) {
    if (evt.keyCode == 45) // Ins
    {
       evt.preventDefault();
       console.log("INSERT KEY PRESSED");
       MFILE.activeCode.cstr = $('#code').val();
       console.log("Asking server to insert code '"+MFILE.activeCode.cstr+"'");
       MFILE.insertCode();
       $('#result').html(MFILE.activeCode.result);
    }
}

// handle F3 keypress ("Look up Code") in Code field
function handleF3Code(evt) {
    if (evt.keyCode == 114) // F3
    {
       evt.preventDefault();
       console.log("F3 PRESSED");
       MFILE.activeCode.cstr = $('#code').val();
       console.log("Consulting server concerning the code '"+MFILE.activeCode.cstr+"'");
       MFILE.searchCode();
    }
}

// handle F5 keypress ("Delete Code") in Code field
function handleF5Code(evt) {
    if (evt.keyCode == 116) // F5
    {
       evt.preventDefault();
       console.log("DELETE CODE FUNCTIONALITY ACTIVATED");
       MFILE.activeCode.cstr = $('#code').val();
       console.log("Asking server to delete code '"+MFILE.activeCode.cstr+"'");
       MFILE.fetchCode();
       $('#result').html(MFILE.activeCode.result);
    }
}
 
