MFILE.displayCode = function () {
   $('#codebox').html(MFILE.html.code_box);
   if (MFILE.activeCode.cstr.length === 0) {
      //$('#code').val('<EMPTY>');
      $('#code').val('');
   } else {
      $('#code').val(MFILE.activeCode.cstr);
   }
}

MFILE.changeCode = function () {
   $("#mainarea").html(MFILE.html.change_code);
   MFILE.displayCode();

   // Display/erase help message for Code field
   $("#code").focus(function(event) {
      $("#topmesg").html("ESC=Go back, Ins=Insert, F3=Look up, Del=Delete");
   });

   $("#code").blur(function(event) {
      $('#topmesg').html('');
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
     handleDelCode(event);
     handleF3Code(event);
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

// handle Del keypress (DELETE key) in Code field
function handleDelCode(evt) {
    if (evt.keyCode == 46) // Ins
    {
       evt.preventDefault();
       console.log("DELETE KEY PRESSED");
       MFILE.activeCode.cstr = $('#code').val();
       console.log("Asking server to delete code '"+MFILE.activeCode.cstr+"'");
       MFILE.deleteCode();
       $('#result').html(MFILE.activeCode.result);
    }
}
 
// handle F3 keypress ("Fetch Code") in Code field
function handleF3Code(evt) {
    if (evt.keyCode == 114) // F3
    {
       evt.preventDefault();
       console.log("F3 PRESSED");
       MFILE.activeCode.cstr = $('#code').val();
       console.log("Consulting server concerning the code '"+MFILE.activeCode.cstr+"'");
       MFILE.fetchCode();
    }
}

