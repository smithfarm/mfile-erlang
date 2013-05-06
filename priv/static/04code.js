MFILE.displayCode = function () {
   if (typeof MFILE.code === 'string') {
      if (MFILE.code.length === 0) {
         $('#code').val('<EMPTY>');
      } else {
         $('#code').val(MFILE.code);
      }
   } else {
      $('#code').val('<EMPTY>');
   }
}

MFILE.changeCode = function () {
   if (typeof MFILE.code !== 'string' || MFILE.code.length === 0) {
      $('#code').val('');
   }

   // Display/erase help message for Code field
   $("#code").focus(function(event) {
      $("#topmesg").html("ESC=Clear field, Ins=Insert, F3=Look up, Del=Delete");
   });

   $("#code").blur(function(event) {
      $('#topmesg').html('');
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
      mfileProcessEsc();
      return true;
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

// handle Del keypress (DELETE key) in Code field
function handleDelCode(evt) {
    if (evt.keyCode == 46) // Ins
    {
      evt.preventDefault();
      console.log("DELETE KEY PRESSED");
      return mfilecodeProcessDelete();
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

function mfilecodeProcessInsert() {
    console.log("FILE CODE INSERT FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj(
      "", "", "",
      $("#code").val(),
      ""
    );
    currentRec.mfilecodeInsert();
}

function mfilecodeProcessDelete() {
    console.log("FILE CODE DELETE FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj(
      "", "", "",
      $("#code").val(),
      ""
    );
    currentRec.mfilecodeDelete();
}
 
function mfilecodeProcessFetch() {
    console.log("FILE CODE FETCH FUNCTION ACTIVATED");
    var currentRec = new MfilecodeObj(
      "", "", "",
      $("#code").val(),
      ""
    );
    currentRec.mfilecodeFetch();
}

