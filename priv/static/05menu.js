//----------------
//MFILE.mainMenu()
//----------------

"use strict";   // ES5/Strict

// was a number key pressed?
function isNumberKey(evt) {
    var charCode = (evt.which) ? evt.which : evt.keyCode;
    if (charCode > 31 && charCode != 46 && (charCode < 48 || charCode > 57))
    {
       return false;
    }
    return true;
}

MFILE.startOver = function () {
   $('#code').html('');
   $('#result').html('');
   MFILE.uid = undefined;
   MFILE.sessionid = undefined;
   MFILE.cookie.erase('mfileuid');
   MFILE.cookie.erase('mfilesessionid');
   MFILE.state = 'NOT_LOGGED_IN';
}

MFILE.mainMenu = function () {
   $("#mainarea").html(MFILE.html.main_menu);
   $("#getchar").focus();

   $("#getchar").keydown(function(event) {
      logKeyPress(event);
      event.preventDefault();
      if (isNumberKey(event)) {
         switch (event.which) {
            case 49:  // 1
               MFILE.state = 'ADMINISTER_CODES';
               MFILE.actOnState();
               break;
            case 50:  // 2
               MFILE.state = 'TEST_LDAP';
               MFILE.actOnState();
               break;
            case 51:  // 3
               MFILE.state = 'VIEW_COOKIE';
               MFILE.actOnState();
               break;
            case 52:  // 4
               $("#topmesg").html("You pressed 4");
               break;
            case 53:  // 5
               $("#topmesg").html("Logging you out");
               MFILE.startOver();
               MFILE.actOnState();
               break;
         }
      }
   });
}
