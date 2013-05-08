"use strict";  // ES5/Strict

// MFILE.determineState()
//    
MFILE.determineState = function () {

   var cookie_in_hand = readCookie('mfileuid');

   console.log("UID appears to be "+cookie_in_hand);
   if (cookie_in_hand !== null) {
      MFILE.state = 'MAIN_MENU';
      return true;
   }
   return false;
}

// ------------------
// MFILE.actOnState()
// ------------------
MFILE.actOnState = function () {

   var retval;

   console.log("Acting on state "+MFILE.state);
   if (MFILE.state === "NOT_LOGGED_IN") {
         $('#userid').html('');
         $('#topmesg').html('');
         $('#mainarea').html("Not logged in");
         retval = MFILE.authenticateUser();
         console.log("MFILE.authenticateUser() returned "+retval);
   } else if (MFILE.state === "LOGIN_FAIL") {
         $('#mainarea').html("Not logged in");
         retval = MFILE.authenticateUser();
         console.log("MFILE.authenticateUser() returned "+retval);
   } else if (MFILE.state === "MAIN_MENU") {
         MFILE.uid = readCookie('mfileuid');
         MFILE.sessionid = readCookie('mfilesessionid');
         $("#userid").html("Username: "+MFILE.uid);
         console.log("Logged in! Session cookie: "+MFILE.sessionid);
         retval = MFILE.mainMenu();
   } else if (MFILE.state === "CHANGE_CODE") {
         console.log("Calling MFILE.changeCode()");
         MFILE.changeCode();
   } else {
      console.log("MFILE INTERNAL ERROR: Inconsistent state!");
   }
}

// ------------
// Main Program
// ------------
$(document).ready(function() {

   MFILE.determineState();
   MFILE.actOnState();

});
