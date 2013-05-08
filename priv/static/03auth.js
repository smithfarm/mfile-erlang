// 02auth.js
// 20130503 smithfarm
//
// MFILE.authenticateUser() function

"use strict";   // ES5/strict

var processPassword = function () {
   var nam = $("#username").val();
   var pwd = $("#password").val();
   var session = {};

   // consult the server to see if the credentials are valid
   // session = consult_the_server(nam, pwd);  ... NOT IMPLEMENTED YET ...

   // stopgap:
   if (nam === "smithfarm") {
      session.uid = nam;
      session.id = "some_value_returned_by_the_server";
   } 

   if (session.uid) {
      console.log("User authenticated");
      MFILE.uid = session.uid;
      createCookie('mfileuid', session.uid);
      MFILE.sessionid = session.id;
      createCookie('mfilesessionid', session.id);
      MFILE.state = 'MAIN_MENU';
   } else {
      console.log("Authentication attempt failed");
      MFILE.state = 'LOGIN_FAIL';
   }
   MFILE.actOnState();   
}

MFILE.authenticateUser = function () {
   
   // Throw up the authentication dialog
   $('#mainarea').html(MFILE.html.auth_dialog);

   // If previous login attempt failed, let the user know
   if (MFILE.state === 'LOGIN_FAIL') {
      console.log("Previous login failed");
      $('#mainarea').append("<br>Login failed. Please try again.");
   }

   // Clear the input fields and put cursor in "username" field 
   $("#username").val('');
   $("#password").val('');
   $("#username").focus();

   // Set up a callback function to listen for the <ENTER> key to be pressed in "username" field
   $("#username").keydown(function(event) {
      logKeyPress(event);
      if (event.keyCode === 13) {
         event.preventDefault();
         $("#password").focus();
      }
   });

   // Set up a callback function to listen for the <ENTER> key to be pressed in "password" field
   $("#password").keydown(function(event) {
      logKeyPress(event);
      if (event.keyCode === 13) {
         event.preventDefault();
         processPassword();
      }
   });
}

