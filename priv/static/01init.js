// 01init.js
//
// MFILE JavaScript init script
// 20130503 smithfarm
//
// Defines objects and helper functions

"use strict";  // ES5/strict

// ------------------
// Object Definitions
// ------------------
var MFILE = Object.create(null);

MFILE.state = "NOT_LOGGED_IN";

MFILE.html = {
   auth_dialog: "<br><br><br>Username: <input id='username' size='15' maxlength='9' /><br>Password: <input id='password' type='password' size='20' maxlength='20' /><br>&nbsp;",
   main_menu: "<H2>Main menu</H2>1. Administer codes<br>2. Test LDAP<br>3. View cookies<br>4. Rest in the Self<br>5.  Logout<br><br>Your selection: <input id='getchar' size=2 maxlength=1 style='width: 10px;'>&nbsp;</input>",
   code_box: "Code: <textarea id='code' name='code' rows=1 cols=8 maxlength=8 style='height: 22px'></textarea>",
   change_code: "<h2>Administer codes</h2>Cursor now in the 'Code:' field. Use function keys shown above.",
   code_search_results1: "<h2>Multiple matches</h2>The following codes match your search term:<br>",
   press_any_key: "<br><br>Press any key to continue: <input id='continue' size='1' maxlength='2' />",
   code_delete_conf1: "<h2>Confirmation needed</h2>You are asking to delete the code: ",
   code_delete_conf2: "<br>Press 'y' to delete, any other key to cancel: <input id='yesno' size='1' maxlength='2' />",
   test_ldap1: "<h2>Test LDAP</h2>",
   view_cookie1: "<h2>View cookies</h2>",
}

MFILE.activeCode = {
  result: "",
  id: "",
  dstr: "",
  cstr: "",
  desc: ""
};

// ----------------
// Helper Functions
// ---------------- 
// function to log keypresses
function logKeyPress(evt) {
   console.log("WHICH: "+evt.which+", KEYCODE: "+evt.keyCode);
}

