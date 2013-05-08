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
   main_menu: "<H2>Main menu</H2>1. Select/change code<br>2. Fly a kite<br>3. Pursue peace<br>4. Rest in the Self<br>5.  Logout<br><br>Your selection: <input id='getchar' size=2 maxlength=1 style='width: 10px;'>&nbsp;</input>",
   code_box: "Code: <textarea id='code' name='code' rows=1 cols=8 maxlength=8 style='height: 22px'></textarea>",
   change_code: "<h2>Select/change code</h2>Cursor now in the 'Code:' field. Use function keys shown above."
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

// cookie functions (by Scott Andrew)
function createCookie(name,value,days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function eraseCookie(name) {
	createCookie(name,"",-1);
}

