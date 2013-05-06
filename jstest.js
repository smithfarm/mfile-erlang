// Intro
document.writeln("MFILE");
document.writeln("=====");
document.writeln("");
document.writeln("We will now put JavaScript through its paces.");
document.writeln("");

// 1. convert number to string and determine length of a string
//    (also demonstrate use of UTF-8)
document.writeln("1. Convert a number into a string and determine its length");
var num=143343232525;
var numstr=num.toString();
document.writeln("The ASCII string "+numstr+" is "+numstr.length+" characters long.");
numstr="úúuůůůůPoóéěščěšřě+šěřěřěěřěčěšřěščřčěšřžžžčč";
document.writeln("The UTF-8 string "+numstr+" is "+numstr.length+" characters long.");
document.writeln("");

// 2. Trim leading and trailing whitespace from a string
document.writeln("2. Trim leading and trailing whitespace");
var S1="                        la-de-da                 dfdsd                   ";
var S1trimmed=S1.trim();
document.writeln(S1trimmed);


