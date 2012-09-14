function stackTrace() {
  var callstack = [];
  var isCallstackPopulated = false;
  try {
    i.dont.exist += 0; // Doesn't exist - that's the point.
  } catch(e) {
    if (e.stack) { // Firefox && Chrome
      var lines = e.stack.split("\n");
      for (var i = 0, sz = lines.length; i < sz; i++) {
        // Only append line having line numbers (e.q. ':56')
        if (lines[i].match(/\:\d+/))
          // Remove "file:///" prefix (Chrome).
          callstack.push(lines[i].replace("file:///", ""));
      }
      // Remove call to printStackTrace()
      callstack.shift();
      // If there is no lines (maybe line filter above is wrong), just
      // copy the stack as is.
      if (callstack.length == 0) callstack = lines;
      isCallstackPopulated = true;
    }
    else if (window.opera && e.message) { // Opera
      var lines = e.message.split("\n");
      for (var i = 0, sz = lines.length; i < sz; i++) {
        if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
          var entry = lines[i];
          // Append next line also since it has the file info.
          if (lines[i + 1]) {
            entry += " at " + lines[i+1];
            i++;
          }
          callstack.push(entry);
        }
      }
      // Remove call to printStackTrace().
      callstack.shift();
      isCallstackPopulated = true;
    }
  }
  if (!isCallstackPopulated) { // IE and Safari
    var currentFunction = arguments.callee.caller;
    while (currentFunction) {
      var fn = currentFunction.toString();
      var fname = fn.substring(fn.indexOf("function") + 8, fn.indexOf("(")) 
                  || "anonymous";
      callstack.push(fname);
      currentFunction = currentFunction.caller;
    }
  }
  return callstack.join("\n");
}
