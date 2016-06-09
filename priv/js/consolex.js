var commandHistory = [], snippets = [];
var snippetName = null;
if (commandHistoryStored = Lockr.get('commandHistory')) {
    commandHistory = commandHistoryStored;
}
if (snippetsStored = Lockr.get('snippets')) {
    snippets = snippetsStored;
}

updateCommandHistory(null);
loadSnippets()

function clearCommandHistory() {
    Lockr.rm("commandHistory")
    commandHistory = []
    updateCommandHistory(null);
}

function removeSnippet(name){
  Lockr.set("snippet:" + name)
  snippets.splice(snippets.indexOf(name),1)
  Lockr.set("snippets", snippets)
  loadSnippets()
}

function clearSnippets() {
    for(var i=0; i< snippets.length; i++) {
        removeSnippet(snippets[i])
    }
}

var editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
    lineNumbers: true,
    lineWrapping: true,
    mode: "elixir",
    theme: "text-ex-machina",
    extraKeys: {
        'Ctrl-Enter': executeCode,
        'Cmd-Enter': executeCode,
        'Ctrl-S': saveSnippet,
        'Cmd-S': saveSnippet
    }
});

var consoleLog = CodeMirror.fromTextArea(document.getElementById("console-log"), {
    theme: "text-ex-machina",
    lineWrapping: true,
    readOnly: 'nocursor'
});

$(".task-modal").modal("show");
$("input[name=task]").change(function(){
    task = this.value
    if(task == "other") {
        $("#other-task-input").show()
    } else {
        $("#other-task-input").hide()
    }
})

function launchShell(){
  task_choice = $("input[name=task]:checked").val()
  if(task_choice == "other") {
    task = $("#other-task-input").val()
  } else {
    task = task_choice
  }
  startShell(task);
}

$(".launch-shell").click(launchShell)
$(".launch-options > .ui.form").keypress(function(event){
    var keycode = (event.keyCode ? event.keyCode : event.which);
    if(keycode == '13'){
        $(".task-modal").modal("hide")
        launchShell();
    }
});

ws = null;
if (!window.WebSocket) {
  alert("WebSocket not supported by this browser");
}

function go() {
    ws = new WebSocket("ws://" + location.host + "/websocket");
    ws.onopen = function () {}
    ws.onclose = function () {
      consoleLog.setValue("Launch again\nReason : disconnected from shell, shell terminated")
      setTimeout(function() {go()}, 10000);
    }
    ws.onmessage = function (e) {
        if(!isTerminated) {
            updateConsoleLog(e.data, false)
        }
    }
}

function startShell(task) {
    msg = JSON.stringify({task: task})
    ws.send(msg);
    isTerminated = false
}

function executeCode() {
    var inputType = $("input[name=inputType]:checked").val()
    var msg = {task: "execute", code: editor.getValue(), options: {inputType: inputType}}
    updateConsoleLog(msg.code, true)
    updateCommandHistory(msg.code)
    ws.send(JSON.stringify(msg))
}

function saveSnippet() {
    snippetName = prompt("Snippet name", snippetName || "sample.ex")
    if (snippetName.length > 0) {
      Lockr.set("snippets:" + snippetName, editor.getValue())
      snippets.push(snippetName)
      Lockr.set('snippets', snippets)
      loadSnippets()
    }
}

function loadSnippets(){
    var tmpl = $.templates("#snippet-entry")
    $(".snippets-table").html("")
    for(var i=0; i< snippets.length; i++) {
        name = snippets[i]
        code = Lockr.get("snippets:"+name)
        if (!code) continue;
        var entry = {
            name: name,
            code: prepareCodeToView(code),
            rawCode: code
        }
        var html = tmpl.render(entry)
        $(".snippets-table").append(html)
        new Clipboard('.snippet-code-segment')
    }
    allowOpenInEditor()
    allowCopySegment()
    allowDeleteSnippet()
}

function allowDeleteSnippet(){
  $(".delete-snippet-btn").on("click", function(){
     removeSnippet($(this).data("snippet-name"))
  })
}

function prepareCodeToView(str){
  return str.replace(/(?:\r\n|\r|\n)/g, '<br />')
}

function updateCommandHistory(msg) {
    if(msg) {
        commandHistory.push(msg)
        Lockr.set('commandHistory', commandHistory);
    }

    var tmpl = $.templates("#command-history-entry");
    $(".command-history-table").html("")
    for(var i=0; i<commandHistory.length; i++) {
        command = commandHistory[commandHistory.length-i-1]
        var entry = {
            id: i+1,
            command: prepareCodeToView(command),
            rawCommand: command
        }
        var html = tmpl.render(entry);
        $(".command-history-table").append(html);
        new Clipboard('.history-code-segment');
    }
    if($("input[name=clear-on-send]").is(":checked")) {
        editor.setValue("")
    }
    allowOpenInEditor()
    allowCopySegment()
}

function allowOpenInEditor(){
    $(".open-in-editor-btn").click(function() {
        $('.modal').modal('hide')
        editor.setValue($(this).data("raw-command"))
    })
}

function allowCopySegment(){
    $(".segment").hover(function() {
            $(this).find(".copy-command").show()
        }, function() {
            $(this).find(".copy-command").hide()
        }
    )
}

var isTerminated = false;
$(".terminate-shell-btn").click(function(){
    msg = JSON.stringify({task: "terminate"})
    ws.send(msg);
    isTerminated = true
    ws = null
    go()
})

$(".clear-shell-btn").click(function() {
    consoleLog.setValue("Cleared \n")
})

function updateConsoleLog(data, isInput) {
    var doc = consoleLog.getDoc();
    var cursor = doc.getCursor();
    var initialSize = consoleLog.doc.size
    var line = doc.getLine(cursor.line);
    var pos = {
        line: (doc.size+5),
        ch: line.length - 1
    }
    doc.replaceRange('\n'+data, pos);
    var finalSize = consoleLog.doc.size
    if(isInput) {
        for(var i=initialSize; i<=finalSize; i++) {
            consoleLog.addLineClass(i-1, 'wrap', 'console-log-input')
        }
    }
    consoleLog.scrollTo(0,consoleLog.getScrollInfo().height);
}

$(document).on("ready", function(){
    restoreOptions();
    if($("input[name=restoreEditor]").is(":checked")) {
        editor.setValue(commandHistory[commandHistory.length-1]);
    }
    $("input").on("click", function() {
        saveOptions();
    })
})

function restoreOptions() {
    var default_options = {restoreEditor: "false", inputType: "single"};
    var options = Lockr.get("shell-options");
    options = $.extend(true, {}, default_options, options);
    if(options.restoreEditor=="true") {
        $("input[name='restoreEditor']").prop('checked', "checked");
    }
    $("input[name='inputType'][value="+options.inputType+"]").prop('checked', "checked");
}

function saveOptions() {
    var options = {};
    $("#shell-options").serializeArray().map(function(x){options[x.name] = x.value;});
    Lockr.set("shell-options", options)
}

go();
