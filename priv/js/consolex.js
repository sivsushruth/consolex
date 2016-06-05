var commandHistory = [];
if (commandHistoryStored = Lockr.get('commandHistory')) {
    commandHistory = commandHistoryStored;
}

updateCommandHistory(null);

function clearCommandHistory() {
    Lockr.rm("commandHistory")
    commandHistory = []
    updateCommandHistory(null);
}

var editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
    lineNumbers: true,
    lineWrapping: true,
    mode: "elixir",
    theme: "text-ex-machina",
    extraKeys: {
        'Ctrl-Enter': executeCode,
        'Cmd-Enter': executeCode
    }
});

var consoleLog = CodeMirror.fromTextArea(document.getElementById("console-log"), {
    theme: "text-ex-machina",
    lineWrapping: true,
    readOnly: true
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

function updateCommandHistory(msg) {
    if(msg) {
        commandHistory.push(msg)
        Lockr.set('commandHistory', commandHistory);
    }

    var tmpl = $.templates("#command-history-entry");
    $(".command-history-table").html("")
    for(var i=0; i<commandHistory.length; i++) {
        var entry = {
            id: i+1,
            command: commandHistory[commandHistory.length-i-1].replace(/(?:\r\n|\r|\n)/g, '<br />'),
            rawCommand: commandHistory[commandHistory.length-i-1]
        }
        var html = tmpl.render(entry);
        $(".command-history-table").append(html);
        new Clipboard('.history-code-segment');
    }
    if($("input[name=clear-on-send]").is(":checked")) {
        editor.setValue("")
    }
    $(".history-code-segment").hover(function() {
            $(this).find(".copy-command").show()
        }, function() {
            $(this).find(".copy-command").hide()
        }
    )
    $(".open-in-editor-btn").click(function() {
        $('.command-history-modal').modal('hide')
        editor.setValue($(this).data("raw-command"))
    })
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
